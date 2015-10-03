#|
    Copyright (C) 2015 Mike Harris

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#




;;--------------------- food stores

(defun eat (place)
  (dolist (group (sentients-list place))
    (let ((population (quantity group)))
      (dotimes (i +total-nutrients+)
	(decf (aref (food-stores place) i)
	      (* population (aref (nutrition-needs (get-population-type group)) i)))))))
	       
(defun check-farms (current-sector)
  (dolist (farm (farms-list current-sector))
    (if (datep (current-date) (dormancy-start-date (get-plant (species-index farm))))
	(if (tended farm)
	    (harvest farm current-sector)
	    (setf (tended farm) T)))))

(defun harvest (farm current-sector)
  (with-slots (species-index quantity) farm
    (dolist (product (get-products species-index))
      (when (typep product 'food)
	(with-slots (yield nutrients) product
	  (dotimes (i +total-nutrients+)
	    (incf (aref (food-stores current-sector) i)
		  (* quantity yield (env-suitability farm) (aref nutrients i)))))))))
;;	  (message (concatenate 'string "Harvested " yield "pounds of " (name product)))
;;	  (print (food-stores current-sector)))))))

(defun check-malnutrition (place)
  (setf (afflictions-list place) nil)
  (dolist (group (sentients-list place))
    (let ((population (quantity group)))
      (dotimes (i (- +total-nutrients+ 2))
	(print (aref (nutrition-needs (get-population-type group)) i))
	(print (< 0 (aref (nutrition-needs (get-population-type group)) i)))
	(when (< 0 (aref (nutrition-needs (get-population-type group)) i))
	  (let* ((aff (aref (affliction-array *the-world*) i))
		 (need (* population (aref (nutrition-needs (get-population-type group)) i)))
		 (supply (aref (food-stores place) i))
		 (severity (* -1 (floor (/ supply need) (creature-days-per-severity aff))))
		 (msg-index (- severity (fatality-threshold aff) 3)))
	    (if (> msg-index 3)
		(progn
		  (setf msg-index 3)
		  (if (< (random 100) (fatality-chance aff))
		      (let ((d (round (* (/ (fatality-amount aff) 100) population))))
			(if (> d 0)
			    (deaths (random d) group place))))))
	    (if (>= msg-index 0)
		(push (list i msg-index) (afflictions-list place))))))
      (print (afflictions-list place)))))

;;---------------------- labor

(defmethod needed-workers ((employer farmed-plant-population))
  (with-slots (growth-start-date fruiting-start-date dormancy-start-date harvest-difficulty)
      (aref (plant-instance-array *the-world*) (species-index employer))
    (let ((workers (ceiling (quantity employer) 20)))
      (cond ((date-less (current-date) growth-start-date) 0)
	    ((date-less (current-date) fruiting-start-date)
	     workers)
	    ((date-less (current-date) dormancy-start-date)
	     (* workers harvest-difficulty))
	    (t 0)))))

(defun assign-workers (current-sector)
  (with-slots (idle-workers monthly-workers) current-sector
    (incf idle-workers monthly-workers)
    (setf monthly-workers 0)
    (dolist (farm (farms-list current-sector))
      (let ((workers (needed-workers farm)))
	(if (tended farm)
	  (if (> idle-workers workers)
	      (progn
		(decf idle-workers workers)
		(incf monthly-workers workers))
	      (setf (tended farm) nil)))))))

(defun reproduce (place)
  (dolist (group (sentients-list place))
    (if (< (length (afflictions-list place)) 3)
	(let ((increase
	       (random (+ 2 (* (round (/ (reproductive-capacity (get-population-type group))
				      (* 12 100)))
			       (quantity group))))))
	  (incf (idle-workers place) increase)
	  (incf (quantity group) increase)))))
				       

(defun update-populations (current-sector)
  (let ((ppl 0))
    (dolist (i (sentients-list current-sector))
      (incf ppl (quantity i)))
    (setf (total-workforce current-sector) ppl)))

(defun check-jobs (place)
  (if (job-queue place)
      (let ((i -1))
	(loop
	   (incf i 1)	   
	   (if (>= i (length (job-queue place)))
	       (return))
	   (let ((j (aref (job-queue place) i)))
	     (if (equalp (job-id j) +clear-land+)
		 (decf (hours-remaining j) (workers-used j))
		 (decf (hours-remaining j) 1))
	     (if (<= (hours-remaining j) 0)
		 (progn
		   (finish-job j place)
		   (vector-remove i (job-queue place)))))))))

(defun finish-job (j place)
  (incf (idle-workers place) (workers-used j)) 
  (cond ((equalp (job-id j) +forage+)
	 (finish-foraging j))
	((equalp (job-id j) +fish+)
	 (finish-fishing j))
	((equalp (job-id j) +make-goods+)
	 (finish-goods j))
	((equalp (job-id j) +hunt+)
	 (finish-hunting j))
	((equalp (job-id j) +explore+)
	 (finish-exploring j))
	((equalp (job-id j) +clear-land+)
	 (progn
	   (message "Land has been cleared")
	   (incf (cleared (origin j)) (yield j))))))

;;----------------------------------------- alerts

(defun check-alerts ()
  (if (and (alert-queue *the-world*) (null (showing-alert? *the-world*))
	   (not (equalp (type-of (current-game-state *the-world*)) 'input-game-state)))
      (progn
	(setf (showing-alert? *the-world*) t)
	(with-slots (strings prompts result-func title) (first (alert-queue *the-world*))
	  (draw-info-box strings)
	  (if prompts
		(set-state-to-input title prompts result-func))))))

(defun next-alert ()
  (pop (alert-queue *the-world*))
  (check-alerts))

;;--------------------------------- task lists

(defun hourly-tasks ()
  (dotimes (f-index +total-factions+) 
    (loop for current-sector being the elements of (controlled-sectors (get-faction f-index)) do
	 (check-jobs current-sector)
	 (check-alerts)
	 (sdl:update-display))))

(defun daily-tasks ()
  (dotimes (f-index +total-factions+) 
    (loop for current-sector being the elements of (controlled-sectors (get-faction f-index)) do
	 (assign-workers current-sector)
	 (check-farms current-sector)
	 (eat current-sector)
	 (check-malnutrition current-sector))))

(defun monthly-tasks ()
  (dotimes (f-index +total-factions+) 
    (loop for current-sector being the elements of (controlled-sectors (get-faction f-index)) do
	 (reproduce current-sector)
	 (update-populations current-sector))))
  
;;--------------------------------the event queue
(defun process-ticks-on-load ()
  (let ((old-time (last-played *the-world*)))
    (print old-time)
    (let ((ticks (floor (- (get-universal-time) old-time) +seconds-per-tick+ )))
      (process-some-ticks ticks)
      (incf (last-played *the-world*) (* ticks +seconds-per-tick+)))))

(defun process-some-ticks (&optional (num +tick-processing-interval+))
  (when (mutex *the-world*)
      (setf (mutex *the-world*) nil)
      (dotimes (i num) (process-a-tick))
      (setf (mutex *the-world*) t)))

(defun process-a-tick ()
  (with-slots (hour-num day-num month-num year-num) *the-world* 
    (incf hour-num 1)
    (hourly-tasks)
    (when (equalp 25 hour-num)
      (setf hour-num 1)
      (incf day-num 1)
      (daily-tasks)
      (when (equalp 31 day-num)
	(setf day-num 1)
	(incf month-num 1)
	(monthly-tasks)
	(message "New month!")
	(when (equalp 13 month-num)
	  (setf month-num 1)
	  (incf year-num 1)))))
  (draw-date))

(defun launch-timer ()
  (let ((trigger-time (* +seconds-per-tick+ +tick-processing-interval+)))
    (schedule-timer
     (make-timer #'process-some-ticks :name "the-tick-timer")
     trigger-time
     :repeat-interval trigger-time)))

