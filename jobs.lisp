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


;;----------------------------------------- making goods

(defun supplies-available (item quantity)
  (let ((available t))
    (with-slots (mats good-mats tools) item
      (with-slots (good-stores mat-stores) (selected-town)
	(with-slots (goods-array mats-array) *the-world*
	  (dolist (m mats)
	    (if (< (aref mat-stores (first m)) (* (second m) quantity))
		(progn
		  (setf available nil)
		  (message (concatenate 'string "Needs " (write-to-string ( * (second m) quantity)) " "
					(name (aref mats-array (first m))))))))
	  (dolist (tool tools)
	    (if (< (aref good-stores tool) 1)
		(progn
		  (setf available nil)
		  (message (concatenate 'string "Needs " (name (aref goods-array tool)))))))
	  (dolist (tool good-mats)
	    (if (< (aref good-stores (first tool)) (* quantity (second tool)))
		(progn
		  (setf available nil)
		  (message (concatenate 'string "Needs " (name (aref goods-array (first tool))))))))
	  available)))))

(defun subtract-supplies (item quantity)
  (with-slots (mats good-mats tools) item
    (with-slots (good-stores mat-stores) (selected-town)
      (with-slots (goods-array) *the-world*
	(dolist (m mats)
	  (decf (aref mat-stores (first m)) (* (second m) quantity)))
	(dolist (tool tools)
	  (decf (aref good-stores tool) 1))
	(dolist (tool good-mats)
	  (decf (aref good-stores (first tool)) (* (second tool) quantity)))))))
	
(defun add-supplies (item j)
  (with-slots (mats good-mats tools) item
    (with-slots (good-stores mat-stores) (origin j)
      (with-slots (goods-array) *the-world*
	(dolist (tool tools)
	  (incf (aref good-stores tool) 1))))))

(defun finish-goods (j)
  (let ((item (aref (goods-array *the-world*) (index j))))
    (add-supplies item j)
    (incf (aref (good-stores (origin j)) (index j)) (yield j))))

(defun order-goods (quantity)
  (let ((item (aref (goods-array *the-world*) (selected-item (the-menu-state *the-world*)))))
    (if (supplies-available item quantity)
	(if (>= (idle-workers (selected-town)) 1)
	    (progn
	      (subtract-supplies item quantity)
	      (message "Making goods")
	      (decf (idle-workers (selected-town)) 1)
	      (vector-push (make-instance 'job :workers-used 1 
					  :yield (* quantity (yield (aref (goods-array *the-world*)
									  (selected-item
									   (the-menu-state
									    *the-world*)))))
					  :origin (selected-town)
					  :index (selected-item (the-menu-state *the-world*))
					  :hours-remaining (* (hours item) quantity)
					  :job-id +make-goods+)
			   (job-queue (selected-town))))
	    (message "Not enough people available")))))

;; ---------------------------------------------- Missions

(defun arm-group (job ranged?)
  (with-slots (good-stores) (origin job)
    (with-slots (goods-array) *the-world*
      (let* ((attack 0)
	     (defense 0)
	     (num (workers-used job))
	     (unarmed num)
	     (unarmored num)
	     (power 0)
	     (wpns (if ranged? +ranged+ +weapons+)))
	(dolist (w wpns)
	  (if (equalp unarmed 0)
	      (return))
	  (let ((stock (aref good-stores w)))
	    (if (> stock 0)
		(progn
		  (if (>= stock unarmed)
		      (setf stock unarmed))
		  (decf (aref good-stores w) stock)
		  (push (list w stock) (tools job))
		  (decf unarmed stock)
		  (incf attack (* stock (* (accuracy (aref goods-array w))
					   (+ (fatality-chance (aref goods-array w))
					      (damage (aref goods-array w))))))))))
	(dolist (w +armor+)
	  (if (equalp unarmored 0)
	      (return))
	  (let ((stock (aref good-stores w)))
	    (if (> stock 0)
		(progn
		  (if (>= stock unarmored)
		      (setf stock unarmored))
		  (decf (aref good-stores w) stock)
		  (push (list w stock) (tools job))
		  (decf unarmored stock)
		  (incf defense (* stock (- (* (hardness (aref goods-array w))
					       (coverage (aref goods-array w)))
					    (weight (aref goods-array w)))))))))
	(setf power (1+ (floor (+ attack defense) (* num 1.2))))
	(setf (index job) power)))))

(defun return-arms (job)
  (dolist (tool (tools job))
    (incf (aref (good-stores (origin job)) (first tool)) (second tool))))

(defun finish-exploring (j)
  (let ((luck (random 3))
	(dead 0))
    (if (> luck 0)
	(setf dead (round (* (random 20) (/ 10 (index j)))))
	(setf dead (round (* (random (workers-used j)) (/ 10 (index j))))))
    (if (>= dead (workers-used j))
	(progn
	  (deaths (workers-used j) (first (sentients-list (origin j))) (origin j))
	  (message "Explorers have gone missing"))
	(progn
	  (deaths dead (first (sentients-list (origin j))) (origin j))
	  (return-arms j)
	  (message (concatenate 'string "Explorers have returned, "
				(write-to-string dead) " were eaten."))
	  (explore (first (destination j)) (second (destination j)))
	  (draw-sectors)))))

(defun explore (x y)
  (let ((nx (- x 1))
	(ny (- y 1)))
    (dotimes (cx 3)
      (dotimes (cy 3)
	(if (check-coors (+ nx cx) (+ ny cy))
	    (let ((place (get-sector (+ nx cx) (+ ny cy))))
	      (if (settlement place)
		  (if (null (aref (intel (get-faction 0)) (controller-index (settlement place))))
		      (progn
			(setf (aref (intel (get-faction 0))
				    (controller-index (settlement place))) 1)
			(setf (alert-queue *the-world*)
			      (nconc (alert-queue *the-world*)
				     (list (make-instance
					    'alert :result-func (lambda())
					    :title ""
					    :prompts (list (make-instance
							    'prompt-info :prompt-string "" :input-type nil
							    :trailing-string ""))
					    :strings
					    (list
					     (description (get-faction (controller-index (settlement place))))
					     (description (get-population-type
							   (first (sentients-list (settlement place)))))
					     "Your explorers have encountered strange beings!"))))))))
	      (setf (player-knowledge place) 1))))))
  (setf (player-knowledge (get-sector x y)) 2))

(defun send-explorers (workers dest-y dest-x)
  (if (> workers 0)
      (if (>= (idle-workers (selected-town)) workers)
	  (if (check-coors dest-x dest-y)
	      (progn
		(decf (idle-workers (selected-town)) workers)
		(let ((group (make-instance 'job :workers-used workers
					    :origin (selected-town)
					    :destination (list dest-x dest-y)
					    :hours-remaining (+ 8 (* 2 (get-distance
									dest-x dest-y
									(cursor-x *the-world*)
									(cursor-y *the-world*))))
					    :job-id +explore+)))
		  (arm-group group nil)
		  (vector-push group (job-queue (selected-town)))))
	      (message "Invalid coordinates"))
	  (message "Not enough workers"))))

(defun send-hunters (workers)
  (if (> workers 0)
      (if (>= (idle-workers (selected-town)) workers)
	  (progn
	    (decf (idle-workers (selected-town)) workers)
	    (let ((group (make-instance 'job :workers-used workers
					:origin (selected-town)
					:hours-remaining 1
				    :job-id +hunt+)))
	      (arm-group group t)
	      (vector-push group (job-queue (selected-town)))))
	  (message "Not enough workers"))))

(defun get-job-tools (type-list job required?)
  (with-slots (good-stores) (origin job)
    (with-slots (goods-array) *the-world*
      (let* ((num (workers-used job))
	     (unarmed num)
	     (power num))
	(dolist (type type-list)
	  (if (equalp unarmed 0)
	      (return))
	  (let ((stock (aref good-stores type)))
	    (if (> stock 0)
		(progn
		  (if (>= stock unarmed)
		      (setf stock unarmed))
		  (decf (aref good-stores type) stock)
		  (push (list type stock) (tools job))
		  (decf unarmed stock)
		  (incf power (effectiveness (aref goods-array type)))))))
	(setf (index job) power)))))


(defun send-fishers (workers)
  (if (> workers 0)
  (if (>= (idle-workers (selected-town)) workers)
      (if (>= (+ (fresh-water (selected-sector)) (salt-water (selected-sector)))
	      (floor workers 2))
	  (progn
	    (decf (idle-workers (selected-town)) workers)
	    (let ((group (make-instance 'job :workers-used workers
					:origin (selected-town)
					:hours-remaining 8
					:job-id +fish+)))
	      (get-job-tools +fishing-tools+ group t)
	      (vector-push group (job-queue (selected-town)))))
	  (message "Not enough fishing ground"))
      (message "Not enough workers"))))

(defun finish-fishing (j)
  (message "Fishers have brought back their catches")
  (return-arms j)
  (add-food (make-instance 'food :nutrients
			   (make-array +total-nutrients+
				       :initial-contents
				       '(15 5 0 0 100 20 20 200 30 30 6 1)))
	    (random (index j))
	    (origin j)))


(defun finish-hunting (j)
  (let ((luck (random 3))
	(dead 0))
    (if (> luck 0)
	(setf dead (round (* (random 15) (/ 10 (index j)))))
	(setf dead (round (* (random (workers-used j)) (/ 10 (index j))))))
    (if (>= dead (workers-used j))
	(progn
	  (deaths (workers-used j) (first (sentients-list (origin j))) (origin j))
	  (message "Hunters have gone missing"))
	(progn
	  (deaths dead (first (sentients-list (origin j))) (origin j))
	  (return-arms j)
	  (add-food (make-instance 'food :nutrients
				   (make-array +total-nutrients+
					       :initial-contents
					       '(100 80 10 10 500 10 100 50 500 50 2 5)))
		    (round (* (random (workers-used j)) (1+ (/ (index j) 100))))
		    (origin j))
	  (incf (aref (mat-stores (origin j)) +skins+) (random (floor (workers-used j) 4)))
	  (message (concatenate 'string "Hunters have returned, "
				(write-to-string dead) " were eaten."))))))

(defun send-foragers (workers)
  (if (>= (idle-workers (selected-town)) workers)
      (progn
	(decf (idle-workers (selected-town)) workers)
	(vector-push (make-instance 'job :workers-used workers
				    :hours-remaining 8 :job-id +forage+)
		     (job-queue (selected-town)))
	(message "Foragers dispatched!"))
      (message "Not enough people available")))

(defun deaths (num population place)
  (when (> num 0)
    (if (equalp num 1)
	(message "Someone has died.")
	(message (concatenate 'string (write-to-string num) " people have died.")))
    (decf (quantity population) num)
    (update-populations place)
    (decf (idle-workers place) num)))
  
;;--------------------------------- Designations

(defun clear-acres (acres workers)
  (if (>= (arable (selected-sector)) acres)
      (if (>= (idle-workers (selected-town)) workers)
	  (progn
	    (decf (arable (selected-sector)) acres)
	    (decf (idle-workers (selected-town)) workers)
	    (vector-push (make-instance 'job :workers-used workers :yield acres
					:origin (selected-sector)
					:hours-remaining (* acres 50) :job-id +clear-land+)
			 (job-queue (selected-town))))
	  (message "Not enough people available"))
      (message "There aren't that many acres of useable land")))

(defun remove-farm (index)
  (if (>= index 0)
      (progn
	(incf (cleared (selected-sector))
	      (quantity (nth index (get-farms))))
	(if (zerop index)
	    (if (cdr (get-farms))
		(setf (farms-list (settlement (selected-sector))) (cdr (get-farms)))
		(setf (farms-list (settlement (selected-sector))) nil)))
	(remove-nth (+ index 1) (cons nil (get-farms))))))

(defun calc-plant-suitability (plant place)
  (if (typep plant 'native-plant)
      1.2
      (progn
	(+ (/ 5 (+ 8 (abs (- (rainfall-desired plant)
			       (rainfall place)))))
	   (/ 5 (+ 8 (abs (- (temp-desired plant)
			      (avg-summer-temp place)))))))))

(defun add-farm (amount)
  (let* ((type (selected-item (the-menu-state *the-world*)))
	 (plant (aref (plant-instance-array *the-world*) type))
	 (place (settlement (selected-sector)))
	 (in-time nil)
	 (suitability nil))
    (if (< (cleared (selected-sector)) amount)
	(message "Not enough room!")
	(if (> amount 0)
	    (progn
	      (setf suitability (calc-plant-suitability plant (selected-sector)))
	      (if (or (date-less (current-date) (growth-start-date (get-plant type)))
		      (date-less (dormancy-start-date (get-plant type)) (current-date)))
		  (setf in-time T))
	      (push-item place
			 (make-instance 'farmed-plant-population
					:quantity amount
					:tended in-time
					:env-suitability suitability
					:species-index type))
	      (decf (cleared (selected-sector)) amount)
	      (message "Land allocated!"))))))

;;------------------------ discovering plants


(defun add-food (food amount place)
  (with-slots (nutrients) food
    (dotimes (i +total-nutrients+)
      (incf (aref (food-stores place) i)
	    (* amount (aref nutrients i))))))

(defun finish-foraging (j)
  (let* ((success nil))
    (loop
       (let* ((x (random +num-biomes+))
	      (y (random +plants-per-biome+))
	      (plant (aref (native-plant-array *the-world*) x y)))
	 (if (known-to-player plant)
	     (if (null (poison? 0 x y))
		 (progn
		   (add-food (first (product-list plant)) (random (workers-used j)) (origin j))
		   (message (concatenate 'string "Some foragers have returned with "
					 (name plant)))
		   (setf success t)
		   (return)))
	     (return))))
    (if (null success)
	(let ((coors (next-new-plant)))
	  (if coors
	(let ((x (first coors))
	      (y (second coors)))
	  (setf (known-to-player (aref (native-plant-array *the-world*) x y)) t)
	  (setf (alert-queue *the-world*)
		(nconc (alert-queue *the-world*) (list
						  (make-instance
	    'alert :result-func #'name-plant
	    :title "New Plant"
	    :prompts (list (make-instance
				 'prompt-info :prompt-string "Enter name:" :input-type 'string
				 :trailing-string " "))
		 :strings
		 (list
		  "What would you like to name this plant?"
		  (if (poison? 0 x y)
		      (progn
			(deaths 1 (first (sentients-list (origin j))) (origin j))
			"She died a very painful death.")
		      "And she seems to be fine!")
		  "A brave voluteer put one in her mouth and chewed."
		  (concatenate
	     'string "They show you a pile of "
	     (description (first (product-list
				  (aref (native-plant-array *the-world*) x y))))
	     " that they got from "
	     (first (description (aref (native-plant-array *the-world*) x y)))
	     (second (description (aref (native-plant-array *the-world*) x y))))
		  "Your foragers have found something that might be food.")))
		       ))))))))
	

(defun next-new-plant ()
  (let ((coors nil))
    (dotimes (x +num-biomes+)
      (if coors
	  (return))
      (dotimes (y +plants-per-biome+)
	(if (null (known-to-player (aref (native-plant-array *the-world*) x y)))
	    (progn
	      (setf coors (list x y))
	      (return)))))
    coors))

(defun next-unnamed-plant ()
  (let ((coors nil))
    (dotimes (x +num-biomes+)
      (if coors
	  (return))
      (dotimes (y +plants-per-biome+)
	(if (null (name (aref (native-plant-array *the-world*) x y)))
	    (progn
	      (setf coors (list x y))
	      (return)))))
    coors))

(defun name-plant (name)
  (let* ((coors (next-unnamed-plant))
	 (x (first coors))
	 (y (second coors)))
    (setf (name (aref (native-plant-array *the-world*) x y)) name)
    (if (null (poison? 0 x y))
	(vector-push (aref (native-plant-array *the-world*) x y)
		     (plant-instance-array *the-world*)))
    (draw-sectors)))

(defun poison? (sentient-index biome-index p-index)
  (let ((ret nil))
    (dolist (chem (poisonous-chemicals (aref (sentient-array *the-world*) sentient-index)))
      (if (> (aref (chemicals
		    (first
		     (product-list (aref (native-plant-array
					*the-world*) biome-index p-index)))) chem)
	     0)
	  (setf ret t)))
    ret))
