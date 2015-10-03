
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

(defgeneric screen-resized (game-state))
(defgeneric process-key-event (game-state key unicode))
(defgeneric draw (game-state))

;;------------------------------------------ Input Game State

(defclass input-game-state ()
  ((prompt-queue :initarg :prompt-queue :accessor prompt-queue :initform nil)
   (input-string :initarg :input-string :accessor input-string :initform "")
   (y-cor :initarg :y-cor :accessor y-cor :initform 5) ;; beginning y of current, three lined form
   (results-list :initarg :results-list :accessor results-list :initform nil)
   (resulting-func :initarg :resulting-func :accessor resulting-func :initform nil)))

(defgeneric next-prompt (input))  
(defmethod next-prompt ((input input-game-state))
  (with-slots (prompt-queue y-cor input-string resulting-func results-list) input
    (pop prompt-queue)
    (setf input-string "")
    (if prompt-queue
	(progn
	  (incf y-cor (+ 2 (* 3 (sdl:char-height *font*))))
	  (draw-prompt y-cor (prompt-string (first prompt-queue))
		       (trailing-string (first prompt-queue))))
	(progn
	  (apply resulting-func results-list)
	  (setf resulting-func nil
		results-list nil)
	  (next-game-state)))))

(defmethod draw ((game-state input-game-state))
  (with-slots (y-cor input-string) game-state
    (draw-menu-item input-string sdl:*yellow* 5 (+ 2 y-cor (sdl:char-height *font*)))
    (sdl:update-display)))

(defmethod process-key-event ((game-state input-game-state) key unicode)
  (with-slots (input-string y-cor prompt-queue results-list) game-state
    (when (sdl:key= key :sdl-key-backspace)
      (when (> (length input-string) 0)
	(clear-line (+ y-cor (sdl:char-height *font*) 2))
	(setf input-string (subseq input-string 0 (- (length input-string) 1)))))
    (when (and (> unicode 47) (< unicode 58))
      (if (equalp (input-type (first prompt-queue)) 'integer)
	  (setf input-string (concatenate 'string
					  input-string (list (code-char unicode))))))
    (when (or (equalp unicode 32)
	      (and (> unicode 47)
		   (not (equalp unicode 92))
		   (< unicode 123)))
      (when (equalp (input-type (first prompt-queue)) 'string)
	(setf input-string (concatenate 'string
					input-string (list (code-char unicode))))))
    (when (or (sdl:key= key :sdl-key-return) (sdl:key= key :sdl-key-kp-enter))
      (when (null (input-type (first prompt-queue)))
	(next-prompt game-state))
      (when (> (length input-string) 0)
	(if (equalp (input-type (first prompt-queue)) 'integer)
	    (push (parse-integer input-string) results-list)
	    (push input-string results-list))
	(next-prompt game-state)))))

    


;;------------------------------------------- Menu Game State
(defclass menu-game-state ()
  ((root-menu :initarg :root-menu :accessor root-menu)
   (current-menu :initarg :current-menu :accessor current-menu)
   (selected-item :initarg :selected-item :accessor selected-item :initform 0)
   (max-displayed :initarg :max-displayed :accessor max-displayed 
		  :initform (- (floor (/ *screenh* (sdl:char-height *font*))) 1))
   (first-element :initarg :first-element :accessor first-element :initform 0)
   (total-elements :initarg :total-elements :accessor total-elements :initform 4)))


(defmethod screen-resized ((game-state menu-game-state))
  (setf (max-displayed game-state) (- (floor (/ *screenh* (sdl:char-height *font*))) 1)))

(defmethod draw ((game-state menu-game-state))
  (with-slots (current-menu selected-item max-displayed first-element total-elements) game-state
    (draw-menu current-menu selected-item max-displayed first-element)
    (if (typep (items current-menu) 'list)
	(when (selected-info-box (nth selected-item (items current-menu)))
	  (draw-sectors)
	  (draw-info-box (selected-info-box (nth selected-item (items current-menu))))))
    (when (and (child-info-box current-menu) (items current-menu))
      (draw-sectors)
      (draw-info-box (child-info-box current-menu)))))

(defmethod process-key-event ((game-state menu-game-state) key unicode)
  (with-slots (current-menu selected-item max-displayed first-element total-elements) game-state
    (with-slots (items child-prompts child-action child-info-box action selected-prompts) current-menu
      (when (or (sdl:key= key :sdl-key-return) (sdl:key= key :sdl-key-kp-enter))
	(if child-prompts
	    (set-state-to-input (get-name (aref items selected-item)) child-prompts child-action)
	    (if child-action
		(progn
		  (funcall child-action selected-item))
		(progn
		  (if (typep (nth selected-item items) 'menu)
		      (progn
			(if (selected-prompts (nth selected-item items))
			    (set-state-to-input (name (nth selected-item items))
						(selected-prompts (nth selected-item items))
						(action (nth selected-item items))))
			(when (items (nth selected-item items))
			  (setf current-menu (nth selected-item items))
			  (setf selected-item 0)))))))
	(update-menu-elements))
      (when (sdl:key= key :sdl-key-escape)
	(if (equalp (parent current-menu) nil)
	    (set-state-to-sector)
	    (progn
	      (if child-info-box
		  (draw-sectors)
		  (if (typep (get-selected-child) 'menu)
		      (if (selected-info-box (get-selected-child))
			  (draw-sectors))))
	      (setf current-menu (parent current-menu))
	      (setf selected-item 0)
	      (update-menu-elements)
	      (setf selected-item 0))))
      (when (sdl:key= key :sdl-key-down)
	(if (equalp (- total-elements 1) selected-item)
	    (setf selected-item 0)
	    (incf selected-item 1)))
      (when (sdl:key= key :sdl-key-up)
	(if (equalp selected-item 0)
	    (setf selected-item (- total-elements 1))
	    (decf selected-item 1))))))

(defun update-menu-elements ()
  (with-slots (the-menu-state) *the-world*
    (with-slots (total-elements current-menu selected-item) the-menu-state
      (with-slots (items) current-menu
	(if (equalp (type-of items) 'function)
	    (setf total-elements (length (funcall items)))
	    (setf total-elements (length items)))
	(if (>= selected-item total-elements)
	    (decf selected-item 1))))))
	  
;;------------------------------------------sector selection state

(defclass sector-selection-state ()
  ((redraw :initarg :redraw :accessor redraw :initform t)
   (img-array :initarg :img-array :accessor img-array :initform (make-array +total-graphics+))
   (top-left-x :initarg :top-left-x :accessor top-left-x :initform 0) ;; start points for drawing
   (top-left-y :initarg :top-left-y :accessor top-left-y :initform 0)
   (max-tiles-x :initarg :max-tiles-x :accessor max-tiles-x :initform (floor (- *screenw* 220) 50))
   (max-tiles-y :initarg :max-tiles-y :accessor max-tiles-y
		:initform (floor (- *screenh* (+ 10 (sdl:char-height *font*))) 50))))

(defmethod draw ((game-state null))) ;;draw nothing after quit event

(defmethod draw ((game-state sector-selection-state))
  (if (equalp (redraw game-state) t)
      (progn
	(draw-sectors)
	(draw-sector-title))
      (setf (redraw game-state) t)))
      
    
(defmethod process-key-event ((game-state sector-selection-state) key unicode)
  (with-slots (img-array top-left-x top-left-y max-tiles-x max-tiles-y redraw) game-state
    (with-slots (cursor-x cursor-y) *the-world*      
;; Debug cheat key - make sure to comment out the start timer line in startup.lisp too
;;      (when (sdl:key= key :sdl-key-y)
;;	(setf (mutex *the-world*) t)
;;	(process-some-ticks 250))
      (when (sdl:key= key :sdl-key-t)
	(let ((q nil))
	  (push (make-instance 'prompt-info :trailing-string ""
			       :prompt-string "Y coordinate?" :input-type 'integer) q)
	  (push (make-instance 'prompt-info :trailing-string ""
			       :prompt-string "X coordinate?" :input-type 'integer) q)
	  (set-state-to-input "Teleport" q #'teleport)))
      (when (sdl:key= key :sdl-key-q)
	(save)
	(setf (mutex *the-world*) t)
	(cleanup)
	(sdl:push-quit-event))
      (when (or (sdl:key= key :sdl-key-return) (sdl:key= key :sdl-key-kp-enter))
	(if (not (settlement (selected-sector)))
	    (if (> (player-knowledge (selected-sector)) 1)
		(progn
		  (draw-info-box (generate-acreage-strings))
		  (setf redraw nil)))
	    (if (zerop (controller-index (settlement (selected-sector))))
		(set-state-to-menu))))
      (when (sdl:key= key :sdl-key-left)
	(when (> cursor-x 0)
	  (if (equalp cursor-x top-left-x)
	      (decf top-left-x 1))
	  (decf cursor-x 1)))
      (when (sdl:key= key :sdl-key-right)
	(when (< cursor-x (- +map-size+ 1))
	  (if (equalp cursor-x (- (+ top-left-x max-tiles-x) 1))
	      (incf top-left-x 1))
	  (incf cursor-x 1)))
      (when (sdl:key= key :sdl-key-down)
	(when (< cursor-y (- +map-size+ 1))
	  (if (equalp cursor-y (- (+ top-left-y max-tiles-y) 1))
	      (incf top-left-y 1))
	  (incf cursor-y 1)))
      (when (sdl:key= key :sdl-key-up)
	(when (> cursor-y 0)
	  (if (equalp cursor-y top-left-y)
	      (decf top-left-y 1))
	  (decf cursor-y 1))))))
    

(defun load-graphics (the-selection-state)
;;  (setf (aref (img-array the-selection-state) +grassland1+)
;;	(sdl:load-image "interfaces/grassland1.png"))
  (setf (aref (img-array the-selection-state) +grassland2+)
	(sdl:load-image "interfaces/grassland2.png"))
;;  (setf (aref (img-array the-selection-state) +grassland3+)
;;	(sdl:load-image "interfaces/grassland3.png"))
;;  (setf (aref (img-array the-selection-state) +grassland4+)
;;	(sdl:load-image "interfaces/grassland4.png"))
;;  (setf (aref (img-array the-selection-state) +desert1+) (sdl:load-image "interfaces/desert1.png"))
  (setf (aref (img-array the-selection-state) +desert2+) (sdl:load-image "interfaces/desert2.png"))
  (setf (aref (img-array the-selection-state) +forest1+) (sdl:load-image "interfaces/forest1.png"))
  (setf (aref (img-array the-selection-state) +mountain2+) (sdl:load-image "interfaces/mountain2.png"))
  (setf (aref (img-array the-selection-state) +mountain3+) (sdl:load-image "interfaces/mountain3.png")) 
  (setf (aref (img-array the-selection-state) +hill1+) (sdl:load-image "interfaces/hill1.png"))  
  (setf (aref (img-array the-selection-state) +hill2+) (sdl:load-image "interfaces/hill2.png"))  
;;  (setf (aref (img-array the-selection-state) +woodland1+) (sdl:load-image "interfaces/woodland1.png"))
  (setf (aref (img-array the-selection-state) +ocean1+) (sdl:load-image "interfaces/ocean1.png")))
;;  (setf (aref (img-array the-selection-state) +ocean2+) (sdl:load-image "interfaces/ocean2.png")))

(defmethod screen-resized ((game-state sector-selection-state))
  (setf (max-tiles-x game-state) (floor (- *screenw* 220) 50))
  (setf (max-tiles-y game-state) (floor (- *screenh* (+ 10 (sdl:char-height *font*))) 50)))

;;--------------------- changing states

(defun set-state-to-menu ()
  (with-slots (current-game-state the-menu-state) *the-world*
    (setf current-game-state the-menu-state
	  (current-menu current-game-state) (root-menu current-game-state)
	  (name (root-menu current-game-state))
	  (concatenate 'string "Sector: "
		       (write-to-string (cursor-x *the-world*)) ", " 
		       (write-to-string (cursor-y *the-world*)))))
  (update-menu-elements))

(defun set-state-to-sector ()
  (setf (current-game-state *the-world*) (the-sector-state *the-world*)))

(defun set-state-to-input (title prompt-queue func)
  (clear-menu)
  (draw-title title 5 5)
  (setf (y-cor (the-input-state *the-world*)) (+ 5 (sdl:char-height *title-font*)))
  (if (input-type (first prompt-queue))
      (draw-prompt (y-cor (the-input-state *the-world*)) (prompt-string (first prompt-queue))
		   (trailing-string (first prompt-queue))))
  (setf (prompt-queue (the-input-state *the-world*)) prompt-queue)
  (setf (resulting-func (the-input-state *the-world*)) func)
  (push (current-game-state *the-world*) (queued-game-states *the-world*))
  (setf (current-game-state *the-world*) (the-input-state *the-world*)))
  

(defun next-game-state ()
  (setf (current-game-state *the-world*) (pop (queued-game-states *the-world*)))
  (if (showing-alert? *the-world*)
      (progn
	(setf (showing-alert? *the-world*) nil)
	(next-alert))))


