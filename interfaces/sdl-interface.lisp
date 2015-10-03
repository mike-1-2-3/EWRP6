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


;;------------------------------------ sector stuff
(defun draw-sector-title ()
  (clear-menu)
  (sdl:draw-string-solid
   (concatenate 'string "Sector: "
		(write-to-string (cursor-x *the-world*)) ", " 
		(write-to-string (cursor-y *the-world*)))
   (sdl:point :x 5 :y 5)
   :color sdl:*white* :font *title-font*)
  (let ((green (sdl:color :r 99 :g 250 :b 82)))
    (sdl:draw-string-solid "enter" (sdl:point :x 5 :y 30) :color green :font *font*)
    (sdl:draw-string-solid " - select" (sdl:point :x (+ 5 (* *char-width* 5)) :y 30)
			   :color sdl:*white* :font *font*)
    (sdl:draw-string-solid "t" (sdl:point :x 5 :y (+ 30 *char-height* +line-spacing+))
			   :color green  :font *font*)
    (sdl:draw-string-solid " - teleport" (sdl:point :x (+ 5 *char-width*)
						    :y (+ 30 *char-height* +line-spacing+))
			   :color sdl:*white* :font *font*)
    (sdl:draw-string-solid "q" (sdl:point :x 5 :y (+ 30 (* 2 (+ +line-spacing+ *char-height*))))
			   :color green :font *font*)
    (sdl:draw-string-solid " - save and quit"
			   (sdl:point :x (+ 5 *char-width*)
				      :y (+ 30 (* 2 (+ +line-spacing+ *char-height*))))
			   :color sdl:*white* :font *font*)))

(defun draw-sectors ()
  (with-slots (img-array max-tiles-x max-tiles-y top-left-x top-left-y) (the-sector-state *the-world*) 
    (sdl:draw-box (sdl:rectangle :x 210
				 :y 10
				 :w (* +tile-size+ max-tiles-x)
				 :h (* +tile-size+ max-tiles-y))
		  :color sdl:*black*)
    (dotimes (y max-tiles-y)
      (dotimes (x max-tiles-x)
	(if (> (player-knowledge (get-sector (+ x top-left-x) (+ y top-left-y))) 0)
	    (progn
	      (sdl:draw-surface-at (aref img-array (tile-id (get-sector (+ x top-left-x)
									(+ y top-left-y))))
				   (sdl:point :x (+ (* x 50) 210) :y (+ (* y 50) 10)))
	      (if (settlement (get-sector (+ x top-left-x) (+ y top-left-y)))
		  (highlight-sector x y
				    (color1 (get-owner (+ x top-left-x) (+ y top-left-y)))
				    (color2 (get-owner (+ x top-left-x) (+ y top-left-y)))))))))
    (highlight-sector (- (cursor-x *the-world*) top-left-x)
		      (- (cursor-y *the-world*) top-left-y) sdl:*yellow*
		      (if (settlement (get-sector (cursor-x *the-world*) (cursor-y *the-world*)))
			  (color2 (get-owner (cursor-x *the-world*) (cursor-y *the-world*)))
			  (sdl:color :r 218 :g 165 :b 32)))))

(defun highlight-sector (x-pos y-pos color1 color2)
  (let
      ((offset 49)
       (x1 (+ 210 (* x-pos 50)))
       (y1 (+ 10 (* y-pos 50))))
    (sdl:draw-shape (list
		     (sdl:point :x x1 :y y1) (sdl:point :x (+ offset x1) :y y1)
		     (sdl:point :x (+ x1 offset) :y (+ y1 offset)) (sdl:point :x x1 :y (+ y1 offset))
		     (sdl:point :x x1 :y y1))
		    :color color1)
    (incf x1 1)
    (incf y1 1)
    (decf offset 2)
    (sdl:draw-shape (list
		     (sdl:point :x x1 :y y1) (sdl:point :x (+ offset x1) :y y1)
		     (sdl:point :x (+ x1 offset) :y (+ y1 offset)) (sdl:point :x x1 :y (+ y1 offset))
		     (sdl:point :x x1 :y y1))
		    :color color2)
    (incf x1 1)
    (incf y1 1)
    (decf offset 2)
    (sdl:draw-shape (list
		     (sdl:point :x x1 :y y1) (sdl:point :x (+ offset x1) :y y1)
		     (sdl:point :x (+ x1 offset) :y (+ y1 offset)) (sdl:point :x x1 :y (+ y1 offset))
		     (sdl:point :x x1 :y y1))
		    :color color1)))

(defun teleport (y x)
  (with-slots (top-left-x top-left-y max-tiles-x max-tiles-y) (the-sector-state *the-world*)
    (with-slots (cursor-x cursor-y) *the-world*
      (if (and (>= x 0) (< x +map-size+))
	  (setf cursor-x x))
      (if (and (>= y 0) (< y +map-size+))
	  (setf cursor-y y))
      (if (> (- x (floor max-tiles-x 2)) 0)
	  (if (< (+ x (ceiling max-tiles-x 2)) +map-size+)
	      (setf top-left-x (- x (floor max-tiles-x 2)))
	      (setf top-left-x (- +map-size+ max-tiles-x)))
	  (setf top-left-x 0))
      (if (> (- y (floor max-tiles-y 2)) 0)
	  (if (< (+ y (ceiling max-tiles-y 2)) +map-size+)
	      (setf top-left-y (- y (floor max-tiles-y 2)))
	      (setf top-left-y (- +map-size+ max-tiles-y)))
	  (setf top-left-y 0)))))
      

;;-------------------------------------------- message bar

(defun message (text)
  (sdl:draw-box (sdl:rectangle :x 210
			       :y (- *screenh* (+ 2(sdl:char-height *font*)))
			       :w (- *screenw* +menu-width+ 1)
			       :h (sdl:char-height *font*))
		:color sdl:*black*)
  (sdl:draw-string-solid text 
			 (sdl:point :x 210
				    :y (- *screenh* (+ 2 (sdl:char-height *font*))))
			 :color sdl:*white* :font *font*))

			  
(defun draw-date ()
  (sdl:draw-box (sdl:rectangle :x 5
			       :y (- *screenh* (+ 2(sdl:char-height *font*)))
			       :w 200
			       :h (sdl:char-height *font*))
		:color sdl:*black*)
  (sdl:draw-string-solid (concatenate 'string
				      (write-to-string (month-num *the-world*))
				      "/" (write-to-string (day-num *the-world*))
				      "/" (write-to-string (year-num *the-world*))
				      "  " (write-to-string (hour-num *the-world*))
				      ":00 ")
			 (sdl:point :x 5
				    :y (- *screenh* (+ 2 (sdl:char-height *font*))))
			 :color sdl:*white* :font *font*))


(defun draw-loading-item (text x y)
  (sdl:draw-string-solid text
			 (sdl:point :x x :y y)
			 :color sdl:*white* :font *font*) 
  (sdl:update-display))

;;--------------------------------------------Menu stuff

(defun clear-line (y)
  (sdl:draw-box (sdl:rectangle :x 5 :y y :w 200 :h (+ 2 (sdl:char-height *font*)))
		:color (sdl:color :r 0 :g 0 :b 60)))


(defun draw-prompt (y-start text-1 text-2)
  (draw-menu-item text-1 sdl:*white* 5 y-start)
  (clear-line (+ y-start (sdl:char-height *font*) 2))
  (draw-menu-item text-2 sdl:*white* 5 (+ 4 (* 2 (sdl:char-height *font*)) y-start)))

(defun draw-menu-item (text color x y)
  (sdl:draw-string-solid text
			 (sdl:point :x x :y y)
			 :color color :font *font*))

(defun clear-menu ()
  (sdl:draw-box (sdl:rectangle :x 2 :y 2 :w (- +menu-width+ 3)
			       :h (- *screenh* (+ 2(sdl:char-height *font*))))
		:color sdl:*black*))


(defun draw-title (text x y)
  (sdl:draw-string-solid text
			 (sdl:point :x x :y y)
			 :color sdl:*white* :font *title-font*))

(defmethod draw-menu-items ((m-items function) (first-elt integer) (selected integer) (max integer))
  (draw-menu-items (funcall m-items) first-elt selected max))

(defmethod draw-menu-items ((m-items list) (first-elt integer) (selected integer) (max integer))
  (let ((i 0) (y (+ 5 (sdl:char-height *title-font*))))
    (dolist (item m-items)
      (if (and (>= i first-elt) (< i max))
	  (progn (if (equalp i selected)
		     (draw-menu-item (get-name item) sdl:*yellow* 5 y)
		     (draw-menu-item (get-name item) sdl:*white* 5 y))		     
		 (incf y (sdl:char-height *font*))))
      (incf i 1))))

(defmethod draw-menu-items ((m-items array) (first-elt integer) (selected integer) (max integer))
  (let ((y (+ 5 (sdl:char-height *title-font*))))
    (if (< (length m-items) max)
	(setf max (length m-items)))
    (dotimes (i max)
      (progn (if (equalp i selected)
		 (draw-menu-item (name (aref m-items i)) sdl:*yellow* 5 y)
		 (draw-menu-item (name (aref m-items i)) sdl:*white* 5 y))		     
	     (incf y (sdl:char-height *font*))))))

(defun draw-menu (current-menu selected max first-elt)
  (clear-menu)
  (draw-title (name current-menu) 5 5)
    (draw-menu-items (items current-menu) first-elt selected max))

;;-------------------- decorations

(defun draw-borders ()
  (sdl:draw-shape (list
		   (sdl:point :x 1 :y 1) (sdl:point :x (1- *screenw*)  :y 1)
		   (sdl:point :x (1- *screenw*) :y (1- *screenh*)) (sdl:point :x 1 :y (1- *screenh*))
		   (sdl:point :x 1 :y 1))
		  :color sdl:*yellow*))

(defun draw-sector-border ()
  (with-slots (max-tiles-x max-tiles-y) (the-sector-state *the-world*) 
    (sdl:draw-shape (list
		     (sdl:point :x (- +menu-width+ 1) :y 2)
		     (sdl:point :x (- +menu-width+ 1) :y (- *screenh* 2)))
		    :color (sdl:color :r 73 :g 95 :b 102))))

;; ---------------------------- Info box

(defun break-up-string (item)
  (let* ((max-chars (floor (- *screenw* (+ 45 235)) *char-width*))
	 (len (length item))
	 (new-strings)
	 (cur-char 0)
	 (end 0)
	 (next-char 0))
    (loop
       (if (> (+ cur-char max-chars) len)
	   (progn 
	     (push (subseq item cur-char) new-strings)
	     (return))
	   (setf end (+ cur-char max-chars)))
       (if (setf next-char (position #\  item :from-end t :start cur-char :end end))
	   (progn
	     (push (subseq item cur-char next-char) new-strings))
	   (progn
	     (push (subseq item next-char) new-strings)
	     (return)))
       (setf cur-char (1+ next-char)))
    new-strings))


(defun draw-info-box (strings)
  (if (typep strings 'function)
      (setf strings (funcall strings)))
  (let* ((newstrings nil)
	 (total-lines 0)
	 (total-items (length strings))
	 (maxheight (- *screenh* 70))
	 (height 0)
	 (cury 35))
    (dolist (str strings)
      (if (typep str 'function)
	  (setf str (funcall str)))
      (dolist (s (break-up-string str))
	(push s newstrings)
	(incf total-lines 1))
      (push nil newstrings))
    (setf height (+ (* total-lines (+ 1 *char-height* )) (* total-items +line-spacing+)))      
    (if (> height maxheight)
	(progn
	  (setf height maxheight)
	  (setf total-items (floor height (+ +line-spacing+ *char-height*)))))
    (sdl:draw-box (sdl:rectangle :x 235 :y 35
				 :w (- *screenw* 280)
				 :h height)
		  :color sdl:*black*)
    (sdl:draw-shape (list
		     (sdl:point :x 235 :y 35)
		     (sdl:point :x (- *screenw* 45) :y 35)
		     (sdl:point :x (- *screenw* 45) :y (+ height 35))
		     (sdl:point :x 235 :y (+ height 35))
		     (sdl:point :x 235 :y 35))
		    :color sdl:*yellow*)
    (dolist (str newstrings)
      (if str
	  (progn
	    (sdl:draw-string-solid str
				   (sdl:point :x (+ 235 +line-spacing+)  :y cury)
				   :color sdl:*white* :font *font*)
	    (incf cury (+ 1 *char-height*)))
	  (incf cury +line-spacing+)))))

