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



;;----------------------- generic function definitions
(defgeneric get-name (item))

(defgeneric get-man-hours (employer))

(defgeneric get-population-type (pop))

(defgeneric push-item (place item))
 ;; exists so human doesn't get treated as animal first, rewrite later
(defgeneric push-sentient (place item))

;;(defgeneric print-menu (menu items))

(defgeneric draw-menu-items (m-items first-elt selected max))

;;----------------------- some utility functions

;; compares two dates of format (month, date)
(defun date-less (date1 date2)
  (if (< (first date1) (first date2)) t
      (if (> (first date1) (first date2)) nil
	  (if (< (second date1) (second date2)) t nil))))

(defun datep (date1 date2)
  (if (and (equalp (first date1) (first date2))
	   (equalp (second date1) (second date2)))
      T))

;; ---------------- 1 sector is about 3 miles across, so 1 unit -> 1 hr travel
(defun get-distance (x1 y1 x2 y2)
  (round (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

;; ------- just makes sure the coors are in bounds
(defun check-coors (x y)
  (if (and (>= x 0) (< x +map-size+) (>= y 0) (< y +map-size+))
      t
      nil))
;; --------------------------------- last element replaces removed element
(defun vector-remove (i vec)
  (let ((len (length vec)))
    (if (equalp len 1)
	(vector-pop vec)
	(if (<= i len)
	    (progn
	      (setf (aref vec i) (aref vec (1- len)))
	      (decf (fill-pointer vec) 1))
	    (error "Vector-remove out of bounds")))))

;; WARNING - doesn't work on first element. You can't set what points to the first cons from here
(defun remove-nth (n list)
  (let ((cons (nthcdr (1- n) list)))
    (if cons
	(setf (cdr cons) (cddr cons))
	cons)))

