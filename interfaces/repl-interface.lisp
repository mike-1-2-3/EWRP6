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


(defun popup (message)
  (print message))

(defun show-settlement-info ()
  (let ((place (settlement (selected-sector))))
    (print "total-workforce")
    (print (total-workforce place))
    (print "hours available")
    (print (labor-hours-available place))
    (print "hours used")
    (print (labor-hours-used place))
    (print "livestock list")
    (dolist (f (livestock-list place))
      (print (quantity f))
      (print (name (get-population-type f))))
    (print "farms list")
    (dolist (f (farms-list place))
      (print (quantity f))
      (print (name (get-population-type f))))))
  
(defun show-sector-info ()
  (let ((place (selected-sector)))
    (print "arable")
    (print (arable place))
    (print "cleared")
    (print (cleared place))
    (print "animals list")
    (dolist (f (animals-list place))
      (print (quantity f))
      (print (name (get-population-type f))))))


(defun show-world-information ()
  (print "plants")
  (dotimes (i +total-types-of-plants+)
    (print (name (aref (plant-instance-array *the-world*) i))))
  (print "animal-array")
  (dotimes (i +total-types-of-animals+)
    (print (name (aref (animal-array *the-world*) i))))
  (print "sentient array")
  (dotimes (i +total-types-of-sentients+)
    (print (name (aref (sentient-array *the-world*) i)))))


(defun calendar ()
  (print "hour:")
  (print (hour-num *the-world*))
  (print "day:")
  (print (day-num *the-world*))
  (print "month:")
  (print (month-num *the-world*))
  (print "year:")
  (print (year-num *the-world*)))
