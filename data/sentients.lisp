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

(defclass sentient ()
  ((description :initarg :description :accessor description :initform nil)
   (emotional-intelligence :initarg :emotional-intelligence
			   :accessor emotional-intelligence) ;;0-100
   (analytical-intelligence :initarg :analytical-intelligence
			    :accessor analytical-intelligence))) ;;0-100

(defclass sentient-animal (sentient animal)
  ((poisonous-chemicals :initarg :poisonous-chemicals :accessor poisonous-chemicals :initform nil)
   (poisonous-nutrients :initarg :poisonous-nutrients :accessor poisonous-nutrients :initform nil)
   (chemical-needs :initarg :chemical-needs :accessor chemical-needs :initform
		   (make-array +total-chemicals+ :initial-element 0))
   (nutrition-needs :initarg :nutrition-needs :accessor nutrition-needs :initform
		    (make-array +total-nutrients+ :initial-element 0))))

(defclass sentient-plant (sentient plant) ())

(defclass faction ()
  ((description :initarg :description :accessor description :initform nil)
   (intel :initarg :intel :accessor intel :initform (make-array +total-factions+ :initial-element nil))
   (name :initarg :name :accessor name)
   (color1 :initarg :color1 :accessor color1)
   (color2 :initarg :color2 :accessor color2)
   (controlled-sectors :initarg :controlled-sectors :accessor controlled-sectors :initform nil)
   (bellicose :initarg :bellicose :accessor bellicose))) ;; 0 to 10
