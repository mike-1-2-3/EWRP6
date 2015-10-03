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



(defclass affliction ()
  ((descriptions :initarg :descriptions :accessor descriptions :initform (error "need descriptions"))
   (fatality-threshold :initarg :fatality-threshold :accessor fatality-threshold :initform 3)
   (fatality-chance :initarg :fatality-chance :accessor fatality-chance :initform (error "needs amt"))
   (fatality-amount :initarg :fatality-amount :accessor fatality-amount :initform
		    (error "needs amt"))))

(defclass malnurishment (affliction)
  ((missing-nutrient :initarg :missing-nutrient :accessor missing-nutrient)
   (creature-days-per-severity :initarg :creature-days-per-severity
			       :accessor creature-days-per-severity :initform 30)))

