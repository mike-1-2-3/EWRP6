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


(defclass population ()
  ;; quantity given in percent for innumerable critters
  ((quantity :initarg :quantity :accessor quantity :initform (error "no quantity"))
   (species-index :initarg :species-index :accessor species-index :initform (error "no species"))))

;;----------------------------- Plants!

(defclass plant-population (population)
   ;; a multiplier greater than zero, and usually below 1.5
   ((env-suitability :initarg :env-suitability :accessor env-suitability)))

(defclass farmed-plant-population (plant-population)
  ((tended :initarg :tended :accessor tended :initform T)
   (age :initarg :age :accessor age :initform 0)))

;;----------------------- people!

(defclass sentient-population (population)
  ((growth-rate :initarg :growth-rate :accessor growth-rate :initform 1)))

;;----------------------- animals!

(defclass animal-population (population) ())

(defclass livestock-population (population) ())


