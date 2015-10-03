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



(defclass animal ()
  ((name :initarg :name :accessor name :initform (error "no name"))
   (age-of-maturity :initarg :age-of-maturity :accessor age-of-maturity) ;;in months
   (age-of-senility :initarg :age-of-senility :accessor age-of-senility) ;; in months
   (avg-lifespan :initarg :avg-lifespan :accessor avg-lifespan) ;; in months
   (reproductive-capacity :initarg :reproductive-capacity :accessor reproductive-capacity) ;; 0-100
   (strength :initarg :strength :accessor strength) ;; 0 - 100
   (agility :initarg :agility :accessor agiltiy) ;; 0-100
   (aggressiveness :initarg :aggressiveness :accessor aggressiveness))) ;; 0-100

(defclass domestic-animal (animal)
  ((forage-required :initarg :forage-required :accessor forage-required))) ;; in acres of pasture
