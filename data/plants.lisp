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



(defclass plant ()
  ((name :initarg :name :accessor name :initform nil)
   ;; month, day pair expected
   (growth-start-date :initarg :growth-start-date :reader growth-start-date
		      :initform (error "no growth date"))
   (fruiting-start-date :initarg :fruiting-start-date :reader fruiting-start-date
			:initform (error "no fruiting date"))
   (dormancy-start-date :initarg :dormancy-start-date :reader dormancy-start-date
			:initform (error "no dormancy date"))
   ;;multiplier between 0 and 10
   (harvest-difficulty :initarg :harvest-difficulty :reader harvest-difficulty
		       :initform (error "no harvest difficulty"))
   (product-list :initarg :product-list :reader product-list :initform nil)
   ;;Rainfall is in cm/year
   (temp-desired :initarg :temp-desired :reader temp-desired :initform 80)
   (rainfall-desired :initarg :rainfall-desired
		     :reader rainfall-desired :initform (error "no rainfall info"))))

(defclass bush (plant)
  ((maturity-time :initarg :maturity-time :reader maturity-time :initform (error "no maturity info"))))

(defclass native-plant (plant)
  ((known-to-player :initarg :known-to-player :accessor known-to-player :initform nil)
   (description :initarg :description :accessor description :initform nil)))

(defclass product ()
  ((name :initarg :name :accessor name)
   (yield :initarg :yield :accessor yield))) ;; pounds per acre

(defclass native-food (product)
  ((description :initarg :description :accessor description)
   (chemicals :initarg :chemicals :accessor chemicals
	      :initform (make-array +total-chemicals+ :initial-element 0))
   (nutrients :initarg :nutrients :accessor nutrients
	      :initform (make-array +total-nutrients+ :initial-element 0))))

(defclass food (product)
  ((nutrients :initarg :nutrients :accessor nutrients
	      :initform (make-array +total-nutrients+ :initial-element 0))))
