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


(defclass sector ()
  ((arable :initarg :arable :accessor arable :initform (error "needs more info"))
   (cleared :initarg :cleared :accessor cleared :initform (error "needs more info"))
   (salt-water :initarg :salt-water :accessor salt-water :initform (error "needs more info"))
   (fresh-water :initarg :fresh-water :accessor fresh-water :initform (error "needs more info"))
   (settlement :initarg :settlement :accessor settlement :initform nil)
   (player-knowledge :initarg :player-knowledge :accessor player-knowledge :initform +fog-of-war+)
   (tile-id :initarg :tile-id :accessor tile-id :initform 0)
   (rainfall :initarg :rainfall :accessor rainfall :initform (error "sector needs rainfall"))
   (avg-summer-temp :initarg :avg-summer-temp :accessor avg-summer-temp :initform 0)
   (animals-list :initarg :animals-list :accessor animals-list :initform nil)
   (plants-list :accessor plants-list :initform nil)))

;; this class "overlays" a sector controlled by a faction
;; basically, synonymous with settlement or town
(defclass aligned-sector ()
  ((underlying-sector :initarg :underlying-sector :accessor underlying-sector)
   (controller-index :initarg :controller-index :accessor controller-index :initform
		     (error "no controller"))
   (total-workforce :accessor total-workforce)
   (idle-workers :accessor idle-workers)
   (monthly-workers :accessor monthly-workers)
   (job-queue :initarg :job-queue :accessor job-queue :initform (make-array 2000 :fill-pointer 0))
   (sentients-list :accessor sentients-list :initform nil)
   (livestock-list :initarg :livestock-list :accessor livestock-list :initform nil)
   (farms-list :initarg :farms-list :accessor farms-list :initform nil)
   (afflictions-list :initarg :afflictions-list :accessor afflictions-list
		     :initform nil) ;; (nutrient, severity)
   (good-stores :initarg :good-stores :accessor good-stores
		:initform (make-array +total-goods+ :initial-element 0))
   (mat-stores :initarg :mat-stores :accessor mat-stores
		:initform (make-array +total-materials+ :initial-element 0))
   (food-stores :initarg :food-stores :accessor food-stores
		:initform (make-array +total-nutrients+ :initial-element 30000))))

;;-------------------The massive, global, singleton that holds all persistant data
(defclass world ()
  ((world-grid :initarg :world-grid :accessor world-grid
	       :initform (make-array (list +map-size+ +map-size+)))
   (mutex :initarg :mutex :accessor mutex :initform t)
   (alert-queue :initarg :alert-queue :accessor alert-queue :initform nil)
   (current-game-state :accessor current-game-state)
   (queued-game-states :initarg :queued-game-states :accessor queued-game-states :initform nil)
   (the-menu-state :accessor the-menu-state)
   (the-sector-state :accessor the-sector-state)
   (the-input-state :accessor the-input-state)
   (showing-alert? :initarg :showing-alert? :accessor showing-alert? :initform nil)
   (cursor-x :initarg :cursor-x :accessor cursor-x :initform 1)
   (cursor-y :initarg :cursor-y :accessor cursor-y :initform 1)
   (time-created :initarg :time-created :reader time-created :initform (get-universal-time))
   (last-played :initarg :last-played :accessor last-played :initform (get-universal-time))
   (player-name :initarg :player-name :accessor player-name :initform "User")
   (year-num :initarg :year-num :accessor year-num :initform 2016)
   (month-num :initarg :month-num :accessor month-num :initform 1)
   (day-num :initarg :day-num :accessor day-num :initform 1)
   (hour-num :initarg :hour-num :accessor hour-num :initform 1)
   ;; instance arrays
   (mats-array :initarg :mats-array :accessor mats-array :initform (make-array +total-materials+))
   (goods-array :initarg :goods-array :accessor goods-array :initform (make-array +total-goods+))
   (factions-array :initarg :factions-array :accessor factions-array
		   :initform (make-array +total-factions+))
   (plant-instance-array :initarg :plant-instance-array :accessor plant-instance-array
			 :initform (make-array (+ +total-types-of-plants+ +random-plants+)
					       :fill-pointer 0))
   (native-plant-array :initarg :native-plant-array :accessor native-plant-array
		       :initform (make-array (list +num-biomes+ +plants-per-biome+)))
   (animal-array :initarg :animal-array :accessor animal-array
		 :initform (make-array +total-types-of-animals+))
   (affliction-array :initarg :affliction-array :accessor affliction-array
		     :initform (make-array +total-afflictions+))
   (sentient-array :initarg :sentient-array :accessor sentient-array
		   :initform (make-array +total-types-of-sentients+))))

(defun selected-sector ()
  (get-sector (cursor-x *the-world*) (cursor-y *the-world*)))

(defun selected-town ()
  (settlement (selected-sector)))

;;------------------------- some sector access code shortening functions
(defun get-farms ()
  (farms-list (settlement (selected-sector))))

(defmethod push-item ((place aligned-sector) (item farmed-plant-population))
  (push item (farms-list place)))

(defmethod push-item ((place faction) (item aligned-sector))
  (push item (controlled-sectors place)))

(defmethod push-item ((place sector) (item animal-population))
  (push item (animals-list place)))

(defmethod push-item ((place aligned-sector) (item livestock-population))
  (push item (livestock-list place)))

(defmethod push-item ((place aligned-sector) (item sentient-population))
  (push item (sentients-list place)))

;; ------------------------ some world access code shortening functions  
(defmethod push-item ((place integer) (item plant))
  (vector-push item (plant-instance-array *the-world*)))

(defmethod push-item ((place integer) (item affliction))
  (setf (aref (affliction-array *the-world*) place) item)) 

(defmethod push-sentient ((place integer) (item sentient-animal))
  (setf (aref (sentient-array *the-world*) place) item))

(defmethod push-item ((place integer) (item animal))
  (setf (aref (animal-array *the-world*) place) item))

(defmethod push-item ((place integer) (item faction))
  (setf (aref (factions-array *the-world*) place) item))

(defmethod get-population-type ((pop farmed-plant-population))
  (aref (plant-instance-array *the-world*) (species-index pop))) 

(defmethod get-population-type ((pop livestock-population))
  (aref (animal-array *the-world*) (species-index pop)))

(defmethod get-population-type ((pop animal-population))
  (aref (animal-array *the-world*) (species-index pop))) 

(defmethod get-population-type ((pop sentient-population))
  (aref (sentient-array *the-world*) (species-index pop))) 

(defun get-faction (position)
  (aref (factions-array *the-world*) position))

(defun get-owner (x y)
  (get-faction (controller-index (settlement (get-sector x y)))))

(defun get-sector (x y)
  (aref (world-grid *the-world*) x y))

(defun current-date ()
  (list (month-num *the-world*) (day-num *the-world*)))

(defun get-plant (index)
  (aref (plant-instance-array *the-world*) index))

(defun get-products (index)
  (product-list (get-plant index)))

;;--------------------------------- get the names of things

(defmethod get-name ((item farmed-plant-population))
  (concatenate 'string
	       (name (aref (plant-instance-array *the-world*) (species-index item)))
	       " "
	       (write-to-string (quantity item))
	       " acres"))

(defmethod get-name ((item product))
  (name item))

(defmethod get-name ((item plant))
  (name item))

(defmethod get-name ((item menu))
  (name item))
