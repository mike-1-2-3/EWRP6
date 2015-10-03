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



(defclass job ()
  ((workers-used :initarg :workers-used :accessor workers-used)
   (hours-remaining :initarg :hours-remaining :accessor hours-remaining)
   (job-id :initarg :job-id :accessor job-id)
   (index :initarg :index :accessor index) ;;basic goods index, or military power, because lazy
   (yield :initarg :yield :accessor yield :initform nil)
   (tools :initarg :tools :accessor tools :initform nil)
   (destination :initarg :destination :accessor destination :initform nil)
   (origin :initarg :origin :accessor origin :initform (selected-town))))

(defclass good (product)
  ((hours :initarg :hours :accessor hours)
   (effectiveness :initarg :effectiveness :accessor effectiveness :initform 1)
   (mats :initarg :mats :accessor mats :initform nil) ;; list of lists (index, num)
   (good-mats :initarg :good-mats :accessor good-mats :initform nil)
   (tools :initarg :tools :accessor tools :initform nil)
   (techs :initarg :techs :accessor techs :initform nil)))

(defclass melee (good)
  ((armor-piercing :initarg :armor-piercing :accessor armor-piercing)
   (accuracy :initarg :accuracy :accessor accuracy)
   (fatality-chance :initarg :fatality-chance :accessor fatality-chance)
   (damage :initarg :damage :accessor damage)))

(defclass ranged (good)
  ((armor-piercing :initarg :armor-piercing :accessor armor-piercing)
   (accuracy :initarg :accuracy :accessor accuracy)
   (fatality-chance :initarg :fatality-chance :accessor fatality-chance)
   (damage :initarg :damage :accessor damage)))

(defclass armor (good)
  ((hardness :initarg :hardness :accessor hardness)
   (coverage :initarg :coverage :accessor coverage)
   (weight :initarg :weight :accessor weight)))
