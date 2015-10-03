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


(defclass prompt-info ()
  ((prompt-string :initarg :prompt-string :accessor prompt-string)
   (input-type :initarg :input-type :accessor input-type :initform 'integer)
   (trailing-string :initarg :trailing-string :accessor trailing-string)))

(defclass alert ()
  ((strings :initarg :strings :accessor strings :initform nil)
   (title :initarg :title :accessor title :initform nil)
   (result-func :initarg :result-func :accessor result-func)
   (prompts :initarg :prompts :accessor prompts :initform nil)))

(defclass menu ()
  ((parent :initarg :parent :accessor parent)
   (action :initarg :action :accessor action :initform nil)
   (selected-info-box :initarg :selected-info-box :accessor selected-info-box :initform nil)
   (selected-prompts :initarg :selected-prompts :accessor selected-prompts :initform nil)
   (child-action :initarg :child-action :accessor child-action :initform nil)
   (child-prompts :initarg :child-prompts :accessor child-prompts :initform nil)
   (child-info-box :initarg :child-info-box :accessor child-info-box :initform nil)
   (name :initarg :name :accessor name)
   (items :initarg :items :accessor items :initform nil)))


(defmethod push-item ((place menu) (item prompt-info))
  (push item (child-prompts place)))

(defmethod push-item ((place menu) (item menu))
  (push item (items place)))


(defun print-menu-tree (menu)
  (dolist (i (items menu))
    (if (equalp (type-of i) 'menu)
	(print-menu-tree i))
    (print (get-name i))))

