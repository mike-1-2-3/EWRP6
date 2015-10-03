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



(defun make-raw-mats ()
  (let ((mats (mats-array *the-world*)))
    (setf (aref mats +fibers+) (make-instance 'product :name "Fibers" :yield 5))
    (setf (aref mats +skins+) (make-instance 'product :name "Skins" :yield 1))))	
