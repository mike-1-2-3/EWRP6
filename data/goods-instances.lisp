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



(defun make-goods ()
  (let ((goods (goods-array *the-world*)))
    (setf (aref goods +shoddy-spear+)
	  (make-instance 'ranged :name "Shoddy spear" :yield 1 :hours 2 :tools (list +stone-knife+)
			 :armor-piercing 1 :accuracy 1 :fatality-chance 15 :damage 20))
    (setf (aref goods +simple-bow+)
	  (make-instance 'ranged :name "Simple bow" :yield 1 :hours 32 :tools (list +stone-knife+)
			 :good-mats (list (list +thread+ 1))			 
			 :armor-piercing 1 :accuracy 2 :fatality-chance 15 :damage 20))
    (setf (aref goods +shoddy-shield+)
	  (make-instance 'armor :name "Shoddy shield" :yield 1 :hours 4 :tools (list +stone-knife+)
			 :hardness 1 :coverage 1 :weight 5))
    (setf (aref goods +stone-knife+)
	  (make-instance 'melee :name "Stone knife" :yield 1 :hours 8
			 :armor-piercing 1 :accuracy 1 :fatality-chance 10 :damage 20))
    (setf (aref goods +stone-axe+)
	  (make-instance 'melee :name "Stone axe" :yield 1 :hours 8 :tools (list +stone-knife+)
			 :armor-piercing 2 :accuracy 1 :fatality-chance 30 :damage 40))
    (setf (aref goods +bone-needles+)
	  (make-instance 'good :name "Bone needles" :yield 1 :hours 4 :tools (list +stone-knife+)))
    (setf (aref goods +stone-pot+)
	  (make-instance 'good :name "Stone pot" :yield 1 :hours 32))
    (setf (aref goods +stone-hoe+)
	  (make-instance 'good :name "Stone hoe" :yield 1 :hours 8 :tools (list +stone-knife+)))
    (setf (aref goods +basic-plow+)
	  (make-instance 'good :name "Basic plow" :yield 1 :hours 32 :effectiveness 2
			 :tools (list +stone-axe+ +stone-knife+)))
    (setf (aref goods +loom+)
	  (make-instance 'good :name "Loom set" :yield 1 :hours 64
			 :tools (list +stone-knife+ +stone-axe+)
			 :good-mats (list (list +bone-needles+ 3))))
    (setf (aref goods +thread+)
	  (make-instance 'good :name "Thread" :yield 1 :hours 8 :mats (list (list +fibers+ 1))))
    (setf (aref goods +net+)
	  (make-instance 'good :name "Net" :yield 1 :hours 4 :good-mats (list (list +thread+ 2))))
    (setf (aref goods +shoddy-textiles+)
	  (make-instance 'good :name "Shoddy textile" :yield 5 :hours 10 :tools (list +loom+)
			 :good-mats (list (list +thread+ 4))))
    (setf (aref goods +leather+)
	  (make-instance 'good :name "Leather" :yield 5 :hours 16
			 :tools (list +stone-knife+)
			 :mats (list (list +skins+ 1)))) 
    (setf (aref goods +leather-scales+)
	  (make-instance 'good :name "Leather scales" :yield 20 :hours 2
			 :tools (list +stone-pot+ +stone-knife+)
			 :good-mats (list (list +leather+ 1))))
    (setf (aref goods +lamellar+)
	  (make-instance 'armor :name "Lamellar" :yield 1 :hours 64
			 :hardness 4 :coverage 5 :weight 5
			 :tools (list +stone-pot+ +stone-knife+)
			 :good-mats (list (list +leather+ 1) (list +leather-scales+ 200 ))))))
