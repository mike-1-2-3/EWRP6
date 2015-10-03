
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



(defmacro def-constants (&body body)
  (let ((total (1- (length body)))
	(total-name (pop body))
	(derp nil)
        (subderp nil))
    (dotimes (i (length body))
      (push i subderp)
      (push (pop body) subderp)
      (push 'defconstant subderp)
      (push subderp derp)
      (setf subderp nil))
    (push total subderp)
    (push total-name subderp)
    (push 'defconstant subderp)
    (push subderp derp)
    (push 'progn derp)
    derp))

;;---------
(def-constants +total-goods+
	       +shoddy-spear+
	       +simple-bow+
	       +shoddy-shield+
	       +stone-axe+
	       +stone-knife+
	       +bone-needles+
	       +stone-pot+
	       +stone-hoe+
	       +basic-plow+
	       +loom+
	       +thread+
	       +net+
	       +shoddy-textiles+
	       +leather+
	       +leather-scales+
	       +lamellar+)


(def-constants +total-materials+
	       +fibers+
	       +skins+)

;;-------
(def-constants +job-types+
               +clear-land+
               +make-goods+
	       +explore+
	       +forage+
	       +fish+
	       +hunt+)
;;--- tiles
(def-constants +total-graphics+
;;	       +grassland1+
	       +grassland2+
;;	       +grassland3+
;;	       +grassland4+
;;	       +desert1+
	       +desert2+
	       +ocean1+
;;	       +ocean2+
	       +hill1+
	       +hill2+
	       +mountain2+
	       +mountain3+
	       +forest1+)
;;	       +woodland1+)

;;--- sentients
(def-constants +total-types-of-sentients+
	       +human+
	       +naga+
	       +gnogor+
	       +cyclosiles+
	       +hmmgmm+
	       +torms+)

;;--- factions
(def-constants +total-factions+
	       +earthlings+
	       +gliesens+
	       +nebulites+
	       +cygnites+
	       +errrkruuu+
	       +trrrmsss+
	       +ribes+)

;;--- animals
(def-constants +total-types-of-animals+
	       +rabbit+
	       +cow+)

;;--- plants
(def-constants +total-types-of-plants+
	       +blueberry-bush+
	       +brown-rice+
	       +carrots+
	       +corn+
	       +red-currant-bush+ 
	       +cranberries+
	       +kiwi+
	       +navy-beans+
	       +peanuts+
	       +potato+
	       +sesame+
	       +soybeans+
	       +spinach+
	       +wheat+)

;;--- chemicals array

(def-constants +total-chemicals+
	       +ammonia+
	       +arsenic+
	       +sulfides+
	       +heteropoly-m+
	       +heteropoly-t+
	       +heteropoly-v+
	       +methane+
	       +silicones+)
	       
;;--- nutrition array
(def-constants +total-nutrients+
	       +protein+
	       +fat+
	       +carbs+	
	       +vitamin-a+
	       +vitamin-b+
	       +vitamin-c+
	       +calcium+
	       +iodine+
	       +iron+
	       +zinc+
	       +bonus+
	       +taste+)

(defconstant +total-afflictions+ (+ +total-nutrients+ 0))
(defconstant +num-biomes+ (floor +map-size+ 10))
(defconstant +plants-per-biome+ 5)
(defconstant +random-plants+ (* +num-biomes+ +plants-per-biome+))

;;------- Type lists - ordered from best to worst
(defconstant +weapons+ (list +stone-axe+ +simple-bow+ +shoddy-spear+ +stone-knife+))
(defconstant +ranged+ (list +simple-bow+ +shoddy-spear+))
(defconstant +armor+ (list +lamellar+ +shoddy-shield+))
(defconstant +fishing-tools+ (list +net+))
;;-------
