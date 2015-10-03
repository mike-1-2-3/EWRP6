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


(defun generate-random-plant ()
  (let ((part1 (random 12))
	(part2 (random 12))
	(part3 (random 12))
	(new-plant (make-instance 'native-plant :name nil :description nil
				  :growth-start-date (list 4 1) :fruiting-start-date (list 6 20)
				  :dormancy-start-date (list 9 1) :rainfall-desired 30 
				  :harvest-difficulty 6 :product-list
				  (list (make-instance 'native-food :name nil :yield 500)))))

    (with-slots (name description product-list) new-plant
      (with-slots (nutrients chemicals) (first product-list)
;;---------------------------------------------------------- stump string
	(cond ((equalp part1 0) (progn
				  (push " a thin, branching, crusty and yellow trunk." description)
				  (setf (aref nutrients +iodine+) 200)
				  (setf (aref chemicals +silicones+) 200)))
	       ((equalp part1 1) (progn
				  (push " a brown, coral like, base." description)
				  (setf (aref nutrients +calcium+) 200)
				  (setf (aref chemicals +methane+) 200)))
	       ((equalp part1 2) (progn
				  (push " a spherical, juicy, orange thing." description)
				  (setf (aref nutrients +iron+) 300)
				  (setf (aref chemicals +methane+) 200)))
	       ((equalp part1 3) (progn
				  (push " a helical, flowering, vine." description)
				  (setf (aref chemicals +heteropoly-m+) 300)
				  (setf (aref chemicals +heteropoly-t+) 300)))
	       ((equalp part1 4) (progn
				  (push " a crystalline stalk." description)
				  (setf (aref nutrients +calcium+) 50)
				  (setf (aref chemicals +heteropoly-v+) 300)))
	       ((equalp part1 5) (progn
				   (push " a fleshy mound that smells like ammonia." description)
				   (setf (aref chemicals +ammonia+) 400)
				   (setf (aref chemicals +arsenic+) 400)))
	       ((equalp part1 6) (progn
				   (push " a gnarled mass, with symbiotic guardian insects."
					 description)
				   (setf (aref nutrients +vitamin-a+) 200)
				   (setf (aref nutrients +iron+) 100)
				   (setf (aref nutrients +zinc+) 100)
				   (setf (aref nutrients +bonus+) 3)
				   (setf (aref nutrients +taste+) 2)))
	       ((equalp part1 7) (progn
				   (push " a scintillating, fractal, trunk." description)
				   (setf (aref nutrients +vitamin-b+) 1000)
				   (setf (aref chemicals +arsenic+) 1000)))
	       ((equalp part1 8) (progn
				   (push " a cluster of smelly, yellow, partially transparent vesicles." description)
				   (setf (aref nutrients +fat+) 100)
				   (setf (aref chemicals +sulfides+) 400)))
	       ((equalp part1 9) (progn
				   (push " a dense, blue, shrub." description)
				   (setf (aref nutrients +taste+) 5)
				   (setf (aref nutrients +bonus+) 5)				   
				   (setf (aref nutrients +protein+) 100)
				   (setf (aref nutrients +iodine+) 100)
				   (setf (aref nutrients +vitamin-b+) 100)))
	       ((equalp part1 10) (progn
				   (push " a rocky, honeycombed, mound." description)
				   (setf (aref nutrients +calcium+) 1000)
				   (setf (aref nutrients +fat+) 50)))
	       ((equalp part1 11) (progn
				   (push " a giant, partially submerged, moss ball." description)
				   (setf (aref nutrients +vitamin-b+) 500)
				   (setf (aref chemicals +vitamin-c+) 500))))

	
;;---------------------------------------------- branch string	
	(cond ((equalp part2 0) (progn
				  (push "something with floating, red, sacks anchored to" description)
				  (incf (aref chemicals +heteropoly-t+) 150)
				  (incf (aref chemicals +methane+) 150)))
	      ((equalp part2 1) (progn
				  (push "a veiny web of tendrils boring into" description)
				   (incf (aref nutrients +protein+) 50)
				   (incf (aref nutrients +iron+) 200)))
	      ((equalp part2 2) (progn
				  (push "a thing with huge, stacking, blue, disks rising from" description)
				  (incf (aref nutrients +carbs+) 100)
				  (incf (aref nutrients +bonus+) 8)))
	      ((equalp part2 3) (progn
				  (push "a sheet of enveloping, red, mucus emanating from" description)
				  (incf (aref chemicals +arsenic+) 200)
				   (incf (aref chemicals +sulfides+) 200)))
	      ((equalp part2 4) (progn
				  (push "this thing that had tons of spikes coming out of" description)
				  (incf (aref nutrients +zinc+) 200)
				  (incf (aref nutrients +vitamin-a+) 200)))
	      ((equalp part2 5) (progn
				  (push "a mind boggling blue filigree of symmetrical branches from" description)
				  (incf (aref nutrients +calcium+) 250)
				  (incf (aref chemicals +silicones+) 250)))
	      ((equalp part2 6) (progn
				  (push "pastel colored flowers on" description)
				   (incf (aref nutrients +vitamin-a+) 100)
				   (incf (aref nutrients +vitamin-c+) 100)))
	       ((equalp part2 7) (progn
				   (push "thick, greasy, tubers descending from" description)
				   (incf (aref nutrients +fat+) 100)
				   (incf (aref chemicals +carbs+) 100)))
	       ((equalp part2 8) (progn
				   (push "putrid threads hanging off" description)
				   (incf (aref chemicals +ammonia+) 200)
				   (incf (aref chemicals +heteropoly-v+) 200)))
	       ((equalp part2 9) (progn
				   (push "spindly, jointed, branches protruding from" description)
				   (incf (aref nutrients +protein+) 100)))
	       ((equalp part2 10) (progn
				    (push "something like a bromeliad, but jagged, serrated, and rotating around" description)
				    (incf (aref nutrients +carbs+) 400)
				    (incf (aref nutrients +protein+) 200)))
	       ((equalp part2 11) (progn
				    (push "bulbous green pads attached to" description)
				   (incf (aref chemicals +heteropoly-m+) 200)
				   (incf (aref chemicals +sulfides+) 100))))
	;; ------------------------------- fruit descriptions
	
	(cond ((equalp part3 0) (progn
				   (setf (description (first product-list)) "leathery star shaped flesh")
				   (incf (aref nutrients +carbs+) 100)
				   (incf (aref chemicals +silicones+) 100)
				   (incf (aref nutrients +vitamin-b+) 100)))
	      ((equalp part3 1) (progn
				   (setf (description (first product-list)) "glowing fruit")
				   (incf (aref chemicals +sulfides+) 300)
				   (incf (aref chemicals +heteropoly-v+) 200)))
	      ((equalp part3 2) (progn
				  (setf (description (first product-list)) "golden, gritty, berries")
				  (incf (aref nutrients +vitamin-c+) 800)
				  (incf (aref chemicals +silicones+) 100)))
	      ((equalp part3 3) (progn
				   (setf (description (first product-list)) "fibrous shoots" )
				   (incf (aref nutrients +bonus+) 5)
				   (incf (aref nutrients +zinc+) 500)))
	      ((equalp part3 4) (progn
				   (setf (description (first product-list)) "things that look just like a human kidney" )
				   (incf (aref nutrients +protein+) 100)
				   (incf (aref nutrients +iron+) 300)))
	      ((equalp part3 5) (progn
				   (setf (description (first product-list)) "spongy nodules embedded in white amber")
				   (incf (aref nutrients +protein+) 50)
				   (incf (aref chemicals +methane+) 100)
				   (incf (aref nutrients +calcium+) 600)))
	      ((equalp part3 6) (progn
				   (setf (description (first product-list)) "glazed, blue, pads")
				   (incf (aref nutrients +vitamin-a+) 300)
				   (incf (aref nutrients +taste+) 5)))
	      ((equalp part3 7) (progn
				   (setf (description (first product-list)) "black pellets covered in white powder")
				   (incf (aref nutrients +zinc+) 200)
				   (incf (aref chemicals +arsenic+) 200)))
	      ((equalp part3 8) (progn
				   (setf (description (first product-list)) "barbed melons" )
				   (incf (aref nutrients +carbs+) 600)
				   (incf (aref nutrients +bonus+) 3)))
	      ((equalp part3 9) (progn
				  (setf (description (first product-list)) "hairy buds" )
				  (incf (aref nutrients +taste+) 6)
				  (incf (aref nutrients +bonus+) 6)))
	      ((equalp part3 10) (progn
				  (setf (description (first product-list)) "gilled cones" )
				  (incf (aref nutrients +fat+) 100)
				  (incf (aref nutrients +vitamin-b+) 400)))
	      ((equalp part3 11) (progn
				  (setf (description (first product-list)) "green fruit with woody, white, ribs")
				  (incf (aref nutrients +fat+) 50)
				  (incf (aref nutrients +iodine+) 800))))
	;;---------- add a random nutrient
	(let ((r (random 3))
	      (i (random 8)))
	  (if (equalp r 0)
	      (incf (aref nutrients i) 100)
	      (if (equalp r 1)
		  (incf (aref chemicals i) 100))))
	new-plant))))
