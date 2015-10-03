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



(defun make-plants ()
  (push-item +blueberry-bush+
	     (make-instance 'bush :name "blueberry bush"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 6 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 35 :maturity-time 4
			    :temp-desired 80
			    :harvest-difficulty 6 :product-list
			    (list
			     (make-instance
			      'food :name "blueberries" :yield 500 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(3 1 65 5 10 72 2 0 9 0 2 8))))))
  
  
  (push-item +red-currant-bush+
	     (make-instance 'bush :name "red currants"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 35 :maturity-time 4
			    :temp-desired 80
			    :harvest-difficulty 7 :product-list
			    (list
			     (make-instance
			      'food :name "red currant berries" :yield 2000 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(6 1 64 4 10 304 14 0 27 11 2 3))))))
  
  (push-item +cranberries+
	     (make-instance 'bush :name "cranberries"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 70 :maturity-time 4
			    :temp-desired 80
			    :harvest-difficulty 6 :product-list
			    (list
			     (make-instance
			      'food :name "cranberries" :yield 1000 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(0 0 54 20 16 96 3 1068 9 0 1 2))))))
  
  (push-item +brown-rice+
	     (make-instance 'plant :name "brown rice"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 110
			    :temp-desired 90
			    :harvest-difficulty 7 :product-list
			    (list
			     (make-instance
			      'food :name "rice grain" :yield 1000 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(34 13 341 0 20 0 20 0 48 64 0 3))))))
  
  (push-item +carrots+
	     (make-instance 'plant :name "carrots"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 50
			    :temp-desired 85
			    :harvest-difficulty 3 :product-list
			    (list
			     (make-instance
			      'food :name "carrots" :yield 5000 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(8 0 37 927 5 16 15 0 16 0 2 1))))))
  
  (push-item +corn+
	     (make-instance 'plant :name "corn"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 50
			    :temp-desired 90
			    :harvest-difficulty 1 :product-list
			    (list
			     (make-instance
			      'food :name "corn" :yield 1500 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(15 3 101 0 2 32 0 0 16 16 0 4))))))
  
  (push-item +kiwi+
	     (make-instance 'bush :name "kiwi"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 75
			    :temp-desired 80
			    :harvest-difficulty 7 :maturity-time 4 :product-list
			    (list
			     (make-instance
			      'food :name "kiwis" :yield 1500 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(5 1 64 8 2 688 16 0 0 0 1 8))))))
  
  (push-item +navy-beans+
	     (make-instance 'plant :name "navy beans"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 60
			    :temp-desired 80
			    :harvest-difficulty 6 :product-list
			    (list
			     (make-instance
			      'food :name "navy beans" :yield 460 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(101 7 275 0 300 0 66 0 138 103 2 1))))))
  
  (push-item +peanuts+
	     (make-instance 'plant :name "peanuts"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 33
			    :temp-desired 100
			    :harvest-difficulty 6 :product-list
			    (list
			     (make-instance
			      'food :name "peanuts" :yield 800 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(117 223 73 0 100 0 42 0 120 100 1 5))))))
  
  (push-item +potato+
	     (make-instance 'plant :name "potato"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 50
			    :temp-desired 70
			    :harvest-difficulty 6 :product-list
			    (list
			     (make-instance
			      'food :name "potatoes" :yield 3000 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(10 0 79 0 5 150 5 82 19 9 0 3))))))
  
  (push-item +sesame+
	     (make-instance 'plant :name "sesame"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 120
			    :temp-desired 100
			    :harvest-difficulty 7 :product-list
			    (list
			     (make-instance
			      'food :name "sesame seeds" :yield 200 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(80 225 106 0 150 0 440 0 366 234 4 4))))))
  (push-item +soybeans+
	     (make-instance 'plant :name "soybeans"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 40
			    :temp-desired 80
			    :harvest-difficulty 6 :product-list
			    (list
			     (make-instance
			      'food :name "soybeans" :yield 500  :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(59 31 50 17 20 218 89 0 88 29 2 2))))))
  
  (push-item +spinach+
	     (make-instance 'plant :name "spinach"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 70
			    :temp-desired 75
			    :harvest-difficulty 1 :product-list
			    (list
			     (make-instance
			      'food :name "spinach" :yield 2200 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(13 2 16 849 20 211 45 0 68 16 5 1))))))
  
  (push-item +wheat+
	     (make-instance 'plant :name "wheat"
			    :growth-start-date (list 4 1) :fruiting-start-date (list 7 20)
			    :dormancy-start-date (list 9 1) :rainfall-desired 30
			    :temp-desired 70
			    :harvest-difficulty 6 :product-list
			    (list
			     (make-instance
			      'food :name "wheat" :yield 600 :nutrients
			      (make-array +total-nutrients+ :initial-contents
					  '(60 11 326 0 50 0 15 0 89 80 4 5)))))))
