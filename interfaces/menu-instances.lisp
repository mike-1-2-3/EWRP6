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




(defun generate-goods-strings ()
  (let ((good (aref (goods-array *the-world*) (selected-item (the-menu-state *the-world*))))
	(strs nil))
    (push (concatenate 'string "One worker will make " (write-to-string (yield good))
		       ", takes " (write-to-string (hours good)) " hours.")
	  strs)
    strs))

(defun generate-surroundings-strings ()
  (with-slots (rainfall avg-summer-temp) (selected-sector)
    (let ((strs nil)
	  (temp (- (avg-summer-temp (selected-sector))
		   (* 10 (abs (- 6 (month-num *the-world*)))))))
      (incf temp (+ -10 (random 20)))
      (cond ((< rainfall 10) (push
			      "Only isolated clumps of succulents grow in the cracked earth." strs))
	    ((< rainfall 30) (push "The dry soil only supports scrublands and plains." strs))
	    ((< rainfall 50) (push "There is a fair amount of alien vegetation around." strs))
	    ((< rainfall 70) (push "The vegetation is very thick, the air is humid." strs))
	    (t (push "The air is saturated and the vegetation is incredibly dense." strs)))
      (cond ((< temp 30) (push "It's freezing." strs))
	    ((< temp 60) (push "It's quite chilly." strs))
	    ((< temp 70) (push "It's a cool day." strs))
	    ((< temp 80) (push "It's very nice outside!" strs))
	    ((< temp 100) (push "It's a bit hot." strs))
	    (t (push "The heat is oppressive." strs)))
      strs)))
      
(defun get-selected-child ()
  (with-slots (current-menu selected-item) (the-menu-state *the-world*)
    (with-slots (items) current-menu
      (if (typep items 'list)
	  (nth selected-item items)
	  (if (typep items 'array)
	      (aref items selected-item))))))

(defun generate-census-strings ()
  (let ((strs nil))
    (push (concatenate 'string "Workers available: "
		       (write-to-string (idle-workers (selected-town)))) strs)
    (dolist (population (sentients-list (selected-town)))
      (push (concatenate 'string
			 (write-to-string (quantity population)) " "
			 (name (get-population-type population)) "s.")
	    strs))
    strs))

(defun generate-basic-goods-strings ()
  (let ((strs nil))
   (dotimes (i +total-goods+)
     (push (concatenate 'string (name (aref (goods-array *the-world*) i))
			": "
			(write-to-string (aref (good-stores (selected-town)) i)))
	   strs))
   strs))

(defun generate-mats-strings ()
  (let ((strs nil))
   (dotimes (i +total-materials+)
     (push (concatenate 'string (name (aref (mats-array *the-world*) i))
			": "
			(write-to-string (aref (mat-stores (selected-town)) i)))
	   strs))
   strs))


(defun generate-food-stores-strings ()
  (let ((stores (ceiling (+ (* 7 (aref (food-stores (selected-town)) +fat+))
			    (* 4 (aref (food-stores (selected-town)) +carbs+)))
			 (* (total-workforce (selected-town)) 2000 30))))
    (cond ((> stores 0)
	   (list (concatenate 'string "We have food for about "
			      (write-to-string stores)
			      " months.")))
	  ((> stores -2)
	   (list "We are at half rations."))
	  (t (list "We are basically out of food.")))))

(defun generate-designations-strings ()
  (let ((acres 0))
    (dolist (farm (get-farms))
      (incf acres (quantity farm)))
    (list (concatenate 'string "Acres in use: " (write-to-string acres))
	  "Press enter to remove designations or view details.")))

(defun generate-acreage-strings ()
  (with-slots (arable cleared salt-water fresh-water) (selected-sector)
    (list (concatenate 'string "Arable land: " (write-to-string arable))
	  (concatenate 'string "Available cleared land: " (write-to-string cleared))
	  (concatenate 'string "Salt water: " (write-to-string salt-water))
	  (concatenate 'string "Fresh water: " (write-to-string fresh-water)))))

(defun generate-medical-strings ()
  (let ((affs (afflictions-list (settlement (selected-sector)))))
    (if (null affs)
	(let ((r (random 4)))
	  (cond ((equalp r 0) (list "Some children are playing happily."))
		((equalp r 1) (list "An old man is singing ZZ Top songs rather poorly."))
		((equalp r 2) (list "The head medic is tending his flowers."))
		((equalp r 3) (list "The head medic is putting a splint on a wounded bird."))))
	(progn
	  (let ((medical-strings nil))
	    (dolist (affliction affs)
	      (push (aref (descriptions (aref (affliction-array *the-world*) (first affliction)))
			  (second affliction))
		    medical-strings))
	    medical-strings)))))


(defun generate-farm-strings ()
  (if (null (get-farms))
      (list "No land has been designated.")
      (list "Press enter to free land."
	    (concatenate
	     'string "Farm status: "
	     (if (tended (nth (selected-item (the-menu-state *the-world*)) (get-farms)))
		 "well tended"
		 "fallow - not enough workers to tend the plants.")))))

    
;;----------------------- sector selected menu
(defun make-main-menu ()
  (let ((menu-entry
	 (make-instance 'menu :parent nil
			:name (concatenate 'string "Sector: "
					   (write-to-string (cursor-x *the-world*)) ", " 
					   (write-to-string (cursor-y *the-world*))))))
    
    (let ((submenu
	   (make-instance 'menu :parent menu-entry :name "Look around")))
      (push-item menu-entry submenu)
      (push-item submenu (make-instance 'menu :parent submenu :name "Surroundings"
					:selected-info-box #'generate-surroundings-strings))
      (push-item submenu (make-instance 'menu :parent submenu :name "Medic's hut"
					:selected-info-box #'generate-medical-strings)))

    (let ((submenu
	   (make-instance 'menu :parent menu-entry :name "Designate")))
      (push-item menu-entry submenu)
;;      (push-item submenu (make-instance 'menu :parent submenu :name "Fishing Ground"))
      (let ((subsubmenu (make-instance 'menu :parent submenu :name "Clear Land"
				       :action #'clear-acres
				       :selected-prompts 
				       (list (make-instance 'prompt-info
							    :prompt-string "How many people?"
							    :input-type 'integer
							    :trailing-string "")
					     (make-instance 'prompt-info
							    :prompt-string "How many acres?"
							    :input-type 'integer
							    :trailing-string "")))))

	(push-item submenu subsubmenu))
      
      ;;      (push-item submenu (make-instance 'menu :parent submenu :name "Clear Land"))
      (let ((subsubmenu (make-instance 'menu :parent submenu :name "Farms"
				       :items (plant-instance-array *the-world*)
				       :child-action #'add-farm)))
	(push-item subsubmenu (make-instance 'prompt-info :prompt-string "How many acres?"
					     :input-type 'integer
					     :trailing-string ""))
	(push-item submenu subsubmenu)))  
    
    (let ((submenu (make-instance 'menu :parent menu-entry :name "Inventories")))
      (push-item menu-entry submenu)
      (push-item submenu (make-instance 'menu :parent submenu :name "Census"
					:selected-info-box #'generate-census-strings))
      (push-item submenu (make-instance 'menu :parent submenu :name "Food Supplies"
					:selected-info-box #'generate-food-stores-strings))
      (push-item submenu (make-instance 'menu :parent submenu :name "Basic Goods"
					:selected-info-box #'generate-basic-goods-strings))
      (push-item submenu (make-instance 'menu :parent submenu :name "Raw Materials"
					:selected-info-box #'generate-mats-strings))
      (push-item submenu
		 (make-instance 'menu :parent submenu :name "Designations"
				:items #'get-farms
				:child-action #'remove-farm
				:child-info-box #'generate-farm-strings
				:selected-info-box #'generate-designations-strings))
      (push-item submenu (make-instance 'menu :parent submenu :name "Acreage"
					:selected-info-box #'generate-acreage-strings)))
    
    (let ((submenu (make-instance 'menu :parent menu-entry :name "Give orders")))
      (push-item menu-entry submenu)
      (let ((subsubmenu (make-instance 'menu :parent submenu :name "Forage"
				       :action #'send-foragers
				       :selected-prompts 
				       (list (make-instance 'prompt-info :prompt-string "How many?"
							    :input-type 'integer
							    :trailing-string "people")))))
	(push-item submenu subsubmenu))
      (let ((subsubmenu (make-instance 'menu :parent submenu :name "Explore"
				       :action #'send-explorers
				       :selected-prompts 
				       (list (make-instance 'prompt-info
							    :prompt-string "Destination X cor?"
							    :input-type 'integer
							    :trailing-string "")
					     (make-instance 'prompt-info
							    :prompt-string "Destination y cor?"
							    :input-type 'integer
							    :trailing-string "")
					     (make-instance 'prompt-info
							    :prompt-string "Send how many?"
							    :input-type 'integer
							    :trailing-string "")))))
	(push-item submenu subsubmenu))
      (let ((subsubmenu (make-instance 'menu :parent submenu :name "Make goods"
				       :child-info-box #'generate-goods-strings
				       :items (goods-array *the-world*)
				       :child-action #'order-goods)))
	(push-item subsubmenu (make-instance 'prompt-info :prompt-string "How many goods?"
					     :input-type 'integer
					     :trailing-string ""))
	(push-item submenu subsubmenu))
      (let ((subsubmenu (make-instance 'menu :parent submenu :name "Fish"
				       :action #'send-fishers
				       :selected-prompts 
				       (list (make-instance 'prompt-info :prompt-string "How many?"
							    :input-type 'integer
							    :trailing-string "")))))
	(push-item submenu subsubmenu))
    
      (let ((subsubmenu (make-instance 'menu :parent submenu :name "Hunt"
				       :action #'send-hunters
				       :selected-prompts 
				       (list (make-instance 'prompt-info :prompt-string "How many?"
							    :input-type 'integer
							    :trailing-string "")))))
	(push-item submenu subsubmenu)))
      
      ;;      (push-item submenu (make-instance 'menu :parent submenu :name "Hunt")))
    ;;      (push-item submenu (make-instance 'menu :parent submenu :name "Explore"))
    menu-entry))
	
