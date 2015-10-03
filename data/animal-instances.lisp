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




(defun make-animals ()
  (push-item +rabbit+ (make-instance 'animal :name "rabbit" :age-of-maturity 3 :age-of-senility 160
				     :avg-lifespan 168
				     :reproductive-capacity 50 :strength 2 :agility 5
				     :aggressiveness 1))
  (push-item +cow+ (make-instance 'domestic-animal :name "cow" :age-of-maturity 32 :age-of-senility 72
				  :avg-lifespan 180
				  :reproductive-capacity 10 :strength 30 :agility 3 :aggressiveness 3
				  :forage-required 1))
  
  (push-sentient +human+
		 (make-instance 'sentient-animal :name "human"
				:age-of-maturity 168 :age-of-senility 816
				:avg-lifespan 600
				:reproductive-capacity 15 :strength 30 :agility 40 :aggressiveness 40
				:emotional-intelligence 15 :analytical-intelligence 75
				:poisonous-chemicals (list +ammonia+ +arsenic+ +sulfides+
							   +heteropoly-m+ +heteropoly-v+)
				:nutrition-needs
				(make-array +total-nutrients+
				  :initial-contents '(50 65 300 100 100 100 100 100 100 100 5 5))))
  
  (push-sentient +naga+
		 (make-instance 'sentient-animal :name "Naga"
				:description "They are serpentine creatures with thin, webbed, arms ending in three long fingers. Their eyes and mouths are narrow slits, and they have no neck or chin."
				:reproductive-capacity 2 :strength 20 :agility 50 :aggressiveness 1
				:emotional-intelligence 100 :analytical-intelligence 75
				:poisonous-chemicals (list +arsenic+ +sulfides+
							   +heteropoly-m+ +heteropoly-v+)
				:nutrition-needs
				(make-array +total-nutrients+
					    :initial-contents '(50 50 200 100 100 100 100 100 100 100 5 5))))

  (push-sentient +gnogor+
		 (make-instance 'sentient-animal :name "Gnogor"
				:description "They are covered in loose, yellow, robes. Only large, hooved, feet are visible. They seem to have large, double jointed, legs, and switch between standing erect and walking on all fives."
				:reproductive-capacity 2 :strength 20 :agility 50 :aggressiveness 1
				:emotional-intelligence 100 :analytical-intelligence 75
				:chemical-needs (make-array +total-chemicals+
							    :initial-contents '(100 0 100 100 100 100 10 10))   
				:poisonous-nutrients (list +iron+ +zinc+ +calcium+)))


  (push-sentient +cyclosiles+
		 (make-instance 'sentient-animal :name "Cyclociles"
				:description "They look like spiders made of white crystals with yellow veins."
				:reproductive-capacity 10 :strength 50 :agility 20 :aggressiveness 50
				:emotional-intelligence 60 :analytical-intelligence 200
				:chemical-needs (make-array +total-chemicals+
				   :initial-contents '(50 100 200 0 50 0 100 500))   
				:poisonous-nutrients (list +iodine+ +fat+ +vitamin-a+)
				:nutrition-needs (make-array +total-nutrients+
				   :initial-contents '(0 0 100 0 0 80 200 0 100 0 0 0))))


  (push-sentient +hmmgmm+
		 (make-instance 'sentient-animal :name "Hmmgmm"
				:description "They look like bloated, orange, bats and communicate with a humming noises."
				:reproductive-capacity 15 :strength 50 :agility 50 :aggressiveness 100
				:emotional-intelligence 10 :analytical-intelligence 100
				:chemical-needs (make-array +total-chemicals+
				   :initial-contents '(100 0 100 0 0 0 700 0))   
				:poisonous-nutrients (list +iodine+ +fat+ +vitamin-a+)
				:nutrition-needs (make-array +total-nutrients+
				   :initial-contents '(80 100 0 100 100 0 0 0 0 0 0 0))))

  (push-sentient +torms+
		 (make-instance 'sentient-animal :name "Torms"
				:description "They are short, lumpy, reddish bipeds with long heads."
				:reproductive-capacity 15 :strength 70 :agility 120 :aggressiveness 100
				:emotional-intelligence 10 :analytical-intelligence 80
				:poisonous-nutrients (list +iodine+)
				:nutrition-needs (make-array +total-nutrients+
				    :initial-contents '(500 100 0 40 90 40 100 0 200 40 0 0)))))
