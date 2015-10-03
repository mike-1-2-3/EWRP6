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




(defun make-factions ()
  (push-item +earthlings+ (make-instance 'faction :name "Earthlings" :bellicose 6
					 :description "Your people."
					 :color1 (sdl:color :r 41 :g 73 :b 255)
					 :color2 (sdl:color :r 7 :g 33 :b 179)))
  (push-item +gliesens+ (make-instance 'faction :name "Gliesens" :bellicose 1
				       :description "They are aquatic, but have some water filled, mostly glass, buildings rising above the surface, or built on land. Their architecture is always symmetrical, and often includes beautifully carved columns, colored glass, and gems."
				       :color1 (sdl:color :r 255 :g 255 :b 255)
				       :color2 (sdl:color :r 138 :g 226 :b 255)))
  (push-item +nebulites+ (make-instance 'faction :name "Nebulites" :bellicose 7
					:description "They rarely leave massive, clay, domes. They build large statues of bizarre creatures, with expressions that would be deranged if made by humans."
					   :color1 (sdl:color :r 173 :g 78 :b 0)
					   :color2 (sdl:color :r 143 :g 64 :b 0)))
  (push-item +cygnites+ (make-instance 'faction :name "Cygnites" :bellicose 6
				       :description "It's hard to tell what they are doing. Their structures frequently behave differently than you'd expect of solid matter, and odd, mechanical, constructs, are in constant motion around their towns."
					 :color1 (sdl:color :r 244 :g 255 :b 191)
					 :color2 (sdl:color :r 186 :g 201 :b 107)))
  (push-item +errrkruuu+ (make-instance 'faction :name "Errrkruuu" :bellicose 3
					:description "They live in a single, floating, hive far above the ground, which is anchored by a thick organic cord. They have planted vast fields of plants from their home planet, and populated them with prey animals, which are their main source of food. "
					   :color1 (sdl:color :r 255 :g 215 :b 38)
					   :color2 (sdl:color :r 255 :g 177 :b 8)))
  (push-item +trrrmsss+ (make-instance 'faction :name "Trrrmsss" :bellicose 20
				       :description "They live in a single, floating, hive far above the ground, which is anchored by a thick organic cord. They hunt far and wide. Including us! We barely escaped."
					   :color1 (sdl:color :r 219 :g 26 :b 0)
					   :color2 (sdl:color :r 150 :g 18 :b 0)))
  (push-item +ribes+ (make-instance 'faction :name "Ribes" :bellicose 10
				    :description "They live in huts, and are constantly fighting each other. When one of them dies, they throw it in a fire without any ceremony. They herd creatures that look like obese, headless, pigs, which they impale and eat raw, and alive."
					   :color1 (sdl:color :r 106 :g 133 :b 105)
					   :color2 (sdl:color :r 120 :g 112 :b 89))))

