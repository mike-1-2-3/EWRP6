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




(defun make-afflictions ()
(push-item +protein+ (make-instance
		      'malnurishment
		      :fatality-threshold 3 :fatality-chance 100 :fatality-amount 10
		      :creature-days-per-severity 15
		      :descriptions (make-array 4 :initial-contents
'("Workers seem to be tiring more easily than usual. A few people have complained of head aches."
  "Everyone's hair looks bad for some reason. Like it's thinning. Several mothers are concerned about their children's swollen feet."
  "Everyone's muscles are melting away, most of the children have bloated stomachs. Their eyes are cast down in misery, and their parents blame you."
"The area smells of rot from the dead. Those with strength remaining are preparing to leave, looking for greener pastures."))))
		      						     
(push-item +fat+ (make-instance
		      'malnurishment
		      :fatality-threshold 3 :fatality-chance 80 :fatality-amount 5
		      :creature-days-per-severity 40
		      :descriptions (make-array 4 :initial-contents
'("Everyone misses chocolate."
"Everyone's craving food from the old world."
"People feel generally bad. Achy joints, dry hair and rashes, and feeling brain dead all the time."
"\"I'd kill for a milkshake\" a man laying on the ground says. Everyone's skin is cracked despite drinking constantly, and people feel like they're dying."))))

(push-item +carbs+ (make-instance
		      'malnurishment
		      :fatality-threshold 10 :fatality-chance 20 :fatality-amount 3
		      :creature-days-per-severity 40
		      :descriptions (make-array 4 :initial-contents
						'("You miss eating bread." "You miss eating oatmeal." "Everyone seems sluggish." "The torpor is overwhelming."))))

(push-item +vitamin-a+ (make-instance
		      'malnurishment
		      :fatality-threshold 4 :fatality-chance 50 :fatality-amount 4
		      :creature-days-per-severity 15
		      :descriptions (make-array 4 :initial-contents
'("Someone went out into the wilds last night and hasn't come back yet."
  "The nights seem black and enveloping."
  "There is obviously something wrong with people's vision."
  "A child went blind."))))

(push-item +vitamin-b+ (make-instance
		      'malnurishment
		      :fatality-threshold 3 :fatality-chance 60 :fatality-amount 8
		      :creature-days-per-severity 20
		      :descriptions (make-array 4 :initial-contents
'("This guy, George, won't shut up about how much he misses turkey."
 "Your tongue hurts and you keep forgetting what your doing."
 "Several people have mentioned that their hearts beat fast all the time, your tongue hurts badly and started swelling up, and your stomach has hurt for a week."
 "That George guy lost his marbles. Walked right into a river. Another guy died in his sleep."))))

(push-item +vitamin-c+ (make-instance
			'malnurishment
			:fatality-threshold 3 :fatality-chance 90 :fatality-amount 50
			:creature-days-per-severity 30
			:descriptions (make-array 4 :initial-contents
'("Workers seem a bit lethargic."
  "Several people have spots on their legs. Huh, you have them, too."
  "People are very concerned about their bleeding gums."
  "People are absolutely distraught about their teeth falling out."))))

(push-item +calcium+ (make-instance
		      'malnurishment
		      :fatality-threshold 4 :fatality-chance 70 :fatality-amount 5
		      :creature-days-per-severity 40
		      :descriptions (make-array 4 :initial-contents
'("Your lips are tingling."
  "Everyone seems twitchy, like their muscles just don't want to work."
  "Several people say they can't feel their hands or feet."
  "Someone started convulsing and died."))))
 
(push-item +iodine+ (make-instance
		      'malnurishment
		      :fatality-threshold 5 :fatality-chance 70 :fatality-amount 2
		      :creature-days-per-severity 80
		      :descriptions (make-array 4 :initial-contents
'("Ew. That dude's got a goiter."
  "Kids seem to be getting dumber."
  "There are several people with goiters."
  "Some of the kids are stupid. Like, REALLY stupid. One has gotten his head stuck in the same tree four times now."))))

(push-item +iron+ (make-instance
		      'malnurishment
		      :fatality-threshold 5 :fatality-chance 30 :fatality-amount 2
		      :creature-days-per-severity 80
		      :descriptions (make-array 4 :initial-contents
'("Erica says she misses eating liver. Disgusting."
  "Peterson, the farmer, has problems blacking out when he works too hard."
  "Dizzy spells are frequent."
  "Dizzyness and fatigue have become facts of life."))))

(push-item +zinc+ (make-instance
		      'malnurishment
		      :fatality-threshold 5 :fatality-chance 35 :fatality-amount 2
		      :creature-days-per-severity 80
		      :descriptions (make-array 4 :initial-contents
'("Some people have been complaining they just don't enjoy things like they used to."			 "People are just generally uglier nowadays. Thinning hair and acne abound."			
  "Vinny's mouth is all swollen and his tongue turned white. His girlfriend dumped him."
  "Dehydration, bad skin, and hair falling out, are concerning a lot of people."))))
)
