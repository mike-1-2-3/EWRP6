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




(defun check-tiles (x y accepted)
  (let ((ret nil))
    (dolist (tile accepted)
      (if (equalp tile (tile-id (get-sector x y)))
	  (progn
	    (setf ret t)
	    (return))))
    ret))

(defun place-alien (fact species accepted-tiles)
  (let ((x 0)
	(y 0))
    (loop
       (setf x (random +map-size+))
       (setf y (random +map-size+))
       (if (and (check-tiles x y accepted-tiles)
		(null (settlement (get-sector x y))))
	   (return)))
    (let ((capital (make-instance 'aligned-sector :underlying-sector (get-sector x y)
				  :controller-index fact)))
      (setf (settlement (get-sector x y)) capital)
      (push-item capital
		 (make-instance 'sentient-population :quantity 100 :species-index species))
      (push-item (get-faction fact) capital)
      (setf (idle-workers capital) 100)
      (update-populations capital)
      (setf (monthly-workers capital) 0))))    
  
(defun place-player (x y fact)
  (let ((capital (make-instance 'aligned-sector :underlying-sector (get-sector x y)
				:controller-index fact)))
    (setf (settlement (get-sector x y)) capital)
    (push-item capital
	       (make-instance 'livestock-population :quantity 10 :species-index +cow+))
    (push-item capital
	       (make-instance 'sentient-population :quantity 100 :species-index +human+))
    (push-item (get-faction fact) capital)
    (setf (idle-workers capital) 100)
    (update-populations capital)
    (setf (monthly-workers capital) 0)
    (setf (aref (intel (get-faction fact)) fact) 1)
    (explore x y)))

(defun seed-wildlife ()
  (dotimes (x 50)
    (push-item (get-sector x 1)
	       (make-instance 'animal-population :quantity 1000 :species-index 0))))

(defun set-ocean (x y)
  (with-slots (tile-id rainfall salt-water fresh-water cleared arable) (get-sector x y)
    (incf rainfall 10)
    (setf tile-id +ocean1+
	  cleared 0
	  arable 0
	  fresh-water 0
	  salt-water 10000)))
    
(defun set-forest (x y)
  (with-slots (tile-id rainfall cleared arable fresh-water) (get-sector x y)
    (incf arable (+ 200 (random 1000)))
    (incf rainfall 5)
    (incf fresh-water 3)
    (setf tile-id +forest1+
	  cleared (random 20))))

(defun make-ocean ()
  (let ((x (- 110 (random 120)))
	(y (- 110 (random 120)))
	(h (random 10))
	(newx1 0)
	(newx2 0))
    (dotimes (newy h)
      (decf newx1 (random 3))
      (incf newx2 (random 3))
      (dotimes (curx (+ (* -1 newx1) newx2))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y) 0) (<= (+ newy y) 99))
	    (set-ocean (+ x curx newx1) (+ newy y))) +ocean1+))
    (incf y h)
    (dotimes (newy h)
      (incf newx1 (random 3))
      (decf newx2 (random 3))
      (dotimes (curx (+ (* -1 newx1) newx2))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y) 0) (<= (+ newy y) 99))
	    (set-ocean (+ x curx newx1) (+ newy y))) +ocean1+))))
;;	    (setf (tile-id (get-sector (+ x curx newx1) (+ newy y))) +ocean1+))))))


(defun make-forest ()
  (let ((x (- 110 (random 120)))
	(y (- 110 (random 120)))
	(h (random 8))
	(newx1 0)
	(newx2 0))
    (dotimes (newy h)
      (decf newx1 (random 3))
      (incf newx2 (random 3))
      (dotimes (curx (+ (* -1 newx1) newx2))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y) 0) (<= (+ newy y) 99))
	    (set-forest (+ x curx newx1) (+ newy y)))))
    (incf y h)
    (dotimes (newy h)
      (incf newx1 (random 3))
      (decf newx2 (random 3))
      (dotimes (curx (+ (* -1 newx1) newx2))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y) 0) (<= (+ newy y) 99))
	    (set-forest (+ x curx newx1) (+ newy y)))))
    (incf y h)
    (dotimes (newy h)
      (decf newx1 (random 3))
      (incf newx2 (random 3))
      (dotimes (curx (+ (* -1 newx1) newx2))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y) 0) (<= (+ newy y) 99))
	    (set-forest (+ x curx newx1) (+ newy y)))))
    (incf y h)
    (dotimes (newy h)
      (incf newx1 (random 3))
      (decf newx2 (random 3))
      (dotimes (curx (+ (* -1 newx1) newx2))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y) 0) (<= (+ newy y) 99))
	    (set-forest (+ x curx newx1) (+ newy y)))))))

(defun set-hill (x y tile)
  (with-slots (rainfall avg-summer-temp tile-id fresh-water cleared arable) (get-sector x y)
    (setf tile-id tile
	  cleared (random 20)
	  arable (random 1000))
    (incf rainfall 10)
    (decf avg-summer-temp 10)
    (incf fresh-water 10)))

(defun set-mountain (x y tile)
  (with-slots (avg-summer-temp tile-id cleared arable) (get-sector x y)
    (decf avg-summer-temp 25)
    (setf tile-id tile
	  cleared (random 10)
	  arable (random 5))))

(defun make-mountain ()
  (let ((x (- 110 (random 120)))
	(y (- 110 (random 120)))
	(h (random 20))
	(hill (random 2))
	(mountain (random 2))
	(l 0)
	(newx1 0)
	(dir (random 2)))
    (if (equalp hill 0)
	(setf hill +hill1+)
	(setf hill +hill2+))
    (if (equalp mountain 0)
	(setf mountain +mountain2+)
	(setf mountain +mountain3+))
    (dotimes (newy h)
      (setf l (random 6))
      (if (equalp dir 0)
	  (incf newx1 l)
	  (decf newx1 l))
      (dotimes (curx l)
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y -1) 0) (<= (+ newy y -1) 99))
	    (set-hill (+ x curx newx1) (+ newy y -1) hill))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y) 0) (<= (+ newy y) 99))
	    (set-mountain (+ x curx newx1) (+ newy y) mountain))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y 1) 0) (<= (+ newy y 1) 99))
	    (set-mountain (+ x curx newx1) (+ newy y 1) mountain))
	(if (and (>= (+ x curx newx1) 0) (<= (+ x curx newx1) 99)
		 (>= (+ newy y 2) 0) (<= (+ newy y 2) 99))
	    (set-hill (+ x curx newx1) (+ newy y 2) hill))))))

(defun make-desert ()
  (let ((x 0)
	(w 0)
	(curw 0)
	(d (random 7))
	(extrusions 10)
	(h (+ 4 (random 1))))
    (dotimes (y h)
      (dotimes (x +map-size+)
	(setf (tile-id (get-sector x (- +map-size+ (1+ y)))) +desert2+)))
    (dotimes (i extrusions)
      (setf x (- (+ +map-size+ 10) (random (+ 20 +map-size+))))
      (setf w (+ 8 (floor (random +map-size+) extrusions)))
      (setf d (random 5))
      (dotimes (depth d)
	(dotimes (curx w)
	  (if (and (>= (+ curw x curx) 0) (< (+ curw x curx) +map-size+))
	      (setf (tile-id (get-sector (+ curw x curx) (- +map-size+ (+ 1 depth h)))) +desert2+)))
	(setf w (+ (floor w 2) (random (1+ (floor w 4)))))
	(setf curw (- w (floor w 4)))))))

(defun set-name (n)
  (setf (player-name *the-world*) n))

(defun populate-world ()
  (setf *the-world* (make-instance 'world))

  (make-raw-mats)
  (make-plants)
  (make-animals)
  (make-factions)
  (make-afflictions)
  (make-goods)
  
  (dotimes (x +num-biomes+)
    (dotimes (y +plants-per-biome+)
      (setf (aref (native-plant-array *the-world*) x y) (generate-random-plant))))
  
  (dotimes (x +map-size+)
    (dotimes (y +map-size+)
      (let* ((heat (random 10))
	     (rain (+ heat 5 (abs (- (round (* 2.6 x)) 130))))
	     (temp (+ 50 heat (floor y 1.5))))
	(setf (aref (world-grid *the-world*) x y)
	      (make-instance
	       'sector
	       :arable (+ 1000 (random 3000)) :cleared (+ 100 (random 200))
	       :salt-water 0 :fresh-water (floor (random rain) 2) :tile-id +grassland2+ :rainfall rain
	       :avg-summer-temp temp)))))
  (dotimes (i 140)
    (make-forest))
  (make-desert)
  (dotimes (i 20)
    (make-mountain))
  (dotimes (i 20)
    (make-ocean))
  (seed-wildlife)
  (let ((x 0)
	(y 0))
    (loop
       (setf x (random +map-size+))
       (setf y (random +map-size+))
       (if (and (not (equalp (tile-id (get-sector x y)) +mountain2+))
		(not (equalp (tile-id (get-sector x y)) +mountain3+))
		(not (equalp (tile-id (get-sector x y)) +ocean1+)))
	   (return)))
    (setf (cursor-x *the-world*) x)
    (setf (cursor-y *the-world*) y)
    (place-player x y +earthlings+)

    (place-alien +gliesens+ +naga+ (list +ocean1+))
    (place-alien +nebulites+ +gnogor+ (list +forest1+))
    (place-alien +cygnites+ +cyclosiles+ (list +mountain2+ +mountain3+ +hill1+ +hill2+))
    (place-alien +errrkruuu+ +hmmgmm+ (list +grassland2+ +hill1+ +hill2+))
    (place-alien +trrrmsss+ +hmmgmm+ (list +grassland2+ +hill1+ +hill2+))
    (place-alien +ribes+ +torms+ (list +grassland2+ +forest1+ +hill1+ +hill2+))
			      
    
    (let ((intro-str nil))
      (push "You wake up in a totally alien environment. Taped to your forehead is a note that reads:" intro-str)
      (push "\"Dear human - your stupid species wiped itself out with nukes. Our board of conservation has decided to transfer some of you to Endangered Wildlife Reserve Planet 6. If you mess up this planet, or can't get along with the other endangered species, you'll be moved to our museum of taxidermy.\"" intro-str)
      (push "There are several pallets of supplies and dozens of people around. You decide to go introduce yourself." intro-str)
      (push "----Tips----" intro-str)
      (push "-You are in the blue tile. Select it." intro-str)
      (push "-The escape key backs out of menus." intro-str)
      (push "-Time will pass whether you are playing the game or not." intro-str)
      (push "-Have farms and idle workers by April." intro-str)

      (push (make-instance 'alert :strings intro-str :title "Welcome to EWRP6" :result-func #'set-name :prompts
			   (list (make-instance 'prompt-info :prompt-string "Enter name:"
						:input-type 'string :trailing-string "")))
	    (alert-queue *the-world*)))))



