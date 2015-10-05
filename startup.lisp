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

;; Version 0.5 - Demo

(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-image")
(ql:quickload "cl-store")  

(defun setup-world ()
  (if (equalp nil (probe-file *save-directory*))
      (progn
	(draw-loading-item "Generating world..." 10 30)
	(populate-world))
      (progn (setf *the-world* (cl-store:restore *save-directory*))
	     (message "Welcome back!"))))

;;-------------------------------- some game functions

(defun cleanup ()
  (unschedule-timer (car (list-all-timers))))

(defun save ()
  (message "Saving...")
  (setf (last-played *the-world*) (get-universal-time)
	(the-menu-state *the-world*) nil
	(the-sector-state *the-world*) nil
	(the-input-state *the-world*) nil
	(current-game-state *the-world*) nil)
  (cl-store::store *the-world* *save-directory*)
  (gc)
  #-allegro (quit) #+allegro (exit))


(defun load-all ()

  (load "index-constants.lisp")
  (load "interfaces/sdl-interface.lisp")
  (load "generics.lisp" :verbose nil)
  ;; load various classes
  (load "interfaces/menu.lisp")
  (load "data/plants.lisp" :verbose nil)
  (load "data/animals.lisp" :verbose nil)
  (load "data/sentients.lisp" :verbose nil)
  (load "data/populations.lisp" :verbose nil)
  (load "data/affliction.lisp")
  (load "data/world.lisp" :verbose nil)
  (load "data/goods.lisp")
  ;; load the rest of the code
  (load "jobs.lisp" :verbose nil) 
  (load "scheduling.lisp" :verbose nil)
  
  (load "data/plants-instances.lisp")
  (load "data/animal-instances.lisp")
  (load "data/faction-instances.lisp")
  (load "data/affliction-instances.lisp")
  (load "data/random-plants.lisp")
  (load "data/goods-instances.lisp")
  (load "data/raw-mats-instances.lisp")
  (load "world-gen.lisp")
  (load "interfaces/menu-instances.lisp")
  (load "interfaces/states.lisp"))

(defun make-bin ()
  (load-all)
  (save-lisp-and-die "EWRP6" :toplevel #'start-game :executable t))

(defun read-config ()
  (let ((config (open "config.txt")))
    (defparameter *seconds-per-tick* (parse-integer (read-line config)))
    (defparameter *fog-of-war* (parse-integer (read-line config)))
    (defparameter *save-directory* (read-line config))
    (close config)))

(defun start-game ()
  (cffi:define-foreign-library sdl
    (:darwin (:or (:framework "SDL")
		  (:default "libSDL")))
    (:windows "SDL.dll")
    (:unix (:or "libSDL-1.2.so.0.7.2"
		"libSDL-1.2.so.0"
		"libSDL-1.2.so"
		"libSDL.so"
		"libSDL")))
  (read-config)
  (defparameter *the-world* nil)
  (defparameter *title-font* (sdl:initialise-font sdl:*font-10x20*))
  (defparameter *font* (sdl:initialise-font sdl:*font-9x18B*))
  (defparameter *char-height* (sdl:char-height *font*))
  (defparameter *char-width* (sdl:char-width *font*))
  (defparameter *screenw* +default-screen-width+)
  (defparameter *screenh* +default-screen-height+)
  
  (setf *random-state* (make-random-state t))
  ;; start the display
  (sdl:init-image :png)
  (sdl:with-init ()
    (sdl:window *screenw* *screenh* :title-caption "Endangered Wildlife Reserve Planet 6")
    (sdl:enable-unicode)
    (setf (sdl:frame-rate) 15)    
    (draw-borders)
    (draw-loading-item "Loading game..." 10 10)
    
    ;;start game backend
    (setup-world)
    (draw-date)
    
    (setf (the-menu-state *the-world*) (make-instance 'menu-game-state)
	  (the-sector-state *the-world*) (make-instance 'sector-selection-state)
	  (the-input-state *the-world*) (make-instance 'input-game-state)
	  (current-game-state *the-world*) (the-sector-state *the-world*))
    (teleport (cursor-y *the-world*) (cursor-x *the-world*))
    
    (setf (root-menu (the-menu-state *the-world*)) (make-main-menu))
    (setf (current-menu (the-menu-state *the-world*)) (root-menu (the-menu-state *the-world*)))
    
    (load-graphics (the-sector-state *the-world*))
    (draw (the-sector-state *the-world*))
    (draw-sector-border)
    (check-alerts)
    (sdl:update-display)
    (process-ticks-on-load)
    (setf (mutex *the-world*) t)
    (launch-timer)
    
    ;; game loop
    (with-slots (current-game-state) *the-world*
      (sdl:with-events (:wait)
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))	
	(:key-down-event
	   (:key key :unicode unicode)
	   (when (mutex *the-world*)
	     (setf (mutex *the-world*) nil)
	     (process-key-event current-game-state key unicode)
	     (draw current-game-state)
	     (sdl:update-display)
	     (setf (mutex *the-world*) t)))))))

