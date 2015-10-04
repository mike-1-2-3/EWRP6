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



;; Eventually, the user will input these at world gen or in a file

(defconstant +seconds-per-tick+ 20) ;; 1 min = 1 hr, for now
(defconstant +tick-processing-interval+ 1) ;;adjust to avoid freezes while you're doing stuff
(defconstant +map-size+ 100)
(defconstant +default-screen-width+ 640)
(defconstant +default-screen-height+ 480)
(defconstant +tile-size+ 50)
(defconstant +menu-width+ 210)
(defconstant +line-spacing+ 2)
(defconstant +fog-of-war+ 0) ;; = 0 on, = 1 visible, = 2 acreage known
