#EWRP 6

A silly idle game I started a couple years ago. Not under active development, or worth playing.

##Requirements for binary
* SDL 1.2 and SDL image
* A semi-recent linux (briefly tested on Debian Sid and Ubuntu 14.04. protip: use ldd start-game to see if your system can find SDL)
* Download [this](https://github.com/mike-1-2-3/EWRP6/releases/download/0.5.1/EWRP6.tar.gz), decompress it, and execute the start-game file. You can make the game faster, reveal all tiles in a new game, and change the save file in the config.txt file.

##Requirements to run source
* SDL 1.2 and SDL image
* Only tested in SBCL
* quicklisp, lispbuilder-sdl, lispbuilder-sdl-image, and cl-load
* to run the game, enter the directory, launch lisp, and run: (progn (load "startup.lisp") (load-all) (start-game))

##Current features
You can...
* Create a number of goods, weapons, and armor
* Explore the world and discover new people (who eventually starve to death because the AI isn't done)
* Discover new plants and grow them yourself, or choose a crop from Earth that is suited for the environment
* Dispatch hunting and fishing parties

![screenshot](/screenshot.png?raw=true)