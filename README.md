#Endangered Wildlife Reserve Planet 6

*An idle, desktop, minimal graphics, strategy game. Currently in the demo phase.*

##Requirements for binary
* SDL 1.2 and SDL image
* A semi-recent linux (briefly tested on Debian Sid and Ubuntu 14.04, other distributions will likely need to put libSDL.so, etc., where the binary can find it, until the release is packaged better)
* Download [this](https://github.com/mike-1-2-3/EWRP6/releases/download/0.5.0/EWRP6.tar.gz), decompress it, and execute the start-game file. You can delete your save/sav1 to start a new game, or save it as a different name to start a second one.

##Requirements to run source
* Same as above, in addition:
* Only tested in SBCL
* quicklisp, lispbuilder-sdl, lispbuilder-sdl-image, and cl-load

##Current features
You can...
* Create a number of goods
* Explore the world and discover new people (who eventually starve to death because the AI isn't done)
* discover new plants and grow them yourself
* Dispatch hunting and fishing parties

Feedback is greatly appreciated!