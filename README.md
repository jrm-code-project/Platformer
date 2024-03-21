# Platformer
A simple platformer game in Common Lisp

This game is a Common Lisp clone of the [Java Platformer
Tutorial](https://www.kaaringaming.com/platformer-tutorial) by [Kaarin
Gaming](https://kaaringaming.com).  The pixel art is from [Pixel Frog](http://pixelfrog-assets.itch.io/).

To install and run, you must adjust the pathnames in the file
"config.lisp" so that the game can find the "Game Over" font and the
resource files.

Run `(main)` in the "PLATFORMER" package to bring up the main menu.
Select "Play" to play the levels.  "Options" is unimplemented and
simply ends the game.  "Quit" is implemented and simply ends the game.
When playing, arrow keys move you left and right.  Space bar jumps.
Left mouse attacks.  Backspace opens a (useless) pause menu.  Each
level ends when you kill all the enemies on the level, and the game
ends when the last level finishes.  Pressing "Esc" or "x" at any time
exits the game.

The game was created by following the Java Platformer Tutorial up to
about episode 22.  There is plenty left to do if you wish.  The sound
libraries I looked at introduced a noticeable delay before playing
.wav files, so sound effects were a non-starter.  Perhaps you can find
a better sound library.  Power attacks are unimplemented.  Other
enemies are unimplemented.

The code is a mixed bag of both embarrassing hastily-written code and
well-designed Lisp.  The code that builds the initial levels is a bit
convoluted, and the code that implements the menu buttons is obscure.
There are needed abstractions that could greatly simplify the code.

When running the game, the main thread will be rendering the graphics
at about 70 frames per second, and a background thread will be
running the game logic at about 200 frames per second.  If an error
happens, things can get confusing.  It is usually best to try to
continue the main SDL loop.  It is typically able to clean up and exit
in error situations.


