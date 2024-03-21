;;; -*- Lisp -*-

(in-package "PLATFORMER")

;;; Configuration parameters for the game.  Adjust these as necessary.

(defun game-over-font ()
  "Returns the pathname of a True-Type font used for the Game Over screen."
  ;; "/mnt/c/Windows/Fonts/Inconsolata-Regular.ttf"
  "/mnt/c/Windows/Fonts/arial.ttf"
  ;; "/mnt/c/Windows/Fonts/comic.ttf"
  )

(defun project-directory ()
  "Returns the directory the source code is installed in."
  #p"/mnt/c/Users/JosephMarshallC/source/repos/Platformer/")
