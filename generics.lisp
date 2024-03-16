;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defgeneric get-buttons (game-state))
(defgeneric get-height (object))
(defgeneric get-state (object))
(defgeneric get-width (object))
(defgeneric get-x     (object))
(defgeneric get-y     (object))

(defgeneric keydown (keyhandler scancode)
  (:method ((keyhandler t) (scancode t))
    (format *trace-output* "~&Keydown: ~s" scancode)))

(defgeneric mousemove (mousehandler x y)
  (:method ((mousehandler t) x y)
    (format *trace-output* "~&Mousemove: ~d, ~d" x y)))

(defgeneric game-step! (game game-state dticks))
(defgeneric render-game-state! (renderer texture-map game game-state))
