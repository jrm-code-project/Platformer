;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defmethod keydown ((keyhandler game) (scancode (eql :scancode-left))) nil)
(defmethod keydown ((keyhandler game) (scancode (eql :scancode-right))) nil)
(defmethod keydown ((keyhandler game) (scancode (eql :scancode-down))) nil)
(defmethod keydown ((keyhandler game) (scancode (eql :scancode-up))) nil)
(defmethod keydown ((keyhandler game) (scancode (eql :scancode-space))) nil)
(defmethod keydown ((keyhandler game) (scancode (eql :scancode-backspace))) nil)

(defmethod keydown ((keyhandler game) (scancode (eql :scancode-escape)))
  (sdl2:push-quit-event))

(defmethod keydown ((keyhandler game) (scancode (eql :scancode-x)))
  (sdl2:push-quit-event))

(defmethod mousemove ((mousehandler game) x y) nil)

(defun start-game! (game)
  (setq *the-game* game)
  (setq *shutdown* (cons 0 0))
  (setf (game-thread game) (bordeaux-threads:make-thread
                            #'(lambda ()
                                (game-loop *the-game*))
                            :name "Game Loop")))

(defun shutdown-game! (&optional game)
  (sb-ext:atomic-incf (car *shutdown*))
  (bordeaux-threads:join-thread (game-thread game)))

(defparameter *updates-per-second* 200)
(defparameter *ticks-per-second* 1000)
(defparameter *game-ticker* 0)

(defun game-loop (game)
  (setq *game-ticker* 0)
  (let* ((now (sdl2:get-ticks))
         (dticks (- now (game-last-ticks game))))
    (if (= dticks 0)
        (sdl2:delay 2)
        (progn
          (incf *game-ticker* dticks)
          (when (>= *game-ticker* 5)
            (setf (game-last-ticks game) now)
            (incf (game-count game))
            (decf *game-ticker* 5)
            (game-step! game (get-state game) dticks)))))
  (unless (> (car *shutdown*) 0)
    (game-loop game)))
