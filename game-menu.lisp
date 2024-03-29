;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defun menu-atmosphere-base-width () 832)
(defun menu-atmosphere-base-height () 448)
(defun-scaled menu-background-width 282)
(defun-scaled menu-background-height 336)
(defun menu-background-x () (floor (- (/ (game-width) 2) (/ (menu-background-width) 2))))
(defun-scaled menu-background-y 45)
(defun-scaled menu-button-width 140)
(defun-scaled menu-button-height 56)
(defun-scaled play-button-position 150)
(defun-scaled options-button-position 220)
(defun-scaled quit-button-position 290)

(defclass menu ()
  ((buttons :initarg :buttons :reader get-buttons)))

(defun menu-button-under-mouse? (button mouse-x mouse-y)
  (let ((button-left (- (/ (game-width) 2) (/ (menu-button-width) 2)))
        (button-right (+ (/ (game-width) 2) (/ (menu-button-width) 2)))
        (button-top (get-position button))
        (button-bottom (+ (get-position button) (menu-button-height))))
    (and (< button-left mouse-x button-right)
         (< button-top mouse-y button-bottom))))

(defmethod game-step! (game (menu menu) dticks)
  (declare (ignore dticks))
  (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
    (dolist (menu-button (get-buttons menu))
      (cond ((not (menu-button-under-mouse? menu-button mouse-x mouse-y))
             (setf (get-column menu-button) 0))
            ((logbitp 0 mouse-buttons)
             (ecase (get-column menu-button)
               (0 nil)
               (1 (setf (get-column menu-button) 2))
               (2 nil)))
            (t (ecase (get-column menu-button)
                 (0 (setf (get-column menu-button) 1))
                 (1 nil)
                 (2 (ecase (get-row menu-button)
                      (0 (setf (get-state game) (get-level game)))
                      (1
                       (format t "~&Options menu unimplemented, quitting game.~%")
                       (force-output)
                       (sdl2:push-quit-event))
                      (2 (sdl2:push-quit-event))))))))))

(defun render-menu-background! (renderer menu-background-texture)
  (sdl2:with-rects ((src 0 0 (base-menu-background-width) (base-menu-background-height))
                    (dst (menu-background-x)
                         (menu-background-y)
                         (menu-background-width)
                         (menu-background-height)))
    (sdl2:render-copy renderer menu-background-texture :source-rect src :dest-rect dst)))

(defun render-menu-button! (renderer menu-button-atlas-texture menu-button)
  (sdl2:with-rects ((src (* (base-menu-button-width) (get-column menu-button))
                         (* (base-menu-button-height) (get-row menu-button))
                         (base-menu-button-width)
                         (base-menu-button-height))
                    (dst (floor (- (/ (game-width) 2) (/ (menu-button-width) 2)))
                         (get-position menu-button)
                         (menu-button-width)
                         (menu-button-height)))
    (sdl2:render-copy renderer menu-button-atlas-texture :source-rect src :dest-rect dst)))

(defun render-menu-buttons! (renderer menu-button-atlas-texture menu-buttons)
  (map nil (lambda (menu-button)
             (render-menu-button! renderer menu-button-atlas-texture menu-button))
       menu-buttons))

(defun render-menu-atmosphere! (renderer menu-atmosphere-texture)
  (sdl2:with-rects ((src 0 0 (menu-atmosphere-base-width) (menu-atmosphere-base-height))
                    (dst 0 0 (game-width) (game-height)))
    (sdl2:render-copy renderer menu-atmosphere-texture :source-rect src :dest-rect dst)))

(defmethod render-game-state! (renderer texture-map game (game-state menu))
  (declare (ignore game))
  (render-menu-atmosphere! renderer (texture/menu-atmosphere texture-map))
  (render-menu-background! renderer (texture/menu-background texture-map))
  (render-menu-buttons! renderer (texture/menu-buttons texture-map) (get-buttons game-state)))
