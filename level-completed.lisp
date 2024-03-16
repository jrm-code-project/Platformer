;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass level-completed ()
  ((buttons :initarg :buttons :reader get-buttons)))

(defun restart-level-button-x-offset ()
  (- (/ (game-width) 2)
     (level-complete-buttons-x-offset)
     (urm-button-width)))

(defun restart-level-button-y-offset ()
  (+ (level-complete-buttons-y-offset)
     (- (/ (game-height) 2)
        (/ (level-complete-height) 2))))

(defun next-level-button-x-offset ()
  (+ (/ (game-width) 2)
     (level-complete-buttons-x-offset)))

(defun restart-level-button-under-mouse? (mouse-x mouse-y)
  (let* ((button-left (restart-level-button-x-offset))
         (button-right (+ button-left (urm-button-width)))
         (button-top (restart-level-button-y-offset))
         (button-bottom (+ button-top (urm-button-height))))
    (and (< button-left mouse-x button-right)
         (< button-top mouse-y button-bottom))))

(defun next-level-button-under-mouse? (mouse-x mouse-y)
  (let* ((button-left (next-level-button-x-offset))
         (button-right (+ button-left (urm-button-width)))
         (button-top (restart-level-button-y-offset))
         (button-bottom (+ button-top (urm-button-height))))
    (and (< button-left mouse-x button-right)
         (< button-top mouse-y button-bottom))))

(defmethod game-step! (game (game-state level-completed) dticks)
  (declare (ignore dticks))
  (let* ((buttons (get-buttons game-state))
         (restart-level-button (first buttons))
         (next-level-button (second buttons)))

    (multiple-value-bind (mouse-x mouse-y mouse-buttons) (sdl2:mouse-state)
      (cond ((not (restart-level-button-under-mouse? mouse-x mouse-y))
             (setf (get-column restart-level-button) 0))
            ((logbitp 0 mouse-buttons)
             (ecase (get-column restart-level-button)
               (0 nil)
               (1 (setf (get-column restart-level-button) 2))
               (2 nil)))
            (t (ecase (get-column restart-level-button)
                 (0 (setf (get-column restart-level-button) 1))
                 (1 nil)
                 (2 (setf (get-column restart-level-button) 0)
                  (funcall (get-action restart-level-button) game)))))

      (cond ((not (next-level-button-under-mouse? mouse-x mouse-y))
             (setf (get-column next-level-button) 0))
            ((logbitp 0 mouse-buttons)
             (ecase (get-column next-level-button)
               (0 nil)
               (1 (setf (get-column next-level-button) 2))
               (2 nil)))
            (t (ecase (get-column next-level-button)
                 (0 (setf (get-column next-level-button) 1))
                 (1 nil)
                 (2 (setf (get-column next-level-button) 0)
                  (funcall (get-action next-level-button) game))))))))

(defun render-level-complete-buttons! (renderer texture restart-level-button next-level-button)
  (render-urm-button! renderer texture restart-level-button
                      (restart-level-button-x-offset)
                      (restart-level-button-y-offset))
  (render-urm-button! renderer texture next-level-button
                      (next-level-button-x-offset)
                      (restart-level-button-y-offset)))

(defun render-level-complete-background! (renderer texture)
  (sdl2:with-rects ((src 0 0 (base-level-complete-width) (base-level-complete-height))
                    (dst (- (/ (game-width) 2) (/ (level-complete-width) 2))
                         (- (/ (game-height) 2) (/ (level-complete-height) 2))
                         (level-complete-width)
                         (level-complete-height)))
    (sdl2:render-copy renderer texture :source-rect src :dest-rect dst)))

(defun render-level-complete! (renderer texture-map buttons)
  (render-level-complete-background! renderer (texture/level-complete texture-map))
  (render-level-complete-buttons! renderer (texture/urm texture-map) (first buttons) (second buttons)))

(defmethod render-game-state! (renderer texture-map game (game-state level-completed))
  (render-game-state! renderer texture-map game (get-level game))
  (render-level-complete! renderer texture-map (get-buttons game-state)))