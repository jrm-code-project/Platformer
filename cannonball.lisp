;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass cannonball (entity)
  ((speed :initarg :speed :reader get-speed)
   (start-tick :initform nil :accessor get-start-tick))
  (:default-initargs
   :attackbox nil
   :hitbox (make-instance 'rectangle
                          :x-offset (cannonball-hitbox-x-offset)
                          :y-offset (cannonball-hitbox-y-offset)
                          :width (cannonball-hitbox-width)
                          :height (cannonball-hitbox-height))))

(defmethod get-x ((cannonball cannonball))
  (+ (call-next-method)
     (* (get-speed cannonball)
        (- (sdl2:get-ticks) (get-start-tick cannonball)))))

(defun fire-cannonball! (cannonball)
  (setf (get-start-tick cannonball) (sdl2:get-ticks)
        (get-active? cannonball) t))

(defmethod entity-step! (game game-state (cannonball cannonball) entity-state dticks)
  (let* ((left (floor (get-x cannonball)))
         (right (floor (+ left (get-width (get-hitbox cannonball)))))
         (bottom (floor (get-y cannonball)))
         (top (floor (- bottom (get-height (get-hitbox cannonball))))))
    (when (or (< left 0)
              (>= right (level-width game-state))
              (< top 0)
              (>= bottom (game-height))
              (not (blank-tile? game-state left bottom))
              (not (blank-tile? game-state left top))
              (not (blank-tile? game-state right top))
              (not (blank-tile? game-state right bottom))
              )
      (setf (get-active? cannonball) nil))
    (when (player-touches-entity? (get-player game-state) cannonball)
      (damage! (get-player game-state) 20)
      (setf (get-active? cannonball) nil
            (get-state (get-player game-state)) :hit))))

