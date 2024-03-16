;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass potion (entity)
  ()
  (:default-initargs
   :attackbox nil))

;;; Make potions bob up and down.
(defmethod get-y ((object potion))
  (+ (call-next-method)
     (* 10 (sin (* 2 (/ (sdl2:get-ticks) 1000) 3.14)))))

(defclass red-potion (potion)
  ()
  (:default-initargs
   :hitbox (make-instance 'rectangle
                          :width (red-potion-hitbox-width)
                          :height (red-potion-height)
                          :x-offset (red-potion-hitbox-x-offset)
                          :y-offset (red-potion-hitbox-y-offset))))

(defmethod entity-step! (game game-state (potion red-potion) entity-state dticks)
  (let ((player (get-player game-state)))
    (when (player-touches-entity? player potion)
      (damage! player -10)
      (setf (get-active? potion) nil))))

(defclass blue-potion (potion)
  ()
  (:default-initargs
   :hitbox (make-instance 'rectangle
                          :width (blue-potion-hitbox-width)
                          :height (blue-potion-height)
                          :x-offset (blue-potion-hitbox-x-offset)
                          :y-offset (blue-potion-hitbox-y-offset))))

(defmethod entity-step! (game game-state (potion blue-potion) entity-state dticks)
  (when (player-touches-entity? (get-player game-state) potion)
    (format *trace-output* "~&Power up.~%")
    (force-output *trace-output*)
    (setf (get-active? potion) nil)))

              
