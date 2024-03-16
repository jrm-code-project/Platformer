;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass trap (entity)
  ()
  (:default-initargs
   :attackbox nil))

(defclass spikes (trap)
  ()
  (:default-initargs
   :hitbox (make-instance 'rectangle
                          :width (spikes-hitbox-width)
                          :height (spikes-hitbox-height)
                          :x-offset (spikes-hitbox-x-offset)
                          :y-offset (spikes-hitbox-y-offset))))

(defmethod entity-step! (game game-state (spikes spikes) entity-state dticks)
  (when (player-touches-entity? (get-player game-state) spikes)
    (format *trace-output* "~&Ouch!~%")
    (force-output *trace-output*)
    (damage! (get-player game-state) 50)))
