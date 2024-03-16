;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass cannon (container)
  ((fired? :initform nil :accessor cannon-fired?))
  (:default-initargs
   :attackbox nil
   :hitbox (make-instance 'rectangle
                          :width (cannon-hitbox-width)
                          :height (cannon-hitbox-height)
                          :y-offset (cannon-hitbox-y-offset)
                          :x-offset (cannon-hitbox-x-offset))))

(defmethod hit! ((entity cannon)) nil)

(defvar *last-frame* 0)

(defmethod entity-step! (game game-state (cannon cannon) entity-state dticks)
  (let ((player (get-player game-state)))
    (cond ((get-start-tick (get-animation cannon))  
           (when (and (= (get-frame (get-animation cannon)) 4)
                      (not (cannon-fired? cannon)))
             (fire-cannonball! (get-contents cannon))
             (setf (cannon-fired? cannon) t))

           (when (> (- (sdl2:get-ticks) (get-start-tick (get-animation cannon)))
                    3000)
             (reset-one-shot! (get-animation cannon))))
          ((and (zerop (get-frame (get-animation cannon)))
                (= (get-y player) (get-y cannon))
                (if (get-flip cannon)
                    (> (get-x player) (get-x cannon))
                    (< (get-x player) (get-x cannon))))
           (setf (cannon-fired? cannon) nil)
           (trigger-one-shot! (get-animation cannon)))
          (t nil))))
