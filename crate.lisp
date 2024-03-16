;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass container (entity)
  ((contents :initarg :contents :reader get-contents)))

(defmethod hit! ((entity container))
  (trigger-one-shot! (get-animation entity))
  (setf (get-active? (get-contents entity)) t))

(defmethod entity-step! (game game-state (entity container) entity-state dticks)
  (when (null (get-frame (get-animation entity)))
    (setf (get-active? entity) nil)))

(defclass chest (container)
  ()
  (:default-initargs
   :attackbox nil
   :hitbox (make-instance 'rectangle
                          :width (chest-hitbox-width)
                          :height (chest-hitbox-height)
                          :x-offset (chest-hitbox-x-offset)
                          :y-offset (chest-hitbox-y-offset))))

(defclass barrel (container)
  ()
  (:default-initargs
   :attackbox nil
   :hitbox (make-instance 'rectangle
                          :width (barrel-hitbox-width)
                          :height (barrel-hitbox-height)
                          :x-offset (barrel-hitbox-x-offset)
                          :y-offset (barrel-hitbox-y-offset))))

