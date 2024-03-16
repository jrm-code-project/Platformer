;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass rectangle ()
  ((width  :initarg :width  :reader get-width)
   (height :initarg :height :reader get-height)
   (x-offset :initarg :x-offset :initform 0 :accessor get-x-offset)
   (y-offset :initarg :y-offset :accessor get-y-offset)))

(defclass entity ()
  ((active? :initform t :accessor get-active?)
   (x :initarg :x :accessor get-x)
   (y :initarg :y :accessor get-y)
   (flip :initarg :flip :initform nil :accessor get-flip)
   (delta-y :initform 0 :accessor delta-y)
   (animation :initarg :animation :reader get-animation)
   (state :initarg :state :accessor get-state)
   (hitbox :initarg :hitbox :accessor get-hitbox)
   (attackbox :initarg :attackbox :accessor get-attackbox)
   (health :initarg :health :accessor get-health)))

(defmethod (setf get-state) :after (state (entity entity))
  (setf (get-row (get-animation entity)) state
        (get-start-tick (get-animation entity)) (sdl2:get-ticks)))

(defgeneric enemy? (entity)
  (:method ((entity entity)) nil))

(defgeneric hit! (entity)
  (:method ((entity entity)) nil))

(defun damage! (entity amount)
  (let ((new-health (min 100 (max 0 (- (get-health entity) amount)))))
    (setf (get-health entity) new-health)))

(defun supported? (game entity)
  ;; true if entity is sitting on something
  (let ((right (floor (+ (get-x entity) (get-width (get-hitbox entity))))))
    (or (>= (1+ (get-y entity)) (game-height))
        (not (= (get-tile game (get-x entity) (1+ (get-y entity))) 11))
        (not (= (get-tile game right (1+ (get-y entity))) 11)))))

(defun overlaps? (a b c d)
  (not (or (< b c)
           (< d a))))

(defun player-touches-entity?  (player entity)
  (let* ((player-hitbox (get-hitbox player))
         (entity-hitbox (get-hitbox entity))
         (player-left (floor (get-x player)))
         (player-right (+ player-left (get-width player-hitbox)))
         (player-bottom (floor (- (get-y player) (get-y-offset player-hitbox))))
         (player-top (- player-bottom (get-height player-hitbox)))

         (entity-left (floor (get-x entity)))
         (entity-right (+ entity-left (get-width entity-hitbox)))
         (entity-bottom (floor (- (get-y entity) (get-y-offset entity-hitbox))))
         (entity-top (- entity-bottom (get-height entity-hitbox))))
    (and (overlaps? player-left player-right entity-left entity-right)
         (overlaps? player-top player-bottom entity-top entity-bottom))))

(defun can-attack-player? (entity player)
  (and (= (get-y player) (get-y entity))
       (let* ((attackbox (get-attackbox entity))
              (hitbox    (get-hitbox player))
              (hitbox-left (- (floor (get-x player)) *world-x-offset*))
              (hitbox-right (+ hitbox-left (get-width hitbox)))
              (hitbox-bottom (floor (- (get-y player) (get-y-offset hitbox))))
              (hitbox-top (- hitbox-bottom (get-height hitbox)))

              (attackbox-left (- (floor (+ (get-x entity) (get-x-offset attackbox))) *world-x-offset*))
              (attackbox-right (+ attackbox-left (get-width attackbox)))
              (attackbox-bottom (+ (get-y entity) (get-y-offset attackbox)))
              (attackbox-top (- attackbox-bottom (get-height attackbox))))
         (and (overlaps? hitbox-left hitbox-right attackbox-left attackbox-right)
              (overlaps? hitbox-top hitbox-bottom attackbox-top attackbox-bottom)))))

(defun can-attack-entity? (player entity)
  (and (= (get-y player) (get-y entity))
       (let* ((attackbox (get-attackbox player))
              (hitbox    (get-hitbox entity))
              (hitbox-left (- (floor (get-x entity)) *world-x-offset*))
              (hitbox-right (+ hitbox-left (get-width hitbox)))
              (hitbox-bottom (floor (- (get-y entity) (get-y-offset hitbox))))
              (hitbox-top (- hitbox-bottom (get-height hitbox)))

              (attackbox-left (- (floor (+ (get-x player) (get-x-offset attackbox))) *world-x-offset*))
              (attackbox-right (+ attackbox-left (get-width attackbox)))
              (attackbox-bottom (+ (get-y player) (get-y-offset attackbox)))
              (attackbox-top (- attackbox-bottom (get-height attackbox))))
         (and (overlaps? hitbox-left hitbox-right attackbox-left attackbox-right)
              (overlaps? hitbox-top hitbox-bottom attackbox-top attackbox-bottom)))))

(defun render-draw-box! (renderer entity box)
  (sdl2:with-rects ((src (floor (- (+ (get-x entity)
                                      (get-x-offset box))
                                    *world-x-offset*))
                         (floor (- (get-y entity)
                                   (get-height box)
                                   (get-y-offset box)))
                         (floor (get-width box))
                         (floor (get-height box))))
    (sdl2:render-draw-rect renderer src)))

(defun render-entity-attackbox! (renderer entity)
  (sdl2:set-render-draw-color renderer #x00 #x00 #xFF #xff)
  (render-draw-box! renderer entity (get-attackbox entity)))

(defun render-entity-hitbox! (renderer entity)
  (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xff)
  (render-draw-box! renderer entity (get-hitbox entity)))

(defun render-entity-sprite! (renderer texture-map entity)
  (render-animation! renderer texture-map (get-animation entity)
                     (- (floor (get-x entity)) *world-x-offset*)
                     (get-y entity)
                     (get-flip entity)))

(defparameter *render-hitbox* nil)
(defparameter *render-attackbox* nil)

(defun render-entity! (renderer texture-map entity)
  (render-entity-sprite! renderer texture-map entity)
  (when *render-hitbox*
    (render-entity-hitbox! renderer entity))
  (when (and *render-attackbox*
             (not (null (get-attackbox entity))))
    (render-entity-attackbox! renderer entity)))

(defclass player (entity)
  ()
  (:default-initargs
   :attackbox  (make-instance 'rectangle
                              :width (player-attackbox-width)
                              :height (player-attackbox-height)
                              :x-offset (player-attackbox-right-x-offset)
                              :y-offset (player-attackbox-y-offset))
   :health 100
   :hitbox (make-instance 'rectangle
                          :width (player-hitbox-width)
                          :height (player-hitbox-height)
                          :y-offset (player-hitbox-y-offset))))

(defclass enemy (entity)
  ((x-velocity :initform 0 :accessor get-x-velocity)))

(defmethod (setf get-x-velocity) :after (new-x-velocity (entity entity))
  (setf (get-flip entity) (minusp new-x-velocity)))

(defmethod enemy? ((entity enemy)) t)

(defclass crabby (enemy)
  ()
  (:default-initargs
   :attackbox (make-instance 'rectangle
                             :width (crabby-attackbox-width)
                             :height (crabby-attackbox-height)
                             :y-offset (crabby-attackbox-y-offset)
                             :x-offset (crabby-attackbox-x-offset))
   :health 20
   :hitbox  (make-instance 'rectangle
                           :width (crabby-hitbox-width)
                           :height (crabby-hitbox-height)
                           :y-offset (crabby-hitbox-y-offset))))

(defmethod hit! ((crabby crabby))
  (damage! crabby 10)
  (setf (get-state crabby) :hit))
