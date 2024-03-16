;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defun level-background-base-width () 832)
(defun level-background-base-height () 448)
(defun-scaled big-cloud-width 448)
(defun-scaled big-cloud-height 101)
(defun-scaled big-cloud-y-offset 204)
(defun-scaled small-cloud-width 74)
(defun-scaled small-cloud-height 24)
(defun-scaled small-cloud-minimum-y 80)
(defun-scaled small-cloud-maximum-y 175)

(defclass level ()
  ((player        :initarg :player      :reader   get-player)
   (entities      :initarg :entities    :accessor get-entities)
   (level-tiles   :initarg :level-tiles :accessor level-tiles)
   (reset         :initarg :reset       :accessor get-reset)
   (next-level    :initarg :next-level  :accessor get-next-level)
   (cloud-heights :initform
                  (do ((l '() (cons (+ (small-cloud-minimum-y)
                                       (random (- (small-cloud-maximum-y)
                                                  (small-cloud-minimum-y))))
                                    l))
                       (i 0 (1+ i)))
                      ((>= i 10) l))
                  :reader get-cloud-heights)))

(defun level-tiles-width (level-tiles)
  (array-dimension level-tiles 0))

(defun level-tiles-height (level-tiles)
  (array-dimension level-tiles 1))

(defun level-width (level)
  (* (level-tiles-width (level-tiles level)) (tile-size)))

(defun get-tile (level x y)
  (aref (level-tiles level) (floor x (tile-size)) (floor y (tile-size)) 0))

(defun blank-tile? (level x y)
  (= 11 (get-tile level x y)))

(defun make-level (tiles
                   player-animation-atlas
                   crabby-animation-atlas
                   potion-animation-atlas
                   crate-animation-atlas
                   spikes-atlas
                   cannon-animation-atlas
                   cannonball-atlas
                   next-level)
  (make-instance
   'level
   :player (block find-player
             (dotimes (i (level-tiles-width tiles))
               (dotimes (j (level-tiles-height tiles))
                 (when (= 100 (aref tiles i j 1))
                   (return-from find-player
                     (make-instance 'player
                                    :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                          (/ (player-hitbox-width) 2))
                                    :y (- (* (+ j 1) (tile-size)) 1)
                                    :state :idle
                                    :animation (make-instance 'frame-loop
                                                              :atlas player-animation-atlas
                                                              :row :idle)))))))
   :entities (let ((entity-list '()))
               (dotimes (i (level-tiles-width tiles) entity-list)
                 (dotimes (j (level-tiles-height tiles))
                   (cond ((zerop (aref tiles i j 1))
                          (push
                           (make-instance 'crabby
                                          :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                (/ (crabby-hitbox-width) 2))
                                          :y (- (* (+ j 1) (tile-size)) 1)
                                          :state :idle
                                          :animation (make-instance 'frame-loop
                                                                    :atlas crabby-animation-atlas
                                                                    :row :idle))
                           entity-list))
                         ((= 0 (aref tiles i j 2))
                          (let ((contents
                                  (make-instance 'blue-potion
                                                 :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                       (/ (blue-potion-hitbox-width) 2))
                                                 :y (- (* (+ j 1) (tile-size)) 1)
                                                 :state :idle
                                                 :animation (make-instance 'frame-loop
                                                                           :atlas potion-animation-atlas
                                                                           :row :idle-blue))))
                            (setf (get-active? contents) nil)
                            (push contents entity-list)
                            (push
                             (make-instance 'chest
                                            :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                  (/ (chest-hitbox-width) 2))
                                            :y (- (* (+ j 1) (tile-size)) 1)
                                            :state :idle
                                            :animation (make-instance 'one-shot
                                                                      :atlas crate-animation-atlas
                                                                      :row :box)
                                            :contents contents)
                             entity-list)))
                         ((= 1 (aref tiles i j 2))
                          (let ((contents
                                  (make-instance 'red-potion
                                                 :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                       (/ (red-potion-hitbox-width) 2))
                                                 :y (- (* (+ j 1) (tile-size)) 1)
                                                 :state :idle
                                                 :animation (make-instance 'frame-loop
                                                                           :atlas potion-animation-atlas
                                                                           :row :idle-red))))
                            (setf (get-active? contents) nil)
                            (push contents entity-list)
                            (push
                             (make-instance 'barrel
                                            :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                  (/ (barrel-hitbox-width) 2))
                                            :y (- (* (+ j 1) (tile-size)) 1)
                                            :state :idle
                                            :animation (make-instance 'one-shot
                                                                      :atlas crate-animation-atlas
                                                                      :row :barrel)
                                            :contents contents)
                             entity-list)))
                         ((= 2 (aref tiles i j 2))
                          (push
                           (make-instance 'blue-potion
                                          :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                (/ (blue-potion-hitbox-width) 2))
                                          :y (- (* (+ j 1) (tile-size)) 1)
                                          :state :idle
                                          :animation (make-instance 'frame-loop
                                                                    :atlas potion-animation-atlas
                                                                    :row :idle-blue))
                           entity-list))
                         ((= 3 (aref tiles i j 2))
                          (push
                           (make-instance 'red-potion
                                          :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                (/ (red-potion-hitbox-width) 2))
                                          :y (- (* (+ j 1) (tile-size)) 1)
                                          :state :idle
                                          :animation (make-instance 'frame-loop
                                                                    :atlas potion-animation-atlas
                                                                    :row :idle-red))
                           entity-list))
                         ((= 4 (aref tiles i j 2))
                          (push
                           (make-instance 'spikes
                                          :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                (/ (spikes-hitbox-width) 2))
                                          :y (- (* (+ j 1) (tile-size)) 1)
                                          :state :idle
                                          :animation (make-instance 'frame-loop
                                                                    :atlas spikes-atlas
                                                                    :row :idle))
                           entity-list))
                         ((= 5 (aref tiles i j 2))
                          (let ((cannonball
                                  (make-instance 'cannonball
                                                 :x (- (* i (tile-size))
                                                       (cannonball-width))
                                                 :y (- (* (+ j 1) (tile-size))
                                                       12)
                                                 :state :idle
                                                 :speed -0.5
                                                 :animation (make-instance 'frame-loop
                                                                           :atlas cannonball-atlas
                                                                           :row :idle))))
                            (setf (get-active? cannonball) nil)
                            (push
                             (make-instance 'cannon
                                            :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                  (/ (cannon-hitbox-width) 2))
                                            :y (- (* (+ j 1) (tile-size)) 1)
                                            :contents cannonball
                                            :state :boom
                                            :animation (make-instance 'one-shot-loop
                                                                      :atlas cannon-animation-atlas
                                                                      :row :boom))
                             entity-list)
                            (push cannonball entity-list)))
                         ((= 6 (aref tiles i j 2))
                          (let ((cannonball
                                  (make-instance 'cannonball
                                                 :x (* (+ i 1) (tile-size))
                                                 :y (- (* (+ j 1) (tile-size))
                                                       12)
                                                 :state :idle
                                                 :speed 0.5
                                                 :animation (make-instance 'frame-loop
                                                                           :atlas cannonball-atlas
                                                                           :row :idle))))
                            (setf (get-active? cannonball) nil)
                            (push cannonball entity-list)
                            (push
                             (make-instance 'cannon
                                            :x (- (+ (/ (tile-size) 2) (* i (tile-size)))
                                                  (/ (cannon-hitbox-width) 2))
                                            :y (- (* (+ j 1) (tile-size)) 1)
                                            :flip t
                                            :contents cannonball
                                            :state :boom
                                            :animation (make-instance 'one-shot-loop
                                                                      :atlas cannon-animation-atlas
                                                                      :row :boom))
                             entity-list)))
                         (t
                          ;; (unless (zerop (aref tiles i j 2))
                          ;;   (format *trace-output* "~&code: ~d~%" (aref tiles i j 2)))
                          nil)))))
   :level-tiles tiles
   :reset (lambda () (make-level tiles
                                 player-animation-atlas
                                 crabby-animation-atlas
                                 potion-animation-atlas
                                 crate-animation-atlas
                                 spikes-atlas
                                 cannon-animation-atlas
                                 cannonball-atlas
                                 next-level))
   :next-level next-level
   ))

(defun clear-floor-between? (level-tiles x0 x1 y)
  (let ((tile-y (floor y (tile-size))))
    (do ((tile-x (floor x0 (tile-size)) (1+ tile-x)))
        ((>= tile-x (floor x1 (tile-size))) t)
      (unless (and (= (aref level-tiles tile-x tile-y 0) 11)
                   (not (= (aref level-tiles tile-x (1+ tile-y) 0) 11)))
        (return-from clear-floor-between? nil)))))

(defun can-reach-player? (level-tiles entity player)
  ;; true if player is in range and entity can walk to player
  (let ((player-right (floor (+ (get-x player) (get-width (get-hitbox player)))))
        (entity-right (floor (+ (get-x entity) (get-width (get-hitbox entity))))))
    (and (= (get-y player) (get-y entity))
         (or (and (< 0 player-right (get-x entity))
                  (< (- (get-x entity) player-right) (* (tile-size) 5))
                  (clear-floor-between? level-tiles player-right (get-x entity) (get-y player)))
             (and (< 0 entity-right (get-x player))
                  (< (- (get-x player) entity-right) (* (tile-size) 5))
                  (clear-floor-between? level-tiles entity-right (get-x player) (get-y player)))))))

(defun move-entity-to! (game-state entity x y)
  (flet ((move-x (x y)
           (let ((top (floor (- y (get-height (get-hitbox entity)))))
                 (right (floor (+ x (get-width (get-hitbox entity))))))
             (when (and (>= x 0)
                        (< right (level-width game-state))
                        (blank-tile? game-state x y)
                        (blank-tile? game-state x top)
                        (blank-tile? game-state right top)
                        (blank-tile? game-state right y))
               (setf (get-x entity) x))))

         (move-y (x y)
           (let ((top (floor (- y (get-height (get-hitbox entity)))))
                 (right (floor (+ x (get-width (get-hitbox entity))))))
             (when (and (> top 0)
                        (<= y (game-height))
                        (blank-tile? game-state x y)
                        (blank-tile? game-state x top)
                        (blank-tile? game-state right top)
                        (blank-tile? game-state right y))
               (setf (get-y entity) y)))))
    (move-x x (get-y entity))
    (move-y (get-x entity) y)))

(defun falling! (player)
  (setf (delta-y player) 0
        (get-state player) :falling))

(defun idle! (player)
  (setf (get-state player) :idle))

(defun attack1! (player)
  (setf (get-state player) :attack1))

(defun running! (player)
  (setf (get-state player) :running))

(defun jumping! (player)
  (setf (delta-y player) (jump-velocity)
        (get-state player) :jumping))

(defgeneric entity-step! (game game-state entity entity-state dticks))

(defmethod game-step! (game (game-state level) dticks)
  (if (sdl2:keyboard-state-p :scancode-backspace)
      (setf (get-state game) (get-paused game))
      (map nil (lambda (entity)
                 (when (get-active? entity)
                   (entity-step! game game-state entity (get-state entity) dticks)))
           (cons (get-player game-state)
                 (get-entities game-state)))))

(defmethod entity-step! (game game-state (player player) (entity-state (eql :idle)) dticks)
  (multiple-value-bind (x y buttons) (sdl2:mouse-state)
    (declare (ignore x y))
    (let ((mouse-left (logbitp 0 buttons))
                                        ;(mouse-right (logbitp 2 buttons))
          (key-left (sdl2:keyboard-state-p :scancode-left))
          (key-right (sdl2:keyboard-state-p :scancode-right))
          (key-space (sdl2:keyboard-state-p :scancode-space)))
         (cond ((zerop (get-health player))
                (setf (get-state game) (get-game-over game)))
               ((not (supported? game-state player)) (falling! player))
               (mouse-left                     (attack1! player))
               ((or (and key-left (not key-right))
                    (and (not key-left) key-right))
                (running! player))
               (key-space                      (jumping! player))
               (t nil)))))

(defmethod entity-step! (game game-state (player player) (entity-state (eql :running)) dticks)
  (multiple-value-bind (x y buttons) (sdl2:mouse-state)
    (declare (ignore x y))
    (let ((mouse-left (logbitp 0 buttons))
                                        ;(mouse-right (logbitp 2 buttons))
          (key-left (sdl2:keyboard-state-p :scancode-left))
          (key-right (sdl2:keyboard-state-p :scancode-right))
          (key-space (sdl2:keyboard-state-p :scancode-space)))
         (cond ((not (supported? game-state player)) (falling! player))
               (mouse-left                     (attack1! player))
               ((or (and key-left key-right)
                    (and (not key-left) (not key-right)))
                (idle! player))
               (key-space                      (jumping! player))
               (key-left
                (setf (get-flip player) t
                      (get-x-offset (get-attackbox player)) (player-attackbox-left-x-offset))
                (move-entity-to! game-state player (- (get-x player) (run-velocity)) (get-y player)))
               (key-right
                (setf (get-flip player) nil
                      (get-x-offset (get-attackbox player)) (player-attackbox-right-x-offset))
                (move-entity-to! game-state player (+ (get-x player) (run-velocity)) (get-y player)))
               (t nil))
          )))

(defmethod entity-step! (game game-state (player player) (entity-state (eql :hit)) dticks)
  (cond ((not (supported? game-state player)) (falling! player))
        ((zerop (get-animation-iteration (get-animation player))) nil)
        (t (setf (get-state player) :idle))))

(defmethod entity-step! (game game-state (player player) (entity-state (eql :attack1)) dticks)
  (multiple-value-bind (x y buttons) (sdl2:mouse-state)
    (declare (ignore x y))
    (let ((mouse-left (logbitp 0 buttons))
                                        ;(mouse-right (logbitp 2 buttons))
          ;;(key-left (sdl2:keyboard-state-p :scancode-left))
          ;;(key-right (sdl2:keyboard-state-p :scancode-right))
          ;;(key-space (sdl2:keyboard-state-p :scancode-space))
          )
         (cond ((not mouse-left) (idle! player))
               ((= 1 (get-frame (get-animation player)))
                (dolist (entity (get-entities game-state))
                  (when (and (get-active? entity)
                             (can-attack-entity? player entity)
                             (not (member (get-state entity) '(:hit :dying))))
                    (hit! entity)
                    (return nil))))
               (t nil)))))

(defmethod entity-step! (game game-state (player player) (entity-state (eql :jumping)) dticks)
  (let ((key-left (sdl2:keyboard-state-p :scancode-left))
        (key-right (sdl2:keyboard-state-p :scancode-right)))
    (cond
      ((< (delta-y player) 0) (falling! player))
      (t (let ((new-y (floor (- (get-y player) (delta-y player))))
               (top   (floor (- (get-y player) (get-height (get-hitbox player)) (delta-y player) 1)))
               (right (floor (+ (get-x player) (get-width (get-hitbox player))))))
           (if (or (< top 0)
                   (not (blank-tile? game-state (get-x player) top))
                   (not (blank-tile? game-state right top)))
               (progn
                 (setf (delta-y player) -2)
                 (move-entity-to! game-state player
                                  (+ (get-x player) (cond (key-right
                                                           (setf (get-flip player) nil
                                                                 (get-x-offset (get-attackbox player)) (player-attackbox-right-x-offset))
                                                           (run-velocity))
                                                          (key-left
                                                           (setf (get-flip player) t
                                                                 (get-x-offset (get-attackbox player)) (player-attackbox-left-x-offset))
                                                           (- (run-velocity)))
                                                          (t 0)))
                                  (+ (get-height (get-hitbox player))
                                     (* (tile-size)
                                        (ceiling top (tile-size)))))
                 (falling! player))
               (progn
                 (move-entity-to! game-state player (+ (get-x player) (cond (key-right
                                                                             (setf (get-flip player) nil
                                                                                   (get-x-offset (get-attackbox player)) (player-attackbox-right-x-offset))
                                                                             (run-velocity))
                                                                            (key-left
                                                                             (setf (get-flip player) t
                                                                                   (get-x-offset (get-attackbox player)) (player-attackbox-left-x-offset))
                                                                             (- (run-velocity)))
                                                                            (t 0)))
                                  new-y)
                 (decf (delta-y player) (gravity)))))))))

(defmethod entity-step! (game game-state (player player) (entity-state (eql :falling)) dticks)
  (let ((key-left (sdl2:keyboard-state-p :scancode-left))
        (key-right (sdl2:keyboard-state-p :scancode-right)))
    (if (supported? game-state player)
        (idle! player)
        (let ((new-y (floor (- (get-y player) (delta-y player))))
              (right (floor (+ (get-x player) (get-width (get-hitbox player))))))
          (if (or (>= new-y (game-height))
                  (not (blank-tile? game-state (get-x player) new-y))
                  (not (blank-tile? game-state right new-y)))
              (progn
                (move-entity-to! game-state player
                                 (+ (get-x player)
                                    (cond (key-right
                                           (setf (get-flip player) nil
                                                 (get-x-offset (get-attackbox player)) (player-attackbox-right-x-offset))
                                           (run-velocity))
                                          (key-left
                                           (setf (get-flip player) t
                                                 (get-x-offset (get-attackbox player)) (player-attackbox-left-x-offset))
                                           (- (run-velocity)))
                                          (t 0)))
                                 (min new-y (floor new-y (tile-size))))
                (idle! player))
              (progn
                (move-entity-to! game-state player
                                 (+ (get-x player)
                                    (cond (key-right
                                           (setf (get-flip player) nil
                                                 (get-x-offset (get-attackbox player)) (player-attackbox-right-x-offset))
                                           (run-velocity))
                                          (key-left
                                           (setf (get-flip player) t
                                                 (get-x-offset (get-attackbox player)) (player-attackbox-left-x-offset))
                                           (- (run-velocity)))
                                          (t 0)))
                                 new-y)
                (decf (delta-y player) (gravity))
                ))))))

(defun can-move-here? (game-state entity x y)
  (let ((top (- y (get-height (get-hitbox entity))))
        (right (+ x (get-width (get-hitbox entity)))))

    (and (> top 0)
         (<= y (game-height))
         (> x 0)
         (<= right (level-width game-state))
         (blank-tile? game-state x y)
         (blank-tile? game-state x top)
         (blank-tile? game-state right top)
         (blank-tile? game-state right y)
         (not (blank-tile? game-state x (1+ y)))
         (not (blank-tile? game-state right (1+ y))))))

(defun check-finish-level (game game-state)
  (unless (find-if (lambda (entity)
                     (and (enemy? entity)
                          (get-active? entity)))
                   (get-entities game-state))
    (setf (get-state game)
          (funcall (get-level-complete game) game))))

(defmethod entity-step! (game game-state (crabby crabby) (crabby-state (eql :idle)) dticks)
  (let ((player (get-player game-state)))
    (cond ((zerop (get-health crabby))
           (setf (get-state crabby) :dying))
          ((can-attack-player? crabby player)
           (setf (get-state crabby) :attack
                 (get-x-velocity crabby) 0))
          ((can-reach-player? (level-tiles game-state) crabby player)
           (setf (get-state crabby) :running
                 (get-x-velocity crabby) (if (> (get-x crabby) (get-x player))
                                             (- (crabby-velocity))
                                             (crabby-velocity))))
          ((supported? game-state crabby)
           nil
                                        ;(setf (get-state crabby) :running
                                        ;      (get-x-velocity crabby) (crabby-velocity))
           )
          (t

           (let ((new-y (floor (- (get-y crabby) (delta-y crabby))))
                 (right (floor (+ (get-x crabby) (get-width (get-hitbox crabby))))))
             (if (or (>= new-y (game-height))
                     (not (blank-tile? game-state (get-x crabby) new-y))
                     (not (blank-tile? game-state right new-y)))
                 (progn
                   (move-entity-to! game-state crabby (get-x crabby)
                                    (min new-y (floor new-y (tile-size)))))
                 (progn
                   (move-entity-to! game-state crabby (get-x crabby)
                                    new-y)
                   (decf (delta-y crabby) (gravity))
                   )))))))

(defmethod entity-step! (game game-state (crabby crabby) (crabby-state (eql :running)) dticks)
  (let ((player (get-player game-state))
        (new-x (+ (get-x crabby) (get-x-velocity crabby))))
    (if (can-move-here? game-state crabby new-x (get-y crabby))
        (move-entity-to! game-state crabby new-x (get-y crabby))
        (cond ((> (get-x-velocity crabby) 0)
               (setf (get-x-velocity crabby) (- (get-x-velocity crabby))))
              ((< (get-x-velocity crabby) 0)
               (setf (get-x-velocity crabby) (- (get-x-velocity crabby))))))
    (cond ((can-attack-player? crabby player)
           (setf (get-state crabby) :attack
                 (get-x-velocity crabby) 0))
          ((can-reach-player? (level-tiles game-state) crabby player)
           (setf (get-x-velocity crabby)  (if (> (get-x crabby) (get-x player))
                                              (- (crabby-velocity))
                                              (crabby-velocity))))
          (t nil))))

(defmethod entity-step! (game game-state (crabby crabby) (crabby-state (eql :attack)) dticks)
  (let ((player (get-player game-state)))
    (if (zerop (get-animation-iteration (get-animation crabby)))
        (when (and (= (get-frame (get-animation crabby)) 3)
                   (can-attack-player? crabby player)
                   (member (get-state player)
                           '(:idle :running :landing :attack1 :attack2 :attack3)))
          (setf (get-state player) :hit)
          (damage! player 15))
        (setf (get-state crabby) :idle
              (get-x-velocity crabby) 0))))

(defmethod entity-step! (game game-state (crabby crabby) (crabby-state (eql :hit)) dticks)
  (cond ((not (supported? game-state crabby))
         (setf (get-state crabby) :idle))
        ((zerop (get-animation-iteration (get-animation crabby))) nil)
        (t (setf (get-state crabby) :idle))))

(defmethod entity-step! (game game-state (crabby crabby) (crabby-state (eql :dying)) dticks)
  (unless (zerop (get-animation-iteration (get-animation crabby)))
    (setf (get-active? crabby) nil)
    (check-finish-level game game-state)))

(defparameter *render-tiles* nil)

(defun render-level! (renderer outside-texture level-tiles)
  (multiple-value-bind (offset-tiles offset-within-tile) (floor *world-x-offset* (tile-size))
    (dotimes (row (height-in-tiles))
      (dotimes (column (if (zerop offset-within-tile)
                           (width-in-tiles)
                           (1+ (width-in-tiles))))
        (let ((red (aref level-tiles (+ column offset-tiles) row 0)))
          (multiple-value-bind (tile-source-row tile-source-column) (floor red 12)
            (sdl2:with-rects ((src (* tile-source-column (base-tile-size))
                                   (* tile-source-row (base-tile-size))
                                   (base-tile-size)
                                   (base-tile-size))
                              (dst (- (* column (tile-size)) offset-within-tile)
                                   (* row (tile-size))
                                   (tile-size)
                                   (tile-size)))
              (sdl2:render-copy renderer
                                outside-texture
                                :source-rect src
                                :dest-rect dst)
              (when *render-tiles*
                (sdl2:render-draw-rect renderer dst)))))))))

(defun render-small-clouds! (renderer small-cloud-texture small-cloud-heights)
  (do ((h small-cloud-heights (cdr h))
       (i 0 (+ i 1)))
      ((null h))
    (sdl2:with-rects ((src 0 0 (base-small-cloud-width) (base-small-cloud-height))
                      (dst (floor (- (* i 4 (small-cloud-width)) (* *world-x-offset* .7)))
                           (car h)
                           (small-cloud-width)
                           (small-cloud-height)))
      (sdl2:render-copy renderer small-cloud-texture :source-rect src :dest-rect dst))))

(defun render-big-clouds! (renderer big-cloud-texture)
  (dotimes (i 3)
    (sdl2:with-rects ((src 0 0 (base-big-cloud-width) (base-big-cloud-height))
                      (dst (- (* i (big-cloud-width)) (floor (* .3 *world-x-offset*))) (big-cloud-y-offset)
                           (big-cloud-width)
                           (big-cloud-height)))
      (sdl2:render-copy renderer big-cloud-texture :source-rect src :dest-rect dst))))

(defun render-clouds! (renderer big-cloud-texture small-cloud-texture small-cloud-heights)
  (render-big-clouds! renderer big-cloud-texture)
  (render-small-clouds! renderer small-cloud-texture small-cloud-heights))

(defun render-level-background! (renderer level-background-texture)
  (sdl2:with-rects ((src 0 0 (level-background-base-width) (level-background-base-height))
                    (dst 0 0 (game-width) (game-height)))
    (sdl2:render-copy renderer level-background-texture :source-rect src :dest-rect dst)))

(defun render-status-bar! (renderer health-status-texture player)
  (sdl2:with-rects ((src 0 0 (base-status-bar-width) (base-status-bar-height))
                    (dst (status-bar-x) (status-bar-y) (status-bar-width) (status-bar-height)))
    (sdl2:render-copy renderer health-status-texture :source-rect src :dest-rect dst))
  (sdl2:with-rects ((src (+ (health-bar-x) (status-bar-x))
                         (+ (health-bar-y) (status-bar-y))
                         (floor (* (health-bar-width) (/ (get-health player) 100)))
                         (health-bar-height)))
    (sdl2:set-render-draw-color renderer #xff #x00 #x00 #xFF)
    (sdl2:render-fill-rect renderer src))
  )

(defmethod render-game-state! (renderer texture-map game (game-state level))
  (declare (ignore game))
  (cond ((and (< (- (get-x (get-player game-state)) *world-x-offset*) (/ (game-width) 5))
              (> *world-x-offset* 0))
         (setf *world-x-offset*
               (max 0 (floor (- (get-x (get-player game-state)) (/ (game-width) 5))))))
        ((and (> (- (get-x (get-player game-state)) *world-x-offset*) (/ (* 4 (game-width)) 5))
              (< *world-x-offset* (- (level-width game-state) (game-width))))
         (setf *world-x-offset*
               (min (- (level-width game-state) (game-width))
                    (floor (- (get-x (get-player game-state)) (/ (* 4 (game-width)) 5))))))
        (t nil))

  (render-level-background! renderer (texture/level-background texture-map))
  (render-clouds! renderer
                  (texture/big-cloud texture-map)
                  (texture/small-cloud texture-map)
                  (get-cloud-heights game-state))
  (render-level! renderer (texture/outside texture-map) (level-tiles game-state))
  (render-status-bar! renderer (texture/health-power texture-map) (get-player game-state))
  (map nil (lambda (entity)
             (when (get-active? entity)
               (render-entity! renderer texture-map entity)))
       (cons (get-player game-state)
             (get-entities game-state))))
