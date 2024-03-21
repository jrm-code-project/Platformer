;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defun-scaled player-sprite-x-offset -22)
(defun-scaled player-sprite-y-offset -31)


(defstruct (surface-map
            (:conc-name "SURFACE/"))
  (player-sprites nil    :read-only t)
  (outside-sprites nil   :read-only t)
  (menu-background nil   :read-only t)
  (menu-atmosphere nil   :read-only t)
  (menu-button-atlas nil :read-only t)
  (paused-menu nil       :read-only t)
  (sound-button nil      :read-only t)
  (urm-button nil        :read-only t)
  (volume-button nil     :read-only t)
  (small-cloud nil       :read-only t)
  (big-cloud nil         :read-only t)
  (level-background nil  :read-only t)
  (crabby-sprites nil    :read-only t)
  (health-power nil      :read-only t)
  (game-over nil         :read-only t)
  (level-completed nil   :read-only t)
  (potions nil           :read-only t)
  (crates nil            :read-only t)
  (spikes nil            :read-only t)
  (cannon nil            :read-only t)
  (cannonball nil        :read-only t)
  )

(defclass game ()
  ((game-state :initarg :menu :accessor get-state)
   (menu :initarg :menu :reader get-menu)
   (level :initarg :level :accessor get-level)
   (paused :initarg :paused :reader get-paused)
   (level-complete :initarg :level-complete :reader get-level-complete)
   (game-over :initarg :game-over :reader get-game-over)
   (thread :initform nil :accessor game-thread)
   (last-ticks :initform (sdl2:get-ticks) :accessor game-last-ticks)
   (game-count :initform 0 :accessor game-count)
   ))

(defun random-color ()
  (vector (random 256) (random 256) (random 256)))

(defvar *the-game* nil)
(defvar *shutdown* (cons 0 0))

(defun render! (renderer texture-map game)
  (sdl2:set-render-draw-color renderer #xff #xff #xff #xff)
  (sdl2:render-clear renderer)
  (render-game-state! renderer texture-map game (get-state game))
  (sdl2:render-present renderer))

(defun make-levels (level-tiles-list
                    player-animation-atlas
                    crabby-animation-atlas
                    potion-animation-atlas
                    crate-animation-atlas
                    spikes-atlas
                    cannon-animation-atlas
                    cannonball-animation-atlas)
  (fold-left (lambda (levels tiles-list)
               (make-level tiles-list
                           player-animation-atlas
                           crabby-animation-atlas
                           potion-animation-atlas
                           crate-animation-atlas
                           spikes-atlas
                           cannon-animation-atlas
                           cannonball-animation-atlas
                           levels))
                   nil
                   (reverse level-tiles-list)))

(defun main-event-loop (window renderer surface-map all-level-data)
  (let-texture (renderer
                (player-texture (surface/player-sprites surface-map))
                (crabby-texture (surface/crabby-sprites surface-map))
                (outside-texture (surface/outside-sprites surface-map))
                (menu-background-texture (surface/menu-background surface-map))
                (menu-atmosphere-texture (surface/menu-atmosphere surface-map))
                (menu-button-atlas-texture (surface/menu-button-atlas surface-map))
                (paused-menu-texture (surface/paused-menu surface-map))
                (sound-button-texture (surface/sound-button surface-map))
                (urm-button-texture (surface/urm-button surface-map))
                (volume-button-texture (surface/volume-button surface-map))
                (small-clouds-texture (surface/small-cloud surface-map))
                (big-clouds-texture (surface/big-cloud surface-map))
                (health-power-texture (surface/health-power surface-map))
                (level-background-texture (surface/level-background surface-map))
                (game-over-texture (surface/game-over surface-map))
                (level-complete-texture (surface/level-completed surface-map))
                (potions-texture (surface/potions surface-map))
                (crates-texture (surface/crates surface-map))
                (spikes-texture (surface/spikes surface-map))
                (cannon-texture (surface/cannon surface-map))
                (cannonball-texture (surface/cannonball surface-map))
                )
    (let* ((last-ticks (sdl2:get-ticks))
           (title-ticker 0)
           (render-ticker 0)
           (frame-count 0)
           (texture-map (make-texture-map
                            :player  player-texture
                            :crabby  crabby-texture
                            :outside  outside-texture
                            :small-cloud  small-clouds-texture
                            :big-cloud  big-clouds-texture
                            :level-background  level-background-texture
                            :menu-background  menu-background-texture
                            :menu-atmosphere  menu-atmosphere-texture
                            :menu-buttons  menu-button-atlas-texture
                            :paused  paused-menu-texture
                            :sound   sound-button-texture
                            :urm     urm-button-texture
                            :volume  volume-button-texture
                            :health-power  health-power-texture
                            :game-over  game-over-texture
                            :level-complete level-complete-texture
                            :potions potions-texture
                            :crates crates-texture
                            :spikes spikes-texture
                            :cannon cannon-texture
                            :cannonball cannonball-texture
                            ))
           (cannonball-animation-atlas
             (make-instance 'animation-atlas
                            :texture-key #'texture/cannonball
                            :cell-x-offset (cannonball-x-offset)
                            :cell-y-offset (cannonball-y-offset)
                            :cell-width (base-cannonball-width)
                            :cell-height (base-cannonball-height)
                            :state-vector #(:idle)
                            :frame-count #(1)
                            :ticks-per-frame 1000))
           (cannon-animation-atlas
             (make-instance 'animation-atlas
                            :texture-key #'texture/cannon
                            :cell-x-offset (cannon-x-offset)
                            :cell-y-offset (cannon-y-offset)
                            :cell-width (base-cannon-width)
                            :cell-height (base-cannon-height)
                            :state-vector #(:boom)
                            :frame-count #(7)
                            :ticks-per-frame 80))
           (spikes-animation-atlas
             (make-instance 'animation-atlas
                            :texture-key #'texture/spikes
                            :cell-x-offset (spikes-x-offset)
                            :cell-y-offset (spikes-y-offset)
                            :cell-width (base-spikes-width)
                            :cell-height (base-spikes-height)
                            :state-vector #(:idle)
                            :frame-count #(1)
                            :ticks-per-frame 10))
           (potion-animation-atlas
             (make-instance 'animation-atlas
                            :texture-key #'texture/potions
                            :cell-x-offset (potion-x-offset)
                            :cell-y-offset (potion-y-offset)
                            :cell-width (base-potion-width)
                            :cell-height (base-potion-height)
                            :state-vector #(:idle-blue :idle-red)
                            :frame-count #(7 7)
                            :ticks-per-frame 80))
           (crate-animation-atlas
             (make-instance 'animation-atlas
                            :texture-key #'texture/crates
                            :cell-x-offset (crate-x-offset)
                            :cell-y-offset (crate-y-offset)
                            :cell-width (base-crate-width)
                            :cell-height (base-crate-height)
                            :state-vector #(:box :barrel)
                            :frame-count #(8 8)
                            :ticks-per-frame 100))
           (player-animation-atlas
             (make-instance 'animation-atlas
                            :texture-key #'texture/player
                            :cell-x-offset (player-sprite-x-offset)
                            :cell-y-offset (player-sprite-y-offset)
                            :cell-width 64
                            :cell-height 40
                            :state-vector #(:idle :running :jumping :falling :landing :hit :attack1 :attack2 :attack3)
                            :frame-count #(5 6 3 1 2 4 3 3 3)
                            :ticks-per-frame 100))
           (crabby-animation-atlas
             (make-instance 'animation-atlas
                            :texture-key #'texture/crabby
                            :cell-x-offset (crabby-sprite-x-offset)
                            :cell-y-offset (crabby-sprite-y-offset)
                            :cell-width (base-crabby-sprite-width)
                            :cell-height (base-crabby-sprite-height)
                            :state-vector #(:idle :running :attack :hit :dying)
                            :frame-count #(9 6 7 4 5)
                            :ticks-per-frame 200))


           (sound-buttons (list
                           (make-instance 'button :row 0 :position (music-button-position-y))
                           (make-instance 'button :row 0 :position (sfx-button-position-y))
                           (make-instance 'button :row 0 :position (/ (game-width) 2))
                           (make-instance 'button :row 0 :position (resume-button-position-x)
                                                  :action (lambda (game)
                                                            (setf (get-state game) (get-level game))))
                           (make-instance 'button :row 1 :position (restart-button-position-x)
                                                  :action (lambda (game)
                                                            (setf (get-level game) (funcall (get-reset (get-level game)))
                                                                  (get-state game) (get-level game))))
                           (make-instance 'button :row 2 :position (home-button-position-x)
                                                  :action (lambda (game)
                                                            (setf (get-state game) (get-menu game))))))
           (buttons  (list
                      (make-instance 'button :row 0 :position (play-button-position))
                      (make-instance 'button :row 1 :position (options-button-position))
                      (make-instance 'button :row 2 :position (quit-button-position))))
           (game (make-instance
                  'game
                  :game-over (make-instance 'game-over)

                  :menu (make-instance
                         'menu
                         :buttons buttons)
                  :paused (make-instance
                           'paused
                           :buttons sound-buttons)
                  :level-complete
                  (lambda (game)
                    (let* ((level (get-level game))
                           (next  (get-next-level level)))
                      (if next
                          (make-instance
                           'level-completed
                           :buttons (list
                                     (make-instance 'button :row 1
                                                            :action (lambda (game)
                                                                      (setf (get-level game) (funcall (get-reset level))
                                                                            (get-state game) (get-level game))))
                                     (make-instance 'button
                                                    :row 0
                                                    :action (lambda (game)
                                                              (setf (get-level game) next
                                                                    (get-state game) next)))))
                          (make-instance 'game-over))))
                  :level (make-levels
                          all-level-data
                          player-animation-atlas
                          crabby-animation-atlas
                          potion-animation-atlas
                          crate-animation-atlas
                          spikes-animation-atlas
                          cannon-animation-atlas
                          cannonball-animation-atlas))))

      (start-game! game)

      (unwind-protect
           (labels ((render-tick! (dticks)
                      (incf render-ticker dticks)
                      (when (>= render-ticker 13)
                        (decf render-ticker 13)
                        (incf frame-count)
                        (render! renderer texture-map game)))

                    (title-tick! (dticks)
                      (incf title-ticker dticks)
                      (when (>= title-ticker 1000)
                        (decf title-ticker 1000)
                        (sdl2:set-window-title window
                                               (format nil "Platformer, frame rate: ~d, game rate: ~d" frame-count
                                                       (game-count game)))
                        (setq frame-count 0)
                        (setf (game-count game) 0))))

             (sdl2:with-event-loop (:method :poll)
               (:idle
                ()
                (let ((this-ticks (sdl2:get-ticks)))
                  (if (= this-ticks last-ticks)
                      (sdl2:delay 5)
                      (let ((dticks (- this-ticks last-ticks)))
                        (setq last-ticks this-ticks)
                        (title-tick! dticks)
                        (render-tick! dticks)))))

               (:keydown (:keysym keysym)
                         (keydown game (sdl2:scancode keysym)))

               (:mousemotion
                (:x x :y y :xrel xrel :yrel yrel :state state)
                (declare (ignore xrel yrel state))
                                        ;(format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                (mousemove game x y))

               (:quit
                ()
                t)

               (:windowevent
                (:event event-id :data1 d1 :data2 d2)
                (case event-id
                  (#.sdl2-ffi:+sdl-windowevent-close+ (format *trace-output* "~&Close window"))
                  (#.sdl2-ffi:+sdl-windowevent-enter+
                                        ;(format *trace-output* "~&Enter window")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-exposed+
                                        ;(format *trace-output* "~&Window exposed")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-focus-gained+
                                        ;(format *trace-output* "~&Window gained focus")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-focus-lost+
                                        ;(format *trace-output* "~&Window lost focus")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-hidden+
                                        ;(format *trace-output* "~&Window hidden")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-hit-test+ (format *trace-output* "~&Window hit test"))
                  (#.sdl2-ffi:+sdl-windowevent-leave+
                                        ;(format *trace-output* "~&Leave window")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-maximized+ (format *trace-output* "~&Window maximized"))
                  (#.sdl2-ffi:+sdl-windowevent-minimized+ (format *trace-output* "~&Window minimized"))
                  (#.sdl2-ffi:+sdl-windowevent-moved+ nil)
                  (#.sdl2-ffi:+sdl-windowevent-none+ (format *trace-output* "~&Windowevent none?"))
                  (#.sdl2-ffi:+sdl-windowevent-resized+
                                        ;(format *trace-output* "~&Window resized")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-restored+ (format *trace-output* "~&Window restored"))
                  (#.sdl2-ffi:+sdl-windowevent-shown+
                                        ;(format *trace-output* "~&Window shown"))
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-size-changed+
                                        ;(format *trace-output* "~&Window size changed")
                   nil)
                  (#.sdl2-ffi:+sdl-windowevent-take-focus+
                                        ;(format *trace-output* "~&Window take focus")
                   nil)
                  (t (format *trace-output* "~&~d ~d ~d" event-id d1 d2) (force-output *trace-output*))))
               ))
        (shutdown-game! game)))))

(defun resource-directory ()
  (merge-pathnames (make-pathname :directory '(:relative "resources")) (project-directory)))

(defun resource-pathname (filename)
  (merge-pathnames (parse-namestring filename) (resource-directory)))

(defun level-files ()
  (sort
   (directory (merge-pathnames (make-pathname :directory '(:relative "levels")
                                             :name :wild
                                             :type "png")
                               (resource-directory)))
   #'string-lessp
   :key #'pathname-name))

(defun all-level-data ()
  (map 'list (lambda (level-file)
               (png-read:image-data
                (png-read:read-png-file level-file)))
       (level-files)))

(defun main-window ()
  (sdl2-ttf:init)
  (call-with-open-font
   (game-over-font) 40
   (lambda (font)
     (call-with-rendered-text
      font (format nil "Game Over") #xFF #xFF #xFF #xFF
      (lambda (game-over-surface)
        (sdl2:with-window (window
                           :title "Platformer"
                           :w (game-width)
                           :h (game-height)
                           :flags '(:shown))
          (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
            (let-surface ((player-sprites-surface    (resource-pathname "player_sprites.png"))
                          (outside-sprites-surface   (resource-pathname "outside_sprites.png"))
                          (menu-background-surface   (resource-pathname "menu_background.png"))
                          (menu-atmosphere-surface   (resource-pathname "background_menu.png"))
                          (menu-button-atlas-surface (resource-pathname "button_atlas.png"))
                          (pause-menu-surface        (resource-pathname "pause_menu.png"))
                          (sound-button-surface      (resource-pathname "sound_button.png"))
                          (urm-button-surface        (resource-pathname "urm_buttons.png"))
                          (volume-button-surface     (resource-pathname "volume_buttons.png"))
                          (small-clouds-surface      (resource-pathname "small_clouds.png"))
                          (big-clouds-surface        (resource-pathname "big_clouds.png"))
                          (level-background-surface  (resource-pathname "playing_bg_img.png"))
                          (crabby-sprites-surface    (resource-pathname "crabby_sprite.png"))
                          (health-power-surface      (resource-pathname "health_power_bar.png"))
                          (level-completed-surface   (resource-pathname "completed_sprite.png"))
                          (potions-surface           (resource-pathname "potions_sprites.png"))
                          (crates-surface            (resource-pathname "objects_sprites.png"))
                          (spikes-surface            (resource-pathname "trap_atlas.png"))
                          (cannon-surface            (resource-pathname "cannon_atlas.png"))
                          (cannonball-surface        (resource-pathname "ball.png"))
                          )
              (main-event-loop window renderer
                               (make-surface-map
                                :player-sprites player-sprites-surface
                                :crabby-sprites crabby-sprites-surface
                                :outside-sprites outside-sprites-surface
                                :menu-background menu-background-surface
                                :menu-atmosphere menu-atmosphere-surface
                                :menu-button-atlas menu-button-atlas-surface
                                :paused-menu pause-menu-surface
                                :sound-button sound-button-surface
                                :urm-button urm-button-surface
                                :volume-button volume-button-surface
                                :small-cloud small-clouds-surface
                                :big-cloud big-clouds-surface
                                :level-background level-background-surface
                                :health-power health-power-surface
                                :game-over game-over-surface
                                :level-completed level-completed-surface
                                :potions potions-surface
                                :crates crates-surface
                                :spikes spikes-surface
                                :cannon cannon-surface
                                :cannonball cannonball-surface
                                )
                               (all-level-data))))))))))

(defun main ()
  (call-with-sdl2-images '(:png)
    (lambda ()
      (sdl2:with-init (:video)
        (main-window)))))
