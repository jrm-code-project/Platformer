;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass game-over () ())

(defmethod game-step! (game (game-state game-over) dticks) nil)

(defmethod render-game-state! (renderer texture-map game (game-state game-over))
  (render-game-state! renderer texture-map game (get-level game))
  (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x7f)
  (sdl2:with-rects ((dst 0 0 (game-width) (game-height)))
    (sdl2:set-render-draw-blend-mode renderer :blend)
    (sdl2:render-fill-rect renderer dst)
    (sdl2:set-render-draw-blend-mode renderer :none))
  (let ((game-over (texture/game-over texture-map)))
    (sdl2:with-rects ((src 0 0 (sdl2:texture-width game-over) (sdl2:texture-height game-over))
                      (dst (floor (- (/ (game-width) 2) (/ (sdl2:texture-width game-over) 2)))
                           (floor (- (/ (game-height) 2) (/ (sdl2:texture-height game-over) 2)))
                           (sdl2:texture-width game-over)
                           (sdl2:texture-height game-over)))
      (sdl2:render-copy renderer game-over :source-rect src :dest-rect dst))))
