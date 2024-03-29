;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defun game-scale () 2.0)
(defun scalef (quantity) (* (game-scale) quantity))
(defun scale (quantity) (floor (scalef quantity)))
(defun width-in-tiles () 26)
(defun height-in-tiles () 14)
(defun-scaled tile-size 32)
(defun game-width () (* (tile-size) (width-in-tiles)))
(defun game-height () (* (tile-size) (height-in-tiles)))

(defun-scaled player-hitbox-width 20)
(defun-scaled player-hitbox-height 26)
(defun-scaled player-hitbox-y-offset -1)
(defun-scaled player-attackbox-width 20)
(defun-scaled player-attackbox-height 9)
(defun-scaled player-attackbox-right-x-offset 25)
(defun-scaled player-attackbox-left-x-offset -22)
(defun-scaled player-attackbox-y-offset 3)

(defun base-crabby-sprite-width () 72)
(defun base-crabby-sprite-height () 32)
(defun-scaled crabby-sprite-x-offset -24)
(defun-scaled crabby-sprite-y-offset -27)

(defun-scaled crabby-hitbox-width 26)
(defun-scaled crabby-hitbox-height 20)
(defun-scaled crabby-hitbox-y-offset -3)
(defun-scaled crabby-attackbox-width 69)
(defun-scaled crabby-attackbox-height 10)
(defun-scaled crabby-attackbox-x-offset -21)
(defun-scaled crabby-attackbox-y-offset 0)

(defun crabby-base-velocity () .35)
(defun crabby-velocity () (scalef (crabby-base-velocity)))

(defun-scaled status-bar-width 192)
(defun-scaled status-bar-height 58)
(defun-scaled status-bar-x 10)
(defun-scaled status-bar-y 10)
(defun-scaled health-bar-width 150)
(defun-scaled health-bar-height 4)
(defun-scaled health-bar-x 34)
(defun-scaled health-bar-y 14)

(defun-scaled level-complete-width 224)
(defun-scaled level-complete-height 204)

(defun-scaled level-complete-buttons-y-offset 120)
(defun-scaled level-complete-buttons-x-offset (/ (urm-button-width) 4))

(defun-scaled potion-width 12)
(defun-scaled potion-height 16)
(defun-scaled potion-x-offset -2)
(defun-scaled potion-y-offset -25)
(defun-scaled blue-potion-hitbox-width 7)
(defun-scaled blue-potion-width 7)
(defun-scaled blue-potion-height 12)
(defun-scaled blue-potion-hitbox-x-offset 0)
(defun-scaled blue-potion-hitbox-y-offset 10)
(defun-scaled red-potion-hitbox-width 10)
(defun-scaled red-potion-width 8)
(defun-scaled red-potion-height 15)
(defun-scaled red-potion-hitbox-x-offset 0)
(defun-scaled red-potion-hitbox-y-offset 8)

(defun-scaled crate-x-offset -7)
(defun-scaled crate-y-offset -29)
(defun-scaled crate-width 40)
(defun-scaled crate-height 30)

(defun-scaled chest-hitbox-width 25)
(defun-scaled chest-hitbox-height 17)
(defun-scaled chest-hitbox-x-offset 0)
(defun-scaled chest-hitbox-y-offset 0)

(defun-scaled barrel-hitbox-width 24)
(defun-scaled barrel-hitbox-height 24)
(defun-scaled barrel-hitbox-x-offset 0)
(defun-scaled barrel-hitbox-y-offset 0)

(defun-scaled spikes-x-offset 0)
(defun-scaled spikes-y-offset -32)
(defun-scaled spikes-width 32)
(defun-scaled spikes-height 32)
(defun-scaled spikes-hitbox-width 32)
(defun-scaled spikes-hitbox-height 16)
(defun-scaled spikes-hitbox-x-offset 0)
(defun-scaled spikes-hitbox-y-offset 0)

(defun-scaled cannon-x-offset 0)
(defun-scaled cannon-y-offset -26)
(defun-scaled cannon-width 40)
(defun-scaled cannon-height 26)
(defun-scaled cannon-hitbox-width 40)
(defun-scaled cannon-hitbox-height 26)
(defun-scaled cannon-hitbox-x-offset 0)
(defun-scaled cannon-hitbox-y-offset 0)

(defun-scaled cannonball-x-offset 0)
(defun-scaled cannonball-y-offset -15)
(defun-scaled cannonball-width 15)
(defun-scaled cannonball-height 15)
(defun-scaled cannonball-hitbox-width 9)
(defun-scaled cannonball-hitbox-height 9)
(defun-scaled cannonball-hitbox-x-offset 0)
(defun-scaled cannonball-hitbox-y-offset 0)

(defun base-gravity () 0.02)
(defun gravity () (scalef (base-gravity)))
(defun base-jump-velocity () 1.5)
(defun jump-velocity () (scalef (base-jump-velocity)))
(defun base-run-velocity () 1)
(defun run-velocity () (scalef (base-run-velocity)))

(defvar *world-x-offset* 0
  "How far the world has scrolled to the right in pixels.")
