;;; -*- Lisp -*-

(defsystem "platformer"
  :depends-on ("named-let" "png-read" "sdl2" "sdl2-image" "sdl2-ttf" "series" "utilities")
  :components ((:file "package")
               (:file "macros" :depends-on ("package"))
               (:file "parameters" :depends-on ("macros" "package"))
               (:file "utilities" :depends-on ("macros" "package"))
               (:file "generics" :depends-on ("package"))
               (:file "animation" :depends-on ("generics" "macros" "package" "utilities"))
               (:file "texture-map" :depends-on ("package"))
               (:file "entity" :depends-on ("generics" "macros" "package" "parameters" "utilities"))
               (:file "crate" :depends-on ("entity" "generics" "macros" "package" "parameters" "utilities"))
               (:file "cannon" :depends-on ("crate" "entity" "generics" "macros" "package" "parameters" "utilities"))
               (:file "cannonball" :depends-on ("entity" "generics" "macros" "package" "parameters" "utilities"))
               (:file "potions" :depends-on ("entity" "generics" "macros" "package" "parameters" "utilities"))
               (:file "traps" :depends-on ("entity" "generics" "macros" "package" "parameters" "utilities"))
               (:file "level" :depends-on ("crate" "entity" "generics" "macros" "package" "parameters" "potions" "texture-map" "traps" "utilities"))
               (:file "game-menu" :depends-on ("generics" "macros" "package" "parameters" "texture-map" "utilities"))
               (:file "pause-menu" :depends-on ("generics" "macros" "package" "parameters" "texture-map" "utilities"))
               (:file "level-completed" :depends-on ("generics" "macros" "package" "parameters" "texture-map" "utilities"))
               (:file "game-over" :depends-on ("generics" "macros" "package" "parameters" "texture-map" "utilities"))
               (:file "main" :depends-on ("animation" "crate" "entity" "game-menu" "game-over" "generics" "macros" "package" "parameters" "pause-menu" "potions" "level" "level-completed" "texture-map" "traps" "utilities"))
               (:file "game" :depends-on ("animation" "entity" "generics" "main" "macros" "package" "parameters" "pause-menu" "level" "level-completed" "texture-map" "utilities"))))
