;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defun call-with-open-font (font-path size receiver)
  (let ((font nil))
    (unwind-protect
         (progn (setq font (sdl2-ttf:open-font font-path size))
                (funcall receiver font))
      (when font
        (sdl2-ttf:close-font font)))))

(defun call-with-rendered-text (font text r g b a receiver)
  (let ((surface nil))
    (unwind-protect
         (progn (setq surface (sdl2-ttf:render-text-solid font text r g b a))
                (funcall receiver surface))
      (when surface
        ;(sdl2:free-surface surface)
        ))))

(defun call-with-sdl2-images (formats thunk)
  "Initialize the SDL2 image library with the given FORMATS and call THUNK."
  (unwind-protect
       (progn (sdl2-image:init formats)
              (funcall thunk))
    (sdl2-image:quit)))

(defun call-with-image-surface (func pathname)
  (let ((surface nil))
    (unwind-protect
         (progn
           (setq surface (sdl2-image:load-image pathname))
           (funcall func surface))
      (when surface (sdl2:free-surface surface)))))

(defun call-with-image-surfaces (func &rest pathnames)
  (if (null pathnames)
      (funcall func)
      (call-with-image-surface
       (lambda (surface)
         (apply #'call-with-image-surfaces
          (lambda (&rest surfaces)
            (apply func (cons surface surfaces)))
          (cdr pathnames)))
       (car pathnames))))

(defun call-with-surface-texture (renderer func surface)
  (let ((texture nil))
    (unwind-protect
         (progn
           (setq texture (sdl2:create-texture-from-surface renderer surface))
           (funcall func texture))
      (when texture (sdl2:destroy-texture texture)))))

(defun call-with-surface-textures (renderer func &rest surfaces)
  (if (null surfaces)
      (funcall func)
      (call-with-surface-texture
       renderer
       (lambda (texture)
         (apply
          #'call-with-surface-textures
          renderer
          (lambda (&rest textures)
            (apply func (cons texture textures)))
          (cdr surfaces)))
       (car surfaces))))

(declaim (ftype (function ((function (t t &rest t) t) t t &rest t) t) fold-left))
(defun fold-left (function initial list &rest lists)
  (labels ((fold-left-1 (state item tail)
             (declare (optimize (debug 0) (safety 0) (speed 3)))
             (cond ((consp tail)
                    (fold-left-1 (funcall function state item) (car tail) (cdr tail)))
                   ((null tail) (funcall function state item))
                   (t (error "Dotted list encountered by fold-left."))))

           (fold-left-n (state items tails)
             (cond ((every #'consp tails)
                    (fold-left-n (apply function state items) (map 'list #'car tails) (map 'list #'cdr tails)))
                   ((every #'null tails) (apply function state items))
                   (t (error "Lists of different lengths or dotted list in fold-left.")))))

    (if (null lists)
        (cond ((consp list) (fold-left-1 initial (car list) (cdr list)))
              ((null list) initial)
              (t (error "Non list in fold-left.")))
        (let ((tails (cons list lists)))
          (cond ((every #'consp tails)
                 (fold-left-n initial (map 'list #'car tails) (map 'list #'cdr tails)))
                ((every #'null tails) initial)
                (t (error "Non list in fold-left.")))))))

