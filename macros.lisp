;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defmacro defun-scaled (parameter value)
  (let ((base-name (intern (concatenate 'string "BASE-" (symbol-name parameter)))))
    `(PROGN
       (DEFUN ,base-name () ,value)
       (DEFUN ,parameter () (SCALE (,base-name))))))

(defmacro let-surface (bindings &body body)
  `(call-with-image-surfaces
    (lambda ,(map 'list #'car bindings)
      ,@body)
    ,@(map 'list (lambda (binding)
                   `(progn ,@(cdr binding)))
           bindings)))

(defmacro let-texture ((renderer &rest bindings) &body body)
  `(call-with-surface-textures
    ,renderer
    (lambda ,(map 'list #'car bindings)
      ,@body)
    ,@(map 'list (lambda (binding)
                   `(progn ,@(cdr binding)))
           bindings)))
