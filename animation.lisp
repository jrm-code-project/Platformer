;;; -*- Lisp -*-

(in-package "PLATFORMER")

(defclass animation-atlas ()
  ((texture-key    :initarg :texture-key     :reader get-texture-key)
   (cell-x-offset  :initarg :cell-x-offset   :reader get-cell-x-offset)
   (cell-y-offset  :initarg :cell-y-offset   :reader get-cell-y-offset)
   (cell-width     :initarg :cell-width      :reader get-cell-width)
   (cell-height    :initarg :cell-height     :reader get-cell-height)
   (state-vector   :initarg :state-vector    :reader get-state-vector)
   (frame-count    :initarg :frame-count     :reader %get-frame-count)
   (tick-per-frame :initarg :ticks-per-frame :reader get-ticks-per-frame)))

(defgeneric get-frame-count (atlas state))

(defmethod get-frame-count ((animation-atlas animation-atlas) state)
  (svref (%get-frame-count animation-atlas)
         (position state (get-state-vector animation-atlas))))

(defclass animation ()
  ((atlas :initarg :atlas :reader   get-atlas)
   (row   :initarg :row   :accessor get-row)))

(defgeneric get-frame (animation))

(defun render-animation! (renderer texture-map animation x y flip)
  (let ((atlas (get-atlas animation)))
    (multiple-value-bind (column row) (get-frame animation)
      (when column
        (sdl2:with-rects ((src (* column (get-cell-width atlas))
                               (* row (get-cell-height atlas))
                               (get-cell-width atlas)
                               (get-cell-height atlas))
                          (dst (floor (+ (get-cell-x-offset atlas) x))
                               (floor (+ (get-cell-y-offset atlas) y))
                               (scale (get-cell-width atlas))
                               (scale (get-cell-height atlas))))
          (sdl2:render-copy-ex renderer
                               (funcall (get-texture-key atlas) texture-map)
                               :source-rect src
                               :dest-rect dst
                               :flip (if flip
                                         '(:horizontal)
                                         '(:none))))))))

(defclass frame-loop (animation)
  ((start-tick :initform (sdl2:get-ticks) :accessor get-start-tick)))

(defmethod get-frame ((frame-loop frame-loop))
  (let* ((atlas (get-atlas frame-loop))
         (row   (get-row   frame-loop))
         (frame-count (get-frame-count atlas row)))
    (values
     (if (= frame-count 1)
         0
         (let* ((dticks (- (sdl2:get-ticks) (get-start-tick frame-loop)))
                (frames (floor dticks (get-ticks-per-frame atlas))))
           (mod frames frame-count)))
     (position row (get-state-vector atlas)))))

(defun get-animation-iteration (frame-loop)
  (let* ((atlas (get-atlas frame-loop))
         (row   (get-row   frame-loop))
         (frame-count (get-frame-count atlas row))
         (dticks (- (sdl2:get-ticks) (get-start-tick frame-loop)))
         (frames (floor dticks (get-ticks-per-frame atlas))))
    (floor frames frame-count)))

(defclass one-shot (animation)
  ((start-tick :initform nil :accessor get-start-tick)))

(defmethod get-frame ((one-shot one-shot))
  (let ((atlas (get-atlas one-shot)))
    (values
     (if (null (get-start-tick one-shot))
        0
        (let* ((dticks (- (sdl2:get-ticks) (get-start-tick one-shot)))
               (frame (ceiling dticks (get-ticks-per-frame atlas))))
          (unless (>= frame (get-frame-count atlas (get-row one-shot)))
            frame)))
     (position (get-row one-shot) (get-state-vector atlas)))))

(defun trigger-one-shot! (one-shot)
  (setf (get-start-tick one-shot) (sdl2:get-ticks)))

(defun reset-one-shot! (one-shot)
  (setf (get-start-tick one-shot) nil))

(defclass one-shot-loop (one-shot)
  ())

(defmethod get-frame ((one-shot-loop one-shot-loop))
  (let ((atlas (get-atlas one-shot-loop)))
    (values
     (if (null (get-start-tick one-shot-loop))
         0
         (let* ((dticks (- (sdl2:get-ticks) (get-start-tick one-shot-loop)))
                (frame (ceiling dticks (get-ticks-per-frame atlas))))
           (if (>= frame (get-frame-count atlas (get-row one-shot-loop)))
               0
               frame)))
     (position (get-row one-shot-loop) (get-state-vector atlas)))))
