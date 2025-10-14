;;; main.lisp --- COMMON LISP implementation of 'stars' test project (in development)

;; Define project main package, export 'main' function
(defpackage #:stars
  (:use :cl)
  (:export :main))

;; Set package namespace
(in-package #:stars)

(defstruct vec3
  "Define 3-component vector structure."
  x y z)

(defun vec3-add (lhs rhs)
  "Calculate sum of 3-component vectors"
  (make-vec3
    :x (+ (vec3-x lhs) (vec3-x rhs))
    :y (+ (vec3-y lhs) (vec3-y rhs))
    :z (+ (vec3-z lhs) (vec3-z rhs))))

(defun vec3-dot (lhs rhs)
  "Calculate dot product of 3-component vectors"
  (+
    (* (vec3-x lhs) (vec3-x rhs))
    (* (vec3-y lhs) (vec3-y rhs))
    (* (vec3-z lhs) (vec3-z rhs))))

(defun vec3-mul-c (v c)
  "Calculate per-component product of 3-component vectors"
  (make-vec3
    :x (* (vec3-x v) c)
    :y (* (vec3-x v) c)
    :z (* (vec3-x v) c)))

(defun vec3-from-spherical (phi theta)
  "Construct vector from spherical coordinates"
  (make-vec3
   :x (* (cos phi) (sin theta))
   :y (* (sin phi) (sin theta))
   :z (cos theta)))

(defstruct time-controller
  "Define structure that manages time in the project
  (it is not possible to name it \"timer\", so...)"
  perf-freq
  perf-counter-init
  perf-counter-curr
  curr-delta-time
  curr-time
  fps-frame-count
  fps-duration
  fps-last-measure
  fps
  fps-is-new)

(defun time-controller-ctor (perf-frequency perf-counter)
  "Construct timer for initial performance counter and frequency"
  (make-time-controller
    :perf-freq perf-frequency
    :perf-counter-init perf-counter
    :perf-counter-curr perf-counter
    :curr-delta-time 0.001
    :curr-time 0
    :fps-frame-count 0
    :fps-duration (* perf-frequency 3)
    :fps-last-measure perf-counter
    :fps 0.0
    :fps-is-new nil))

(defun time-controller-update (self counter)
  "Update timer with new time value (**must be** > previous counter)"

  (defun duration-of (begin end)
    "Calculate duration (in seconds) elapsed from begin until end"
    (/ (- end begin) (time-controller-perf-freq self)))

  (setf
   (time-controller-curr-delta-time self)
   (duration-of (time-controller-perf-counter-curr self) counter))

  (setf
   (time-controller-curr-time self)
   (duration-of (time-controller-perf-counter-init self) counter))

  ;; Increment frame counter
  (incf (time-controller-fps-frame-count self))

  (let ((fps-update-required (> (- counter
                                   (time-controller-fps-last-measure self))
                                (time-controller-fps-duration self))))
    (if fps-update-required
      (block update-time-controller-fps
        ;; Update timer FPS
        (setf (time-controller-fps self)
          (/
            (time-controller-fps-frame-count self)
              (duration-of counter (time-controller-fps-last-measure self))))

        ;; Update FPS counter and frame count
        (setf (time-controller-fps-last-measure self) counter)
        (setf (time-controller-fps-frame-count self) 1))
      nil)))

(defun splitmix64 (n)
  "Generate next splitmix64 number"
  (declare (type (unsigned-byte 64) n))

  (let ((uint64-max #xFFFFFFFFFFFFFFFF))
    (setf n (logand uint64-max (+ n #x9E3779B97F4A7C15)))
    (setf n (logand uint64-max (* (logxor n (ash n -30)) #xBF58476D1CE4E5B9)))
    (setf n (logand uint64-max (* (logxor n (ash n -27)) #x94D049BB133111EB)))
    (setf n (logxor n (ash n -31)))
    n))

(defun random-ctor (seed)
  "Construct xorshift random generator"
  (let* ((s0 (splitmix64 seed))
         (s1 (splitmix64 s0))
         (s2 (splitmix64 s1))
         (s3 (splitmix64 s2)))
    (make-array 4 :initial-contents (list s0 s1 s2 s3))))

(defun random-uint64 (self)
  "Generate next uint64 by xorshift rand"

  (let ((uint64-max #xFFFFFFFFFFFFFFFF))
    (flet ((rotate-l (x n)
             "Uint64 Bitwise left rotation function"
             (logand uint64-max (logior (ash x n) (ash x (- 64 n))))))
      (let ((result (+ (aref self 0) (rotate-l (+ (aref self 0) (aref self 1)) 23)))
            (temp (logand uint64-max (ash (aref self 1) 17))))

        (setf (aref self 2) (logxor (aref self 2) (aref self 0)))
        (setf (aref self 3) (logxor (aref self 3) (aref self 1)))
        (setf (aref self 0) (logxor (aref self 0) (aref self 2)))
        (setf (aref self 1) (logxor (aref self 1) (aref self 3)))
        (setf (aref self 2) (logxor (aref self 2) temp))
        (setf (aref self 3) (rotate-l (aref self 3) 45))
        result))))

(defun random-float (self)
  "Generate next unit floating-point number with xorshift random generator"
  (let ((u (random-uint64 self)))
    (float (/ (coerce u 'double-float) (coerce #xFFFFFFFFFFFFFFFF 'double-float)))))

(defun random-unit-vec3 (self)
  "Generate random 3-component vector evenly distributed on unit sphere surface"
  (let ((ksi1 (random-float self))
        (ksi2 (random-float self)))
    (vec3-from-spherical
     (* ksi1 2.0f0 (coerce pi 'single-float))
     (acos (- (* 2.0f0 ksi2) 1.0f0)))))

(defun random-sphere-vec3 (self)
  "Generate random 3-component vector evenly distributed in unit sphere"
  (loop
    (let ((v (make-vec3
              :x (random-float self)
              :y (random-float self)
              :z (random-float self))))
      (when (<= (vec3-dot v v) 1.0f0) (return v)))))

(defstruct input-state
  "Descriptor of some input"
  (rotation 0.0f0)
  (acceleration 0.0f0)
  (move-x 0.0f0)
  (move-y 0.0f0))

(defun input-compose (self input)
  "Compose self with another input value"
  (macrolet ((handle-field (f)
               `(setf (,f self) (max (min (+ (,f self) (,f input)) 1.0f0) -1.0f0))))
    (handle-field input-state-rotation)
    (handle-field input-state-acceleration)
    (handle-field input-state-move-x)
    (handle-field input-state-move-y)))

(defstruct context
  "Application context structure"
  random
  stars
  input
  timer
  speed)

(defun context-ctor ()
  "Construct application context"
  (let* ((random-generator (random-ctor 47))
         (stars (make-array 8192
                            :element-type 'vec3
                            :initial-element (make-vec3 :x 0.0f0 :y 0.0f0 :z 0.0f0)))
         (timer (time-controller-ctor
                 (sdl2:get-performance-counter)
                 (sdl2:get-performance-frequency))))

    ;; Generate stars
    (loop for i from 0 below 8192 do
      (setf (aref stars i) (random-sphere-vec3 random-generator)))

    ;; Construct context
    (make-context
      :random (random-ctor 47)
      :stars (make-array 8192)
      :input (make-input-state)
      :timer timer
      :speed 1.0f0)))

;; Add constant to all stars
(defun context-move-stars (self offset)
  "Move stars by some offset vector"
  (loop for star in (context-stars self) do
    (setf star (vec3-add star offset))
    (when (> (vec3-dot star star) 1.0)
      (setf star (random-unit-vec3 (context-random self)))
      (setf star (vec3-mul-c star (signum (vec3-dot star offset)))))))

(defun main ()
  "Main project function"
  (sdl2:init :video)
  (sdl2:with-window (window :title "stars-lisp")
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:idle ()
        ;; Access surface and fill it with yellow
        (let ((surface (sdl2:get-window-surface window)))
          ;; Fill surface with yellow rectangle
          (sdl2:fill-rect
           surface
           nil
           (sdl2:map-rgb (sdl2:surface-format surface) 255 255 0)))
        ;; Update window
        (sdl2:update-window window))))
  (sdl2:quit))

;; main.lisp ends here
