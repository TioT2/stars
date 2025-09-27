;;;; COMMON LISP implementation of 'stars' test project (not finished yet)

; Require SDL2 library
(ql:quickload :sdl2)

; Define 3-compmonent vector structure
; Structure is picked (instead of a class) to improve performance
(defstruct vec3 x y z)

; Add vectors
(defun vec3-add (lhs rhs)
  (make-vec3
    :x (+ (vec3-x lhs) (vec3-x rhs))
    :y (+ (vec3-y lhs) (vec3-y rhs))
    :z (+ (vec3-z lhs) (vec3-z rhs))))

; Get dot product
(defun vec3-dot (lhs rhs)
  (+
    (* (vec3-x lhs) (vec3-x rhs))
    (* (vec3-y lhs) (vec3-y rhs))
    (* (vec3-z lhs) (vec3-z rhs))))

; Multiply vector to constant
(defun vec3-mul-c (v c)
  (make-vec3
    :x (* (vec3-x v) c)
    :y (* (vec3-x v) c)
    :z (* (vec3-x v) c)))


; Define timer structure
(defstruct timer
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

; Construct timer for initial performance counter and frequency
(defun timer-ctor (perf-frequency perf-counter)
  (make-timer
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

; Update timer with new counter value
(defun timer-update (self counter)

  ; Recalculate duration into float
  (defun duration-of (begin end) (/ (- end begin) (timer-perf-freq self)))

  (setf (timer-delta-time self) (duration-of (timer-perf-counter-curr self) counter))
  (setf (timer-time self) (duration-of (timer-perf-counter-init self) counter))

  ; Increment frame counter
  (incf (timer-fps-frame-count timer))

  ; uugh readability
  (let ((fps-update-required (> (- counter (timer-fps-last-measure self)) (timer-fps-duration self))))
    (if fps-update-required
      (block
        ; Update timer FPS
        (setf (timer-fps self)
          (/
            (timer-fps-frame-count self)
            (/
              (- counter (timer-fps-last-measure self))
              (timer-fps-frequency self))))
        (setf (timer-fps-last-measure self) counter)
        (setf (timer-fps-frame-count self) 0))
      nil)))

;; Main function
(defun main ()
  (sdl2:init :video)

  ; Construct window
  (sdl2:with-window (window :title "stars-lisp")
    ; Access surface and fill it with yellow
    (let ((screen-surface (sdl2:get-window-surface window)))

      ; Fill surface with yellow rectangle
      (sdl2:fill-rect
        screen-surface
        nil
        (sdl2:map-rgb (sdl2:surface-format screen-surface) 255 255 0)))

    ; Update window
    (sdl2:update-window window)
    
    ; Wait!
    (sdl2:delay 5000))

  (sdl2:quit))
