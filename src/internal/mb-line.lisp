;;;
;;; mb-line
;;;

(in-package #:dufy/internal)

(defstruct mb-line
  "Slope-intercept representation of a line"
  (slope 0.0d0 :type double-float)
  (intercept 1.0d0 :type double-float))

(declaim (inline mb-line-ray-intersect-distance))
(defun mb-line-ray-intersect-distance (theta line)
  "Return the (signed) distance at which a ray, starting at the origin and travelling at angle THETA, intersects LINE."
  (declare (optimize (speed 3) (safety 1))
           (double-float theta)
           (mb-line line))
  (/ (mb-line-intercept line)
     (- (sin theta) (* (mb-line-slope line) (cos theta)))))

(declaim (inline mb-line-distance-from-origin))
(defun mb-line-distance-from-origin (line)
  "Return the distance from the line to the origin."
  (declare (optimize (speed 3) (safety 1))
           (mb-line line))
  (/ (abs (mb-line-intercept line))
     (sqrt (+ (square (mb-line-slope line)) 1.0d0))))
