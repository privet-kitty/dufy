;;;
;;; mb-line
;;;

(in-package #:dufy/internal)

(defstruct mb-line
  "Slope-intercept representation of a line"
  (slope 0.0d0 :type double-float)
  (intercept 1.0d0 :type double-float))

(defun mb-line-ray-intersect-distance (theta line)
  "Return the (signed) distance at which a ray, starting at the origin and travelling at angle THETA, intersects LINE."
  (declare (inline)
           (double-float theta)
           (mb-line line))
  (/ (mb-line-intercept line)
     (- (sin theta) (* (mb-line-slope line) (cos theta)))))

(defun mb-line-distance-from-origin (line)
  "Return the distance from the line to the origin."
  (/ (abs (mb-line-intercept line))
     (sqrt (+ (square (mb-line-slope line)) 1.0d0))))

(export (list 'make-mb-line 'mb-line-ray-intersect-distance 'mb-line-distance-from-origin))
