(in-package :clcl)

;; CIE76
(defun deltae (l1 a1 b1 l2 a2 b2)
  (let ((deltal (- l1 l2))
	(deltaa (- a1 a2))
	(deltab (- b1 b2)))
    (sqrt (+ (* deltal deltal)
	     (* deltaa deltaa)
	     (* deltab deltab)))))

(defun xyz-deltae (x1 y1 z1 x2 y2 z2 &optional (illuminant d65))
  (destructuring-bind (l1 a1 b1) (xyz-to-lab x1 y1 z1 illuminant)
    (destructuring-bind (l2 a2 b2) (xyz-to-lab x2 y2 z2 illuminant)
      (deltae l1 a1 b1 l2 a2 b2))))

(defun rgb255-deltae (r1 g1 b1 r2 g2 b2 &optional (rgbspace srgbd65))
  (destructuring-bind (x1 y1 z1) (rgb255-to-xyz r1 g1 b1 rgbspace)
    (destructuring-bind (x2 y2 z2) (rgb255-to-xyz r2 g2 b2 rgbspace)
      (xyz-deltae x1 y1 z1 x2 y2 z2 (rgbspace-illuminant rgbspace)))))
