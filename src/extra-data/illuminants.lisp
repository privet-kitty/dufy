;;;
;;; Supplemental data of standard illuminants
;;; All the illuminants are defined under DUFY:+OBS-CIE1931+ (2-degree observer)
;;; (originally contributed by wasserwerk)
;;;

(in-package :dufy/extra-data)

;; The white points of illuminants A, B, C, and D in dufy are
;; calculated with 5 nm intervals. (See the comment in the file with
;; the same name in dufy/core package.)
(defparameter +illum-d55+
  (make-illuminant :spectrum (gen-illum-d-spectrum 5500 :rectify t)
                   :begin-wl 380 :end-wl 780 :band 5
                   :compile-time t))

(defparameter +illum-d75+
  (make-illuminant :spectrum (gen-illum-d-spectrum 7500 :rectify t)
                   :begin-wl 380 :end-wl 780 :band 5
                   :compile-time t))


;;; illuminant B
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +illum-b-table+
    (make-array 97
                :element-type 'double-float
                :initial-contents '(0.0d0 0.0d0 0.0d0 0.0d0 0.02d0 0.26d0 0.50d0 1.45d0 2.40d0 4.00d0 5.60d0 7.60d0 9.60d0 12.40d0 15.20d0 18.8d0 22.40d0 26.85d0 31.30d0 36.18d0 41.30d0 46.62d0 52.10d0 57.70d0 63.20d0 68.37d0 73.10d0 77.31d0 80.80d0 83.44d0 85.40d0 86.88d0 88.30d0 90.08d0 92.00d0 93.75d0 95.20d0 96.23d0 96.50d0 95.71d0 94.20d0 92.37d0 90.70d0 89.95d0 89.50d0 90.43d0 92.20d0 94.46d0 96.90d0 99.16d0 101.00d0 102.20d0 102.80d0 102.92d0 102.60d0 101.90d0 101.00d0 100.07d0 99.20d0 98.44d0 98.00d0 98.08d0 98.50d0 99.06d0 99.70d0 100.36d0 101.00d0 101.56d0 102.20d0 103.05d0 103.90d0 104.59d0 105.00d0 105.08d0 104.90d0 104.55d0 103.90d0 102.84d0 101.60d0 100.38d0 99.10d0 97.70d0 96.20d0 94.60d0 92.90d0 91.10d0 89.40d0 88.00d0 86.90d0 85.90d0 85.20d0 84.80d0 84.70d0 84.90d0 85.40d0 86.10d0 87.00d0))))

(defparameter +illum-b+
  (make-illuminant :spectrum (gen-spectrum +illum-b-table+ 300 780)
                   :begin-wl 380 :end-wl 780 :band 5
                   :compile-time t))


;;; F series (without 3.x)

;;; Wavelength range for SPD is from 380 nm to 780 nm. Unfortunately,
;;; I could not find any sources with a range from 300 nm to 830 nm.
;;; 
;;; Source for SPD:
;;; 
;;; Source 1:
;;; https://archive.org/details/gov.law.cie.15.2004
;;; 
;;; Source 2:
;;; https://www.rit.edu/science/pocs/useful-data
;;; http://www.rit-mcsl.org/UsefulData/Fluorescents.xls
;;; 
;;; Source 2 seems to have some typos. See the value for F10 for 385 nm -
;;; the same value as for F11 in next columns.
;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +illum-f1-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(1.87d0 2.36d0 2.94d0 3.47d0 5.17d0 19.49d0 6.13d0 6.24d0 7.01d0 7.79d0 8.56d0 43.67d0 16.94d0 10.72d0 11.35d0 11.89d0 12.37d0 12.75d0 13d0 13.15d0 13.23d0 13.17d0 13.13d0 12.85d0 12.52d0 12.2d0 11.83d0 11.5d0 11.22d0 11.05d0 11.03d0 11.18d0 11.53d0 27.74d0 17.05d0 13.55d0 14.33d0 15.01d0 15.52d0 18.29d0 19.55d0 15.48d0 14.91d0 14.15d0 13.22d0 12.19d0 11.12d0 10.03d0 8.95d0 7.96d0 7.02d0 6.2d0 5.42d0 4.73d0 4.15d0 3.64d0 3.2d0 2.81d0 2.47d0 2.18d0 1.93d0 1.72d0 1.67d0 1.43d0 1.29d0 1.19d0 1.08d0 0.96d0 0.88d0 0.81d0 0.77d0 0.75d0 0.73d0 0.68d0 0.69d0 0.64d0 0.68d0 0.69d0 0.61d0 0.52d0 0.43d0)))

  (defparameter +illum-f2-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(1.18d0 1.48d0 1.84d0 2.15d0 3.44d0 15.69d0 3.85d0 3.74d0 4.19d0 4.62d0 5.06d0 34.98d0 11.81d0 6.27d0 6.63d0 6.93d0 7.19d0 7.4d0 7.54d0 7.62d0 7.65d0 7.62d0 7.62d0 7.45d0 7.28d0 7.15d0 7.05d0 7.04d0 7.16d0 7.47d0 8.04d0 8.88d0 10.01d0 24.88d0 16.64d0 14.59d0 16.16d0 17.56d0 18.62d0 21.47d0 22.79d0 19.29d0 18.66d0 17.73d0 16.54d0 15.21d0 13.8d0 12.36d0 10.95d0 9.65d0 8.4d0 7.32d0 6.31d0 5.43d0 4.68d0 4.02d0 3.45d0 2.96d0 2.55d0 2.19d0 1.89d0 1.64d0 1.53d0 1.27d0 1.1d0 0.99d0 0.88d0 0.76d0 0.68d0 0.61d0 0.56d0 0.54d0 0.51d0 0.47d0 0.47d0 0.43d0 0.46d0 0.47d0 0.4d0 0.33d0 0.27d0)))

  (defparameter +illum-f3-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(0.82d0 1.02d0 1.26d0 1.44d0 2.57d0 14.36d0 2.7d0 2.45d0 2.73d0 3d0 3.28d0 31.85d0 9.47d0 4.02d0 4.25d0 4.44d0 4.59d0 4.72d0 4.8d0 4.86d0 4.87d0 4.85d0 4.88d0 4.77d0 4.67d0 4.62d0 4.62d0 4.73d0 4.99d0 5.48d0 6.25d0 7.34d0 8.78d0 23.82d0 16.14d0 14.59d0 16.63d0 18.49d0 19.95d0 23.11d0 24.69d0 21.41d0 20.85d0 19.93d0 18.67d0 17.22d0 15.65d0 14.04d0 12.45d0 10.95d0 9.51d0 8.27d0 7.11d0 6.09d0 5.22d0 4.45d0 3.8d0 3.23d0 2.75d0 2.33d0 1.99d0 1.7d0 1.55d0 1.27d0 1.09d0 0.96d0 0.83d0 0.71d0 0.62d0 0.54d0 0.49d0 0.46d0 0.43d0 0.39d0 0.39d0 0.35d0 0.38d0 0.39d0 0.33d0 0.28d0 0.21d0)))

  (defparameter +illum-f4-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(0.57d0 0.7d0 0.87d0 0.98d0 2.01d0 13.75d0 1.95d0 1.59d0 1.76d0 1.93d0 2.1d0 30.28d0 8.03d0 2.55d0 2.7d0 2.82d0 2.91d0 2.99d0 3.04d0 3.08d0 3.09d0 3.09d0 3.14d0 3.06d0 3d0 2.98d0 3.01d0 3.14d0 3.41d0 3.9d0 4.69d0 5.81d0 7.32d0 22.59d0 15.11d0 13.88d0 16.33d0 18.68d0 20.64d0 24.28d0 26.26d0 23.28d0 22.94d0 22.14d0 20.91d0 19.43d0 17.74d0 16d0 14.42d0 12.56d0 10.93d0 9.52d0 8.18d0 7.01d0 6d0 5.11d0 4.36d0 3.69d0 3.13d0 2.64d0 2.24d0 1.91d0 1.7d0 1.39d0 1.18d0 1.03d0 0.88d0 0.74d0 0.64d0 0.54d0 0.49d0 0.46d0 0.42d0 0.37d0 0.37d0 0.33d0 0.35d0 0.36d0 0.31d0 0.26d0 0.19d0)))

  (defparameter +illum-f5-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(1.87d0 2.35d0 2.92d0 3.45d0 5.1d0 18.91d0 6d0 6.11d0 6.85d0 7.58d0 8.31d0 40.76d0 16.06d0 10.32d0 10.91d0 11.4d0 11.83d0 12.17d0 12.4d0 12.54d0 12.58d0 12.52d0 12.47d0 12.2d0 11.89d0 11.61d0 11.33d0 11.1d0 10.96d0 10.97d0 11.16d0 11.54d0 12.12d0 27.78d0 17.73d0 14.47d0 15.2d0 15.77d0 16.1d0 18.54d0 19.5d0 15.39d0 14.64d0 13.72d0 12.69d0 11.57d0 10.45d0 9.35d0 8.29d0 7.32d0 6.41d0 5.63d0 4.9d0 4.26d0 3.72d0 3.25d0 2.83d0 2.49d0 2.19d0 1.93d0 1.71d0 1.52d0 1.48d0 1.26d0 1.13d0 1.05d0 0.96d0 0.85d0 0.78d0 0.72d0 0.68d0 0.67d0 0.65d0 0.61d0 0.62d0 0.59d0 0.62d0 0.64d0 0.55d0 0.47d0 0.4d0)))

  (defparameter +illum-f6-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(1.05d0 1.31d0 1.63d0 1.9d0 3.11d0 14.8d0 3.43d0 3.3d0 3.68d0 4.07d0 4.45d0 32.61d0 10.74d0 5.48d0 5.78d0 6.03d0 6.25d0 6.41d0 6.52d0 6.58d0 6.59d0 6.56d0 6.56d0 6.42d0 6.28d0 6.2d0 6.19d0 6.3d0 6.6d0 7.12d0 7.94d0 9.07d0 10.49d0 25.22d0 17.46d0 15.63d0 17.22d0 18.53d0 19.43d0 21.97d0 23.01d0 19.41d0 18.56d0 17.42d0 16.09d0 14.64d0 13.15d0 11.68d0 10.25d0 8.95d0 7.74d0 6.69d0 5.71d0 4.87d0 4.16d0 3.55d0 3.02d0 2.57d0 2.2d0 1.87d0 1.6d0 1.37d0 1.29d0 1.05d0 0.91d0 0.81d0 0.71d0 0.61d0 0.54d0 0.48d0 0.44d0 0.43d0 0.4d0 0.37d0 0.38d0 0.35d0 0.39d0 0.41d0 0.33d0 0.26d0 0.21d0)))

  (defparameter +illum-f7-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(2.56d0 3.18d0 3.84d0 4.53d0 6.15d0 19.37d0 7.37d0 7.05d0 7.71d0 8.41d0 9.15d0 44.14d0 17.52d0 11.35d0 12d0 12.58d0 13.08d0 13.45d0 13.71d0 13.88d0 13.95d0 13.93d0 13.82d0 13.64d0 13.43d0 13.25d0 13.08d0 12.93d0 12.78d0 12.6d0 12.44d0 12.33d0 12.26d0 29.52d0 17.05d0 12.44d0 12.58d0 12.72d0 12.83d0 15.46d0 16.75d0 12.83d0 12.67d0 12.45d0 12.19d0 11.89d0 11.6d0 11.35d0 11.12d0 10.95d0 10.76d0 10.42d0 10.11d0 10.04d0 10.02d0 10.11d0 9.87d0 8.65d0 7.27d0 6.44d0 5.83d0 5.41d0 5.04d0 4.57d0 4.12d0 3.77d0 3.46d0 3.08d0 2.73d0 2.47d0 2.25d0 2.06d0 1.9d0 1.75d0 1.62d0 1.54d0 1.45d0 1.32d0 1.17d0 0.99d0 0.81d0)))

  (defparameter +illum-f8-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(1.21d0 1.5d0 1.81d0 2.13d0 3.17d0 13.08d0 3.83d0 3.45d0 3.86d0 4.42d0 5.09d0 34.1d0 12.42d0 7.68d0 8.6d0 9.46d0 10.24d0 10.84d0 11.33d0 11.71d0 11.98d0 12.17d0 12.28d0 12.32d0 12.35d0 12.44d0 12.55d0 12.68d0 12.77d0 12.72d0 12.6d0 12.43d0 12.22d0 28.96d0 16.51d0 11.79d0 11.76d0 11.77d0 11.84d0 14.61d0 16.11d0 12.34d0 12.53d0 12.72d0 12.92d0 13.12d0 13.34d0 13.61d0 13.87d0 14.07d0 14.2d0 14.16d0 14.13d0 14.34d0 14.5d0 14.46d0 14d0 12.58d0 10.99d0 9.98d0 9.22d0 8.62d0 8.07d0 7.39d0 6.71d0 6.16d0 5.63d0 5.03d0 4.46d0 4.02d0 3.66d0 3.36d0 3.09d0 2.85d0 2.65d0 2.51d0 2.37d0 2.15d0 1.89d0 1.61d0 1.32d0)))

  (defparameter +illum-f9-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(0.9d0 1.12d0 1.36d0 1.6d0 2.59d0 12.8d0 3.05d0 2.56d0 2.86d0 3.3d0 3.82d0 32.62d0 10.77d0 5.84d0 6.57d0 7.25d0 7.86d0 8.35d0 8.75d0 9.06d0 9.31d0 9.48d0 9.61d0 9.68d0 9.74d0 9.88d0 10.04d0 10.26d0 10.48d0 10.63d0 10.78d0 10.96d0 11.18d0 27.71d0 16.29d0 12.28d0 12.74d0 13.21d0 13.65d0 16.57d0 18.14d0 14.55d0 14.65d0 14.66d0 14.61d0 14.5d0 14.39d0 14.4d0 14.47d0 14.62d0 14.72d0 14.55d0 14.4d0 14.58d0 14.88d0 15.51d0 15.47d0 13.2d0 10.57d0 9.18d0 8.25d0 7.57d0 7.03d0 6.35d0 5.72d0 5.25d0 4.8d0 4.29d0 3.8d0 3.43d0 3.12d0 2.86d0 2.64d0 2.43d0 2.26d0 2.14d0 2.02d0 1.83d0 1.61d0 1.38d0 1.12d0)))

  (defparameter +illum-f10-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(1.11d0 0.8d0 0.62d0 0.57d0 1.48d0 12.16d0 2.12d0 2.7d0 3.74d0 5.14d0 6.75d0 34.39d0 14.86d0 10.4d0 10.76d0 10.67d0 10.11d0 9.27d0 8.29d0 7.29d0 7.91d0 16.64d0 16.73d0 10.44d0 5.94d0 3.34d0 2.35d0 1.88d0 1.59d0 1.47d0 1.8d0 5.71d0 40.98d0 73.69d0 33.61d0 8.24d0 3.38d0 2.47d0 2.14d0 4.86d0 11.45d0 14.79d0 12.16d0 8.97d0 6.52d0 8.31d0 44.12d0 34.55d0 12.09d0 12.15d0 10.52d0 4.43d0 1.95d0 2.19d0 3.19d0 2.77d0 2.29d0 2d0 1.52d0 1.35d0 1.47d0 1.79d0 1.74d0 1.02d0 1.14d0 3.32d0 4.49d0 2.05d0 0.49d0 0.24d0 0.21d0 0.21d0 0.24d0 0.24d0 0.21d0 0.17d0 0.21d0 0.22d0 0.17d0 0.12d0 0.09d0)))

  (defparameter +illum-f11-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(0.91d0 0.63d0 0.46d0 0.37d0 1.29d0 12.68d0 1.59d0 1.79d0 2.46d0 3.33d0 4.49d0 33.94d0 12.13d0 6.95d0 7.19d0 7.12d0 6.72d0 6.13d0 5.46d0 4.79d0 5.66d0 14.29d0 14.96d0 8.97d0 4.72d0 2.33d0 1.47d0 1.1d0 0.89d0 0.83d0 1.18d0 4.9d0 39.59d0 72.84d0 32.61d0 7.52d0 2.83d0 1.96d0 1.67d0 4.43d0 11.28d0 14.76d0 12.73d0 9.74d0 7.33d0 9.72d0 55.27d0 42.58d0 13.18d0 13.16d0 12.26d0 5.11d0 2.07d0 2.34d0 3.58d0 3.01d0 2.48d0 2.14d0 1.54d0 1.33d0 1.46d0 1.94d0 2d0 1.2d0 1.35d0 4.1d0 5.58d0 2.51d0 0.57d0 0.27d0 0.23d0 0.21d0 0.24d0 0.24d0 0.2d0 0.24d0 0.32d0 0.26d0 0.16d0 0.12d0 0.09d0)))

  (defparameter +illum-f12-table+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(0.96d0 0.64d0 0.4d0 0.33d0 1.19d0 12.48d0 1.12d0 0.94d0 1.08d0 1.37d0 1.78d0 29.05d0 7.9d0 2.65d0 2.71d0 2.65d0 2.49d0 2.33d0 2.1d0 1.91d0 3.01d0 10.83d0 11.88d0 6.88d0 3.43d0 1.49d0 0.92d0 0.71d0 0.6d0 0.63d0 1.1d0 4.56d0 34.4d0 65.4d0 29.48d0 7.16d0 3.08d0 2.47d0 2.27d0 5.09d0 11.96d0 15.32d0 14.27d0 11.86d0 9.28d0 12.31d0 68.53d0 53.02d0 14.67d0 14.38d0 14.71d0 6.46d0 2.57d0 2.75d0 4.18d0 3.44d0 2.81d0 2.42d0 1.64d0 1.36d0 1.49d0 2.14d0 2.34d0 1.42d0 1.61d0 5.04d0 6.98d0 3.19d0 0.71d0 0.3d0 0.26d0 0.23d0 0.28d0 0.28d0 0.21d0 0.17d0 0.21d0 0.19d0 0.15d0 0.1d0 0.05d0))))


(defparameter +illum-f1+
  (make-illuminant :spectrum (gen-spectrum +illum-f1-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f2+
  (make-illuminant :spectrum (gen-spectrum +illum-f2-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f3+
  (make-illuminant :spectrum (gen-spectrum +illum-f3-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f4+
  (make-illuminant :spectrum (gen-spectrum +illum-f4-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f5+
  (make-illuminant :spectrum (gen-spectrum +illum-f5-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f6+
  (make-illuminant :spectrum (gen-spectrum +illum-f6-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f7+
  (make-illuminant :spectrum (gen-spectrum +illum-f7-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f8+
  (make-illuminant :spectrum (gen-spectrum +illum-f8-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f9+
  (make-illuminant :spectrum (gen-spectrum +illum-f9-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f10+
  (make-illuminant :spectrum (gen-spectrum +illum-f10-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f11+
  (make-illuminant :spectrum (gen-spectrum +illum-f11-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))
(defparameter +illum-f12+
  (make-illuminant :spectrum (gen-spectrum +illum-f12-table+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t))

