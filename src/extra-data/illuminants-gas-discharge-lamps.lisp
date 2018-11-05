;;;
;;; Illuminants data of several gasdischarge lamps
;;; (based on "Measuring Color" 4th edition by R. W. G. Hunt and M. R. Pointer)
;;;

(in-package :dufy-extra-data)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +illum-sox-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(0.1d0 0.0d0 0.0d0 0.1d0 0.0d0 0.2d0 0.0d0 0.0d0 0.0d0 0.1d0 0.1d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.1d0 0.1d0 0.0d0 0.0d0 0.1d0 0.0d0 0.3d0 0.7d0 0.0d0 0.0d0 0.2d0 0.1d0 0.0d0 0.1d0 0.1d0 0.1d0 0.1d0 0.1d0 0.2d0 0.2d0 2.1d0 8.1d0 1.3d0 1.4d0 131.8d0 1000.0d0 150.6d0 2.0d0 1.5d0 1.3d0 3.5d0 1.9d0 0.6d0 0.6d0 0.8d0 1.8d0 0.6d0 0.7d0 0.4d0 0.3d0 0.2d0 0.5d0 0.2d0 0.0d0 0.0d0 0.1d0 0.5d0 0.2d0 0.6d0 0.1d0 0.0d0 0.1d0 0.2d0 0.0d0 0.0d0 0.2d0 0.0d0 0.4d0 0.2d0 0.1d0 23.6d0 55.8d0 11.1d0 0.0d0)))
  (defparameter +illum-hp1-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(1.90d0 2.20d0 2.50d0 2.70d0 3.10d0 4.30d0 3.80d0 4.20d0 4.80d0 5.19d0 5.89d0 7.39d0 7.89d0 5.69d0 12.89d0 6.69d0 4.30d0 20.78d0 12.99d0 6.69d0 1.40d0 1.50d0 3.20d0 18.18d0 56.24d0 2.90d0 2.10d0 13.39d0 2.10d0 2.00d0 2.20d0 2.30d0 2.60d0 5.10d0 11.39d0 15.48d0 20.78d0 55.64d0 254.03d0 56.14d0 111.78d0 297.98d0 142.55d0 334.84d0 189.40d0 117.78d0 79.92d0 108.09d0 46.85d0 38.16d0 32.47d0 28.37d0 25.37d0 22.98d0 20.38d0 19.78d0 17.78d0 16.78d0 19.18d0 17.98d0 13.69d0 9.99d0 8.19d0 7.59d0 6.99d0 6.79d0 6.49d0 6.39d0 6.09d0 5.99d0 5.79d0 5.79d0 5.79d0 5.79d0 6.39d0 5.99d0 5.59d0 31.97d0 27.87d0 5.89d0 6.69d0)))
  (defparameter +illum-hp2-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(2.64d0 2.77d0 3.42d0 3.68d0 4.33d0 5.50d0 5.94d0 7.20d0 9.02d0 10.27d0 12.48d0 16.82d0 16.04d0 15.26d0 22.58d0 20.07d0 15.13d0 25.27d0 28.04d0 15.99d0 10.40d0 11.10d0 13.44d0 22.62d0 49.71d0 17.21d0 17.12d0 27.26d0 20.02d0 21.54d0 23.36d0 25.66d0 29.69d0 43.12d0 98.3d0 125.6d0 134.57d0 149.7d0 166.12d0 98.77d0 30.47d0 1.17d0 0.39d0 1.65d0 21.41d0 76.11d0 126.16d0 161.96d0 160.06d0 158.19d0 153.69d0 147.40d0 140.60d0 134.92d0 127.59d0 124.65d0 118.02d0 113.94d0 118.1d0 115.16d0 102.85d0 90.54d0 83.34d0 79.44d0 76.97d0 74.85d0 73.12d0 71.51d0 70.13d0 69.04d0 67.48d0 66.70d0 66.31d0 65.14d0 65.70d0 64.79d0 64.10d0 83.04d0 86.25d0 63.93d0 64.92d0)))
  (defparameter +illum-mb-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(5.14d0 4.28d0 7.49d0 5.26d0 7.71d0 121.05d0 61.57d0 5.73d0 3.67d0 4.29d0 6.26d0 168.75d0 140.57d0 5.84d0 3.86d0 3.22d0 2.84d0 2.72d0 2.74d0 3.00d0 2.77d0 3.10d0 5.24d0 6.11d0 2.88d0 2.58d0 2.39d0 2.58d0 2.28d0 2.43d0 2.53d0 3.67d0 4.29d0 195.18d0 181.63d0 6.93d0 3.74d0 3.50d0 3.79d0 100.00d0 292.87d0 39.62d0 4.23d0 3.38d0 2.74d0 2.97d0 2.78d0 3.10d0 2.65d0 3.51d0 2.67d0 2.91d0 2.66d0 2.76d0 2.66d0 2.78d0 2.68d0 2.81d0 2.96d0 3.18d0 2.75d0 2.95d0 6.51d0 5.19d0 2.82d0 3.17d0 4.29d0 3.22d0 2.76d0 2.97d0 2.74d0 2.97d0 2.71d0 2.97d0 2.63d0 3.00d0 2.67d0 3.10d0 2.99d0 3.86d0 2.64d0)))
  (defparameter +illum-mbf-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(4.53d0 3.91d0 6.68d0 4.81d0 7.73d0 107.15d0 54.92d0 6.08d0 4.33d0 4.97d0 7.15d0 146.23d0 123.51d0 6.82d0 4.66d0 3.79d0 3.46d0 3.27d0 3.49d0 3.70d0 3.58d0 3.94d0 6.04d0 6.39d0 3.73d0 3.49d0 3.57d0 3.75d0 3.67d0 3.88d0 4.38d0 6.74d0 9.93d0 198.10d0 171.97d0 10.57d0 7.55d0 6.61d0 6.76d0 100.00d0 273.65d0 42.59d0 15.05d0 27.64d0 13.65d0 10.75d0 24.34d0 83.81d0 149.80d0 58.07d0 17.99d0 10.51d0 8.09d0 7.86d0 10.98d0 10.76d0 7.25d0 6.29d0 6.06d0 5.91d0 5.18d0 5.48d0 9.83d0 18.35d0 46.48d0 35.63d0 15.82d0 5.92d0 4.61d0 4.48d0 4.11d0 4.23d0 3.92d0 4.08d0 3.84d0 4.13d0 3.67d0 3.99d0 3.79d0 4.61d0 3.41d0)))
  (defparameter +illum-mbtf-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(4.73d0 4.01d0 6.40d0 4.85d0 6.77d0 102.94d0 54.53d0 5.97d0 4.19d0 4.82d0 6.75d0 144.91d0 131.86d0 7.00d0 4.96d0 4.28d0 4.08d0 4.05d0 4.34d0 4.68d0 4.68d0 5.20d0 7.27d0 8.03d0 5.59d0 5.55d0 5.73d0 6.16d0 6.24d0 6.70d0 7.29d0 9.43d0 12.21d0 195.59d0 178.84d0 14.46d0 11.60d0 11.26d0 11.92d0 100.00d0 265.84d0 47.39d0 19.07d0 28.62d0 18.67d0 16.68d0 26.49d0 69.19d0 119.79d0 53.70d0 23.74d0 18.90d0 18.23d0 18.56d0 21.14d0 21.58d0 19.31d0 18.98d0 19.18d0 19.68d0 19.43d0 20.20d0 24.27d0 30.79d0 51.26d0 43.56d0 29.82d0 23.00d0 22.22d0 22.86d0 22.84d0 23.63d0 23.41d0 24.35d0 24.30d0 26.32d0 25.18d0 26.40d0 26.22d0 27.96d0 27.08d0)))
  (defparameter +illum-hmi-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(116.39d0 114.92d0 115.62d0 107.07d0 108.19d0 123.74d0 125.42d0 123.88d0 125.56d0 116.39d0 89.57d0 122.69d0 120.03d0 71.29d0 65.93d0 73.25d0 80.53d0 72.48d0 66.65d0 67.82d0 66.60d0 59.40d0 60.43d0 62.92d0 64.03d0 68.89d0 57.96d0 57.28d0 54.24d0 50.74d0 52.25d0 53.26d0 56.32d0 114.85d0 119.19d0 59.24d0 56.91d0 62.96d0 65.18d0 100.00d0 158.96d0 81.58d0 82.42d0 72.41d0 74.44d0 62.19d0 61.75d0 61.00d0 60.76d0 62.91d0 55.78d0 50.61d0 52.97d0 49.83d0 46.82d0 47.96d0 56.13d0 53.38d0 67.89d0 73.25d0 47.32d0 50.08d0 44.96d0 56.34d0 69.68d0 36.16d0 28.41d0 27.15d0 28.21d0 34.95d0 34.94d0 27.68d0 24.19d0 22.61d0 21.79d0 30.47d0 33.75d0 36.16d0 32.39d0 26.55d0 24.38d0)))
  (defparameter +illum-xenon-arr+
    (make-array 81
                :element-type 'double-float
                :initial-contents '(93.03d0 94.59d0 96.33d0 100.56d0 102.81d0 100.40d0 100.84d0 101.45d0 102.57d0 101.94d0 101.29d0 101.54d0 103.74d0 103.67d0 110.30d0 112.78d0 116.52d0 129.56d0 141.07d0 126.45d0 115.94d0 118.42d0 111.75d0 113.67d0 105.17d0 103.88d0 102.90d0 102.71d0 102.29d0 101.90d0 101.54d0 101.47d0 101.12d0 101.43d0 100.98d0 100.75d0 100.44d0 100.28d0 100.19d0 100.00d0 99.86d0 100.47d0 100.33d0 99.04d0 97.17d0 96.65d0 96.84d0 98.50d0 100.21d0 99.91d0 97.80d0 96.84d0 98.92d0 99.18d0 101.99d0 98.78d0 97.14d0 98.43d0 100.30d0 101.29d0 101.97d0 109.55d0 110.88d0 102.08d0 94.49d0 93.05d0 98.10d0 106.20d0 98.24d0 95.69d0 101.73d0 108.38d0 102.62d0 100.28d0 97.33d0 97.45d0 101.71d0 137.21d0 105.22d0 84.41d0 80.55d0))))

(defparameter +illum-sox+
  (make-illuminant :spectrum (gen-spectrum +illum-sox-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "Low pressure sodium lamp SOX")
(defparameter +illum-hp1+
  (make-illuminant :spectrum (gen-spectrum +illum-hp1-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "Standard high pressure sodium lamp HP1")
(defparameter +illum-hp2+
  (make-illuminant :spectrum (gen-spectrum +illum-hp2-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "Color enhanced high pressure sodium lamp HP2")
(defparameter +illum-mb+
  (make-illuminant :spectrum (gen-spectrum +illum-mb-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "High pressure mercury lamp type MB")
(defparameter +illum-mbf+
  (make-illuminant :spectrum (gen-spectrum +illum-mbf-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "High pressure mercury lamp type MBF")
(defparameter +illum-mbtf+
  (make-illuminant :spectrum (gen-spectrum +illum-mbtf-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "High pressure mercury lamp type MBTF")
(defparameter +illum-hmi+
  (make-illuminant :spectrum (gen-spectrum +illum-hmi-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "High pressure mercury lamp type HMI")
(defparameter +illum-xenon+
  (make-illuminant :spectrum (gen-spectrum +illum-xenon-arr+ 380 780)
                   :begin-wl 380 :end-wl 780
                   :compile-time t)
  "Xenon lamp")





