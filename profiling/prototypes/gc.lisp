(in-package :maxima)
;; Larger nursery: 4x the default 5% of dynamic space.
(setf (sb-ext:bytes-consed-between-gcs) (* 4 (sb-ext:bytes-consed-between-gcs)))
