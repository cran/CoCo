
(defun invalid-real (x)
  (or (eq x #.NOT-A-NUMBER)
   (not (< x #.POSITIVE-INFINITY))
;;   (< (abs (/ (- x *my-not-a-number*) *my-not-a-number*)) 0.0001)
   )
  )

(defun invalid-real (x)
  (if (or (eq x #.NOT-A-NUMBER) (eq x #.POSITIVE-INFINITY)) T
    (not (< x #.POSITIVE-INFINITY))
    )
  )

(defun invalid-real (x)
  (if (or T ;; (equal x #.NOT-A-NUMBER) 
          (equal x #.POSITIVE-INFINITY)) T
    (not (< x #.POSITIVE-INFINITY))
    )
  )

(defun replace-my-not-a-number (real-list)
  (mapcar #'(lambda (i) (if (invalid-real i) nil i))
   ;; #.NOT-A-NUMBER
   ;; #.POSITIVE-INFINITY
   real-list)
  )

(defun replace-my-not-a-number (real-list)
(setf rl real-list)
  (mapcar #'(lambda (i) (if (invalid-real i) -1000 i))
   ;; #.NOT-A-NUMBER
   ;; #.POSITIVE-INFINITY
   real-list)
  )

(trace :compute-test)
(trace :print-test)
(trace format)
;; (trace invalid-real)
(trace replace-my-not-a-number)
