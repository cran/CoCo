
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the test-proto for the drag-graph-proto
;;; and adds the basic methods to this proto


;; Induce a circle:

;; (require "draggraph")


(defproto test-proto '(list-of-integers list-of-floats)
  '(instances-tests) (list *object*))

(send test-proto :documentation 'proto "Test prototype based on *object*")

(defmeth test-proto :isnew (test)
  (slot-value 'list-of-integers (car test))
  (slot-value 'list-of-floats (cadr test))
  (send test-proto :slot-value 'instances-tests
	(cons self (slot-value 'instances-tests)))
  )

(defmeth coco-proto :return-test-object (&optional (model-1 nil) (model-2 nil))
  (send self :print-models 'base)
  (send self :print-models 'current)
  (let ((test-result (if (objectp model-2)
			 (send model-2 :compute-test-against-model-object model-1)
		       (send self :compute-test model-1 model-2))))
    (if test-result (send test-proto :new test-result)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth test-proto :number-of-cases ()
  (nth 0 (slot-value 'list-of-integers)))

(defmeth test-proto :df ()
  (nth 1 (slot-value 'list-of-integers)))

(defmeth test-proto :adj ()
  (if (> (nth 2 (slot-value 'list-of-integers)) 0)
      (nth 2 (slot-value 'list-of-integers)) 0))

(defmeth test-proto :number-of-tables ()
  (nth 3 (slot-value 'list-of-integers)))

(defmeth test-proto :deviance ()
  (nth 0 (slot-value 'list-of-floats)))

(defmeth test-proto :p-exact-deviance ()
  (nth 1 (slot-value 'list-of-floats)))

(defmeth test-proto :square ()
  (nth 2 (slot-value 'list-of-floats)))

(defmeth test-proto :p-exact-square ()
  (nth 3 (slot-value 'list-of-floats)))

(defmeth test-proto :power ()
  (nth 4 (slot-value 'list-of-floats)))

(defmeth test-proto :p-exact-power ()
  (nth 5 (slot-value 'list-of-floats)))

(defmeth test-proto :gamma ()
  (nth 6 (slot-value 'list-of-floats)))

(defmeth test-proto :standard-deviation-of-gamma-0 ()
  (nth 7 (slot-value 'list-of-floats)))

(defmeth test-proto :standard-deviation-of-gamma-1 ()
  (nth 8 (slot-value 'list-of-floats)))

(defmeth test-proto :p-exact-gamma-1-sided ()
  (nth 9 (slot-value 'list-of-floats)))

(defmeth test-proto :p-exact-gamma-2-sided ()
  (nth 10 (slot-value 'list-of-floats)))

(defmeth test-proto :df-float ()
  (nth 11 (slot-value 'list-of-floats)))

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :format-p-value (p)
  (format nil "P = ~7,5f" (format-invalid p))
  )

(defmeth drag-graph-proto :p-to-width (p-value)
 (if (invalid-real p-value) 1
  (if (equalp p-value 0) 20
   (+ (+ 2 (floor (* (log (/ 1.00 0.20)) (/ 1 (log 2)))))
    (floor (* (log (/ 0.20 p-value)) (/ 1 (log 2)))))))
 )

(defmeth test-proto :select-p-value (&key (print-test nil))
  (if print-test (send self :print-test))
  (let ((number-of-cases  (nth 0 (slot-value 'list-of-integers)))
	(df               (nth 1 (slot-value 'list-of-integers)))
	(adj              (if (> (nth 2 (slot-value 'list-of-integers)) 0)
			      (nth 2 (slot-value 'list-of-integers)) 0))
	(number-of-tables (nth 3 (slot-value 'list-of-integers)))
	(deviance         (nth 0 (slot-value 'list-of-floats)))
	(e_deviance       (nth 1 (slot-value 'list-of-floats)))
	(square           (nth 2 (slot-value 'list-of-floats)))
	(e_square         (nth 3 (slot-value 'list-of-floats)))
	(power            (nth 4 (slot-value 'list-of-floats)))
	(e_power          (nth 5 (slot-value 'list-of-floats)))
	(gamma            (nth 6 (slot-value 'list-of-floats)))
	(gamma_s          (nth 7 (slot-value 'list-of-floats)))
	(gamma_s_1        (nth 8 (slot-value 'list-of-floats)))
	(e_gamma_1        (nth 9 (slot-value 'list-of-floats)))
	(e_gamma_2        (nth 10 (slot-value 'list-of-floats)))
	(df-float         (nth 11 (slot-value 'list-of-floats)))
	)
    (if (and (not (invalid-real gamma)) (> gamma -2) (< gamma 2))
     (if (and (not (invalid-real e_gamma_2)) (> number-of-tables 0)
          (> e_gamma_2 -1)) e_gamma_2
      (if (and (not (invalid-real gamma_s)) (> gamma_s 0))
       (* 2 (- 1 (normal-cdf (/ (abs gamma) (sqrt gamma_s)))))
       0))
     (if (and (not (invalid-real e_deviance)) (> number-of-tables 0)
          (> e_deviance -1)) e_deviance
      (if (not (or (invalid-real deviance) (invalid-integer df)
                (invalid-integer adj) (<= (- df adj) 0)))
       (- 1 (if (> (- df adj) 0)
             (chisq-cdf deviance (- df adj)) 0))
                                        #.NOT-A-NUMBER))))
  )

(defmeth coco-proto :select-p-value (test &key (print-test nil))
  (send test :select-p-value :print-test print-test)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth test-proto :print-test (&key (print-test nil))
  (let ((number-of-cases  (nth 0 (slot-value 'list-of-integers)))
	(df               (nth 1 (slot-value 'list-of-integers)))
	(adj              (if (> (nth 2 (slot-value 'list-of-integers)) 0)
			      (nth 2 (slot-value 'list-of-integers)) 0))
	(number-of-tables (nth 3 (slot-value 'list-of-integers)))
	(deviance         (nth 0 (slot-value 'list-of-floats)))
	(e_deviance       (nth 1 (slot-value 'list-of-floats)))
	(square           (nth 2 (slot-value 'list-of-floats)))
	(e_square         (nth 3 (slot-value 'list-of-floats)))
	(power            (nth 4 (slot-value 'list-of-floats)))
	(e_power          (nth 5 (slot-value 'list-of-floats)))
	(gamma            (nth 6 (slot-value 'list-of-floats)))
	(gamma_s          (nth 7 (slot-value 'list-of-floats)))
	(gamma_s_1        (nth 8 (slot-value 'list-of-floats)))
	(e_gamma_1        (nth 9 (slot-value 'list-of-floats)))
	(e_gamma_2        (nth 10 (slot-value 'list-of-floats)))
	(df-float         (nth 11 (slot-value 'list-of-floats))))
    (format t "Number of cases:  ~11d ~%" number-of-cases)
    (format t "                  ~11a ~11a ~11a "
	    "  Statistic" "Probability" "   Adjusted")
    (if (and (> number-of-tables 0) (> e_deviance -1))
	(format t " ~11a " "      Exact"))
    (flet ((print-line 
	    (x df adj exact title)
	    (format t "~%")
	    (format t "~17a ~11,3f ~11,5f ~11,5f " title 
		    (format-invalid x)
		    (if (not (or (invalid-real x) (invalid-integer df)
				 (<= df 0)))
			(- 1 (chisq-cdf x df)))
		    (if (not (or (invalid-real x) (invalid-integer df)
				 (invalid-integer adj) (<= (- df adj) 0)))
			(- 1 (chisq-cdf x (- df adj)))))
	    (if (and (> number-of-tables 0) (> exact -1))
		(format t " ~11,5f " exact))))
	  (print-line deviance df adj e_deviance "Deviance:")
	  (print-line power    df adj e_power    "Power divergence:")
	  (print-line square   df adj e_square   "Pearsons X^2:"))
    (format t "~%")
    (format t "DF.               ~11a ~11d ~11d " " "
	    (if (not (invalid-integer df))
		df)
	    (if (not (or (invalid-integer df) (invalid-integer adj)))
		(- df adj)))
    (format t "~%")
    (if (> number-of-tables 1)
	(format t "Number of table:  ~11d ~%" number-of-tables))
    (if (and (> gamma -2) (< gamma 2))
	(progn				; Goodman-Kruskal's
	  (format t  "Gamma:            ~11,3f ~11,5f ~11,5f " gamma
		  (if (> gamma_s 0)
		      (* 2 (- 1 (normal-cdf (/ (abs gamma)
					       (sqrt gamma_s  ))))) 0.0)
		  (if (> gamma_s_1 0)
		      (* 2 (- 1 (normal-cdf (/ (abs gamma)
					       (sqrt gamma_s_1))))) 0.0))
	  (format t  " ~11,5f "
		  (if (and (> number-of-tables 0) (>= e_gamma_1 0)) e_gamma_1))
	  (format t  " ~8,5f "
		  (if (and (> number-of-tables 0) (>= e_gamma_2 0)) e_gamma_2))
	  (format t  "~%"))))
  )

;;

(provide "tests")
