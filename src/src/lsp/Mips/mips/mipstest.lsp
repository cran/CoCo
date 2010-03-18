
(defun print-test (test)
  (let ((number-of-cases  (nth 0 (car test)))
	(df               (nth 1 (car test)))
	(adj              (if (> (nth 2 (car test)) 0) (nth 2 (car test)) 0))
	(number-of-tables (nth 3 (car test)))
	(f-test-df        (nth 4 (car test)))
	(deviance         (nth 0 (cadr test)))
	(e-deviance       (nth 1 (cadr test)))
	(square           (nth 2 (cadr test)))
	(e-square         (nth 3 (cadr test)))
	(power            (nth 4 (cadr test)))
	(e-power          (nth 5 (cadr test)))
	(gamma            (nth 6 (cadr test)))
	(gamma-s          (nth 7 (cadr test)))
	(gamma-s-1        (nth 8 (cadr test)))
	(e-gamma-1        (nth 9 (cadr test)))
	(e-gamma-2        (nth 10 (cadr test)))
	(df-real          (nth 11 (cadr test)))
	(e-f-test         (nth 12 (cadr test))))
    (format t "Number of cases:  ~11d ~%" number-of-cases)
    (format t "                  ~11a ~11a ~11a "
	    "  Statistic" "Probability" "   Adjusted")
    (if (and (> number-of-tables 0) (> e-deviance -1))
	(format t " ~11a " "      Exact"))
    (flet ((print-line 
	    (x df adj exact title)
	    (format t "~%")
	    (format t "~17a ~11,3f ~11,5f ~11,5f " title x
		    (if (not (or (invalid-real x) (invalid-integer df)
				 (<= df 0)))
			(- 1 (chisq-cdf x df)))
		    (if (not (or (invalid-real x) (invalid-integer df)
				 (invalid-integer adj) (<= (- df adj) 0)))
			(- 1 (chisq-cdf x (- df adj)))))
	    (if (and (> number-of-tables 0) (> exact -1))
		(format t " ~11,5f " exact))))
	  (print-line deviance df adj e-deviance "Deviance:")
	  (print-line power    df adj e-power    "Power divergence:")
	  (print-line square   df adj e-square   "Pearsons X^2:"))
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
	(progn      ; Goodman-Kruskal's
	  (format t  "Gamma:            ~11,3f ~11,5f ~11,5f " gamma
		  (if (> gamma-s 0)
		      (* 2 (- 1 (normal-cdf (/ (abs gamma)
					       (sqrt gamma-s  ))))) 0.0)
		  (if (> gamma-s-1 0)
		      (* 2 (- 1 (normal-cdf (/ (abs gamma)
					       (sqrt gamma-s-1))))) 0.0))
	  (format t  " ~11,5f "
		  (if (and (> number-of-tables 0) (>= e-gamma-1 0)) e-gamma-1))
	  (format t  " ~8,5f "
		  (if (and (> number-of-tables 0) (>= e-gamma-2 0)) e-gamma-2))
	  (format t  "~%"))))
  )

(defun print-deviance (x)
  (let ((dimension-current         (nth 0 (car x)))
	(dimension-base            (nth 1 (car x)))
	(df                        (nth 2 (car x)))
	(expected-zeros-in-base    (nth 3 (car x)))
	(observed-zeros-in-current (nth 4 (car x)))
	(observed-zeros-in-base    (nth 5 (car x)))
	(adjustment                (nth 6 (car x)))
	(df-adjusted               (nth 7 (car x)))
	(t-test-df                 (nth 8 (car x)))
	(log-l-current             (nth 0 (cadr x)))
	(log-l-base                (nth 1 (cadr x)))
	(deviance                  (nth 2 (cadr x)))
	(probability               (nth 3 (cadr x)))
	(adjusted-probability      (nth 4 (cadr x)))
	(f-test-statistic          (nth 5 (cadr x)))
	(f-test-p-value            (nth 6 (cadr x))))
    (format t  "Deviance:         ~12,3f ~12,5f ~12,5f ~%"
	    deviance probability adjusted-probability))
  )

(defmeth test-proto :select-p-value (&key (print-test nil))
  (if print-test (send self :print-test))
  (let ((number-of-cases  (nth 0 (slot-value 'list-of-integers)))
	(df               (nth 1 (slot-value 'list-of-integers)))
	(adj              (if (> (nth 2 (slot-value 'list-of-integers)) 0)
			      (nth 2 (slot-value 'list-of-integers)) 0))
	(number-of-tables (nth 3 (slot-value 'list-of-integers)))
	(f-test-df        (nth 4 (slot-value 'list-of-floats)))
	(deviance         (nth 0 (slot-value 'list-of-floats)))
	(e-deviance       (nth 1 (slot-value 'list-of-floats)))
	(square           (nth 2 (slot-value 'list-of-floats)))
	(e-square         (nth 3 (slot-value 'list-of-floats)))
	(power            (nth 4 (slot-value 'list-of-floats)))
	(e-power          (nth 5 (slot-value 'list-of-floats)))
	(gamma            (nth 6 (slot-value 'list-of-floats)))
	(gamma-s          (nth 7 (slot-value 'list-of-floats)))
	(gamma-s-1        (nth 8 (slot-value 'list-of-floats)))
	(e-gamma-1        (nth 9 (slot-value 'list-of-floats)))
	(e-gamma-2        (nth 10 (slot-value 'list-of-floats)))
	(df-float         (nth 11 (slot-value 'list-of-floats)))
	(e-f-test         (nth 12 (slot-value 'list-of-floats)))
	)
    (if (and (not (invalid-real gamma)) (> gamma -2) (< gamma 2))
     (if (and (not (invalid-real e-gamma-2)) (> number-of-tables 0)
          (> e-gamma-2 -1)) e-gamma-2
      (if (and (not (invalid-real gamma-s)) (> gamma-s 0))
       (* 2 (- 1 (normal-cdf (/ (abs gamma) (sqrt gamma-s)))))
       0))
     (if (and (not (invalid-real e-deviance)) (> number-of-tables 0)
          (> e-deviance -1)) e-deviance
      (if (not (or (invalid-real deviance) (invalid-integer df)
                (invalid-integer adj) (<= (- df adj) 0)))
       (- 1 (if (> (- df adj) 0)
             (chisq-cdf deviance (- df adj)) 0))
                                        #.NOT-A-NUMBER))))
  )


(defmeth test-proto :print-test (&key (print-test nil))
  (let ((number-of-cases  (nth 0 (slot-value 'list-of-integers)))
	(df               (nth 1 (slot-value 'list-of-integers)))
	(adj              (if (> (nth 2 (slot-value 'list-of-integers)) 0)
			      (nth 2 (slot-value 'list-of-integers)) 0))
	(number-of-tables (nth 3 (slot-value 'list-of-integers)))
	(f-test-df        (nth 4 (slot-value 'list-of-integers)))
	(deviance         (nth 0 (slot-value 'list-of-floats)))
	(e-deviance       (nth 1 (slot-value 'list-of-floats)))
	(square           (nth 2 (slot-value 'list-of-floats)))
	(e-square         (nth 3 (slot-value 'list-of-floats)))
	(power            (nth 4 (slot-value 'list-of-floats)))
	(e-power          (nth 5 (slot-value 'list-of-floats)))
	(gamma            (nth 6 (slot-value 'list-of-floats)))
	(gamma-s          (nth 7 (slot-value 'list-of-floats)))
	(gamma-s-1        (nth 8 (slot-value 'list-of-floats)))
	(e-gamma-1        (nth 9 (slot-value 'list-of-floats)))
	(e-gamma-2        (nth 10 (slot-value 'list-of-floats)))
	(df-float         (nth 11 (slot-value 'list-of-floats)))
	(e-f-test         (nth 12 (slot-value 'list-of-floats)))
        )
    (format t "Number of cases:  ~11d ~%" number-of-cases)
    (format t "                  ~11a ~11a ~11a "
	    "  Statistic" "Probability" "   Adjusted")
    (if (and (> number-of-tables 0) (> e-deviance -1))
	(format t " ~11a " "      Exact"))
    (flet ((print-line 
	    (x df adj exact title)
	    (format t "~%")
	    (format t "~17a ~11,3f ~11,5f ~11,5f " title x
		    (if (not (or (invalid-real x) (invalid-integer df)
				 (<= df 0)))
			(- 1 (chisq-cdf x df)))
		    (if (not (or (invalid-real x) (invalid-integer df)
				 (invalid-integer adj) (<= (- df adj) 0)))
			(- 1 (chisq-cdf x (- df adj)))))
	    (if (and (> number-of-tables 0) (> exact -1))
		(format t " ~11,5f " exact))))
	  (print-line deviance df adj e-deviance "Deviance:")
	  (print-line power    df adj e-power    "Power divergence:")
	  (print-line square   df adj e-square   "Pearsons X^2:"))
    (format t "~%")
    (format t "DF.               ~11a ~11d ~11d " " "
	    (if (not (invalid-integer df))
		df)
	    (if (not (or (invalid-integer df) (invalid-integer adj)))
		(- df adj)))
    (format t "~%")
    (format t "F-test df.        ~11d ~11a ~11a "
	    (if (not (invalid-integer f-test-df))
		f-test-df)
	     " " " ")
    (format t "~%")
    (if (> number-of-tables 1)
	(format t "Number of table:  ~11d ~%" number-of-tables))
    (if (and (> gamma -2) (< gamma 2))
	(progn				; Goodman-Kruskal's
	  (format t  "Gamma:            ~11,3f ~11,5f ~11,5f " gamma
		  (if (> gamma-s 0)
		      (* 2 (- 1 (normal-cdf (/ (abs gamma)
					       (sqrt gamma-s  ))))) 0.0)
		  (if (> gamma-s-1 0)
		      (* 2 (- 1 (normal-cdf (/ (abs gamma)
					       (sqrt gamma-s-1))))) 0.0))
	  (format t  " ~11,5f "
		  (if (and (> number-of-tables 0) (>= e-gamma-1 0)) e-gamma-1))
	  (format t  " ~8,5f "
		  (if (and (> number-of-tables 0) (>= e-gamma-2 0)) e-gamma-2))
	  (format t  "~%"))))
  )