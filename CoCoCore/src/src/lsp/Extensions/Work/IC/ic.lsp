


;;  Dear Ronja,

;;  By the command (trace :select-p-value) before the selection
;;  you can check you model-selection.  Now I have also modified the
;;  method :print-test to print the used IC-value, but note that the
;;  printed values are ``independent'' of what is used (:select-p-value)
;;  in the model-selection.

;;  Regards,

;;  Jens Henrik


;;  Dear Ronja,

;;  No you are not right, that in CoCo the AIC in a block-backward-selection
;;  is not working.  I will give you a English translation of the following
;;  found in ../CoCo.../lsp/src/ic.lsp:

;;  It is describe on page 116 as on of the examples that the user can program.
;;  You have not found that, thus I will give you a piece of code that can
;;  handle both p-values and IC-values. Save this letter in a file ending on
;;  `.lsp', e.g. `ic.lsp', and read the file into xlisp+coco by (load "ic").

;;  If you do not change *use-ic* from `nil', then p-values are used.
;;  Do you change  *use-ic* to T by (def *use-ic* T), then BIC is used,
;;  and by (def *use-ic* 2) you will get AIC.

;;  Note that local test has to be used, see page 117 and exercise 1.

;;  Regards,

;;  Jens Henrik



;;  Kaere Soren,

;;  Det er beskrevet side 116 som et af de eksempler, som brugeren selv kan
;;  programmere. Da du aabenbart ikke har set det, saa faar du er en stump
;;  kode, der kan haandtere baade p-vaerdier og IC-vaerdier. Gem dette
;;  brev i en fil, hvis navn ender paa `.lsp', f.eks. `ic.lsp', og laes
;;  den ind i xlisp+coco med (load "ic").  Hvis saa du ikke aendre
;;  variablen *use-ic* fra `nil', saa andvendes p-vaerdier.
;;  saettes *usde-ic* til T med (def *use-ic* T) saa anvendes BIC,
;;  og med f.eks. (def *use-ic* 2) faar du AIC.

;;  Bemaerk, at du skal anvende lokale tests, som beskrevet oeverst side 117
;;  og i opgave 1.

;;  /jhb

;;  Ps. Brevet skrevet her vil vaere kommentarer i lisp-filen.


(def *use-ic* nil)


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
   (if *use-ic*
	(if (and (not (numberp *use-ic*))
		 (or (invalid-real deviance)
		     (invalid-integer df) (invalid-integer adj)
		     (invalid-integer number-of-cases)
		     (= number-of-cases 0)))
	    0
          (- (- deviance (* (- df adj) (if (numberp *use-ic*) *use-ic*
					 (log number-of-cases))))))
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
                                        #.NOT-A-NUMBER))))))

(defmeth coco-graph-window-proto :p-to-width (p-value)
 (if (invalid-real p-value) 1
  (if *use-ic*
   (1+ (length
        (which
         (< p-value
          (list -320 -160 -80 -40 -20 -10 -5 0 5 10 20 40 80)))))
   (if (equalp p-value 0) 20
    (+ (+ 2 (floor (* (log (/ 1.00 0.20)) (/ 1 (log 2)))))
     (floor (* (log (/ 0.20 p-value)) (/ 1 (log 2))))))))
 )

(defmeth coco-graph-window-proto :format-p-value (p) 
  (if *use-ic*
      (if (numberp *use-ic*)
	(format nil "(A)IC = ~7,5f" p)
      (format nil "BIC = ~7,5f" p))
    (format nil "P = ~7,5f" p))
  )

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
	    (format t "~17a ~11,3f ~11,5f ~11,5f " title x
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
	  (format t  "~%")))
;;;   (if *use-ic*
;;;    (progn
;;;     (format t  "IC, AIC           ~11,5f "
;;;      (- (- deviance (* (- df adj)
;;;                      (if (numberp *use-ic*) *use-ic* 2)))))
;;;     (format t  "~%")
;;;     (format t  "IC, BIC           ~11,5f "
;;;      (- (- deviance (* (- df adj) (log number-of-cases)))))
;;;     (format t  "~%")
;;;     (format t  "IC, Used          ~11,5f "
;;;      (- (- deviance (* (- df adj)
;;;                      (if (numberp *use-ic*) *use-ic*
;;;                       (log number-of-cases))))))
;;;     (format t  "~%")
;;;     ))
   (if *use-ic*
    (progn
     (if (numberp *use-ic*)
       (format t  "IC, AIC       ( ~11,5f ) ~11,5f "
        *use-ic* (- (- deviance (* (- df adj) *use-ic* 2))))
       (format t  "IC, BIC       ( ~11,5f ) ~11,5f "
        (log number-of-cases)
        (- (- deviance (* (- df adj) (log number-of-cases))))))
     (format t  "~%")))
   )
 )
