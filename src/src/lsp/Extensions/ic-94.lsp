
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

(defmeth coco-graph-window-proto :select-p-value (test &key (print-test nil))
  (if print-test (print-test test))
  (let ((number-of-cases  (nth 0 (car test)))
	(df               (nth 1 (car test)))
	(adj              (if (> (nth 2 (car test)) 0)
			      (nth 2 (car test)) 0))
	(number-of-tables (nth 3 (car test)))
	(deviance         (nth 0 (cadr test)))
	(e_deviance       (nth 1 (cadr test)))
	(square           (nth 2 (cadr test)))
	(e_square         (nth 3 (cadr test)))
	(power            (nth 4 (cadr test)))
	(e_power          (nth 5 (cadr test)))
	(gamma            (nth 6 (cadr test)))
	(gamma_s          (nth 7 (cadr test)))
	(gamma_s_1        (nth 8 (cadr test)))
	(e_gamma_1        (nth 9 (cadr test)))
	(e_gamma_2        (nth 10 (cadr test)))
	(df-real          (nth 11 (cadr test))))
    (if *use-ic*
	(if (and (not (numberp *use-ic*))
		 (or (invalid-real deviance)
		     (invalid-integer df) (invalid-integer adj)
		     (invalid-integer number-of-cases)
		     (= number-of-cases 0)))
	    0
          (- (- deviance (* (- df adj) (if (numberp *use-ic*) *use-ic*
					 (log number-of-cases))))))
      (if (and (> gamma -2) (< gamma 2))
	  (if (and (> number-of-tables 0) (> e_gamma_2 -1)) e_gamma_2
	    (if (> gamma_s 0)
		(* 2 (- 1 (normal-cdf (/ (abs gamma) (sqrt gamma_s))))) 0))
	(if (and (> number-of-tables 0) (> e_deviance -1)) e_deviance
	  (if (not (or (invalid-real deviance) (invalid-integer df)
		       (invalid-integer adj) (<= (- df adj) 0)))
	      (- 1 (chisq-cdf deviance (- df adj))) 0)))))
  )

(defmeth coco-graph-window-proto :p-to-width (p-value)
  (if *use-ic*
      (1+ (length
	   (which
	    (< p-value
	       (list -320 -160 -80 -40 -20 -10 -5 0 5 10 20 40 80)))))
    (if (equalp p-value 0) 20
      (+ (+ 2 (floor (* (log (/ 1.00 0.20)) (/ 1 (log 2)))))
	 (floor (* (log (/ 0.20 p-value)) (/ 1 (log 2)))))))
  )


(defmeth coco-graph-window-proto :format-p-value (p) 
  (if *use-ic*
      (if (numberp *use-ic*)
	(format nil "(A)IC = ~7,5f" p)
      (format nil "BIC = ~7,5f" p))
    (format nil "P = ~7,5f" p))
  )
