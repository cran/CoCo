
;;; Copyright 1992, 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the coco-proto and the coco-model-proto
;;; and adds methods to those protos.


(setf *coco-proto-version* "1.4")

(require "cocoapix")


(defproto coco-proto '(identification title) '(instances-coco))

(defmeth coco-proto :end ()
  (coco-end :coco-id (slot-value 'identification)))

(defmeth coco-proto :resume ()
  (coco-resume :coco-id (slot-value 'identification)))

(defmeth coco-proto :isnew (coco-id &key title)
  (slot-value 'identification coco-id)
  (if title (send self :title title))
  (send coco-proto :slot-value 'instances-coco
	(cons self (slot-value 'instances-coco)))
  self
  )

(defmeth coco-proto :title (&optional (title nil set))
  (if set (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth coco-proto :current-coco ()
  (setf *current-coco* (slot-value 'identification)))

;; Function "make-coco" is updated in "cococg.lsp":

(defun make-coco
  (&key (n 65536) (p 65536) (q 1024) (title nil)
	(location (list 700 550)) (manager T) (silent nil))
  (if (boundp '*current-coco*)
      (if (= *current-coco* *fixed-coco*)
	  (format t "Only one CoCo-object with this object file ~%")
	(coco-init n p q :title title :location location :manager manager))
    (coco-init n p q :title title :location location :manager manager))
  (send coco-proto :new *current-coco* :title title)
  )

;; Update of function "make-coco" from "cocometh.lsp":

(defun make-coco
  (&key (n 65536) (p 65536) (q 1024) (r 65534) (s 65533) (ss 65532) (tt 65531) 
        (title nil) (location (list 700 550)) (manager T) (silent nil)
        (object *coco-object-name*))
  (if (boundp '*current-coco*)
      (if (= *current-coco* *fixed-coco*)
	  (format t "Only one CoCo-object with this object file ~%")
	(coco-init n p q r s ss tt :title title
                   :location location :manager manager :object object))
    (coco-init n p q r s ss tt :title title
               :location location :manager manager :object object))
  (send coco-proto :new *current-coco* :title title)
  )

(defun make-mips
  (&key (n 65536) (p 65536) (q 1024) (r 65534) (s 65533) (ss 65532) (tt 65531) 
        (title nil) (location (list 700 550)) (manager T) (silent nil)
        (object "smips.o"))
 (make-coco :n n :p p :q q :r r :s s :ss ss :tt tt :title title
            :location location :manager manager :object object)
 )

;;; Method :status is updated in "cococg:lsp"

(defmeth coco-proto :status (&optional (hit 'all))
 (coco-simple-command 13 (encode (list 'all 'formats 'tests 'exact 'fix 'ips
                                       'em 'old 'specification 'factors
                                       'observations 'data 'limits 'files
                                       'other 'search 'eh) hit
                          (list 1 2 3 4 5 6 7 18 8 8 9 9 9 10 11 12 12) hit)
  :coco-id (slot-value 'identification)))

;;; Update of :status from "cocometh.lsp":

(defmeth coco-proto :status (&optional (hit 'all) (dump nil))
 (coco-simple-command (if dump 12 13) ;; And CoCoCg
                      (encode (list 'all 'formats 'tests 'exact 'fix 'ips
                                       'em 'old 'specification 'factors
                                       'observations 'data 'limits 'files
                                       'other 'search 'eh) hit
                          (list 1 2 3 4 5 6 7 18 8 8 9 9 9 10 11 12 12) hit)
  :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-specification-file (&rest file-name)
  (coco-set-char-options 20 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-observations-file (&rest file-name)
  (coco-set-char-options 21 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-data-file (&rest file-name)
  (coco-set-char-options 22 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :coco-source (&rest file-name)
  (coco-set-char-options 23 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-diary-file (&rest file-name)
  (coco-set-char-options 25 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-output (&rest file-name)
  (coco-set-char-options 27 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-report-file (&rest file-name)
  (coco-set-char-options 28 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-log-file (&rest file-name)
  (coco-set-char-options 29 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-dump-file (&rest file-name)
  (coco-set-char-options 30 file-name nil
			 :coco-id (slot-value 'identification)))

(defun off-on (hit)
 (if (symbolp hit)
  (encode (list 'what 'off 'flop 'on) hit (list -1 1 2 3) (if hit 3 1))
  (if hit 3 1))
 )

(defmeth coco-proto :set-switch (number &optional (hit 'flop))
  (let ((result (call-coco 39 (off-on hit) :arg-long
                 (let ((x (list 'partitioning 'keyboard 'echo 'diary 'timer
                                'graph-mode 'decomposable-mode 'large
                                'short-test-output 'report 'reuse-tests
                                'adjusted-df 'trace 'exact-test
                                'exact-only-log-l 'fast 'exact-test-total
                                'exact-test-parts 'exact-test-unparted
                                'graphical-search 'note 'debug 'option 'log
                                'dump 'sorted 'keep-diary 'keep-report
                                'keep-log 'log-data 'keep-dump
                                'pausing-of-output 'huge 'ic 'bic 'em
                                'warnings))
                       (y (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                                16 17 18 19 20 21 22 23 24 25 26 27
                                28 29 30 31 32 33 34 35 36 37)))
                  (encode x number y (if (numberp number) number 99)))
                 :coco-id (slot-value 'identification))))
    (if (ok-coco result)
	(if (equalp (off-on hit) -1)
	    (equalp (car (nth 3 result)) 1)
	  T)))
  )

;;; ;;; (defmeth coco-proto :set-signall ()
;;; ;;;   (format t "Interface-procedure not to be implemented ~%"))
    
;;; ;;; (defmeth coco-proto :set-interrupt ()
;;; ;;;   (format t "Interface-procedure not to be implemented ~%"))

(defmeth coco-proto :set-print-formats (&rest args)
  (coco-set-long-options 45 args 2 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-table-formats (&rest args)
  (coco-set-long-options 46 args 4 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-test-formats (&rest args)
  (coco-set-long-options 47 args 4 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-page-formats (&rest args)
  (coco-set-long-options 48 args 2 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-paging-length (length)
  (coco-set-long-options 63 length 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-ips-stop-criterion (&optional (code 'what))
  (select (list 'cell 'sum )
	  (1- (coco-set-simple-option 53 (encode (list 'what 'cell 'sum) code 
                                          (list -1 1 2) 1)
				      :coco-id (slot-value 'identification)))))

;;; ;;; (defmeth coco-proto :set-ips-stop ()
;;; ;;;   (format t "Interface-procedure not to be implemented ~%"))

(defmeth coco-proto :set-ips-epsilon (&optional (epsilon 0.0000001))
  (coco-set-real-options 55 epsilon 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-ips-max-iterations (&optional (max 100))
  (coco-set-long-options 56 max 1 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-em-initial (&optional (code 'what))
  (select (list 'uniform 'first 'last 'mean 'random 'input)
	  (1- (car (coco-set-long-options 57
                                          (encode (list 'uniform 'first 'last
                                                        'mean 'random 'input)
                                                  code
                                                        (list 1 2 3 4 5 6) 1)
					  1 (if (eq code 'what) -1)
					  :coco-id (slot-value
						    'identification))))))

(defmeth coco-proto :set-em-epsilon (&optional (epsilon 0.001))
  (coco-set-real-options 58 epsilon 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-em-max-iterations (&optional (max 100))
  (coco-set-long-options 59 max 1 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-algorithm (&optional (sub-code 'what))
  (select (list 'a 'b 'c)
	  (1- (coco-set-simple-option 65 (encode (list 'what 'a 'b 'c)
                                          sub.code (list -1 1 2 3) 1)
				      :coco-id (slot-value 'identification)))))

(defmeth coco-proto :set-acceptance (&optional (alfa 0.05))
  (coco-set-real-options 69 alfa 1 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-rejection (&optional (alfa-rejected 0.025))
  (coco-set-real-options 87 alfa-rejected 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-test (&optional (arg 'what))
  (select (list 'lr 'pearson 'power)
	  (1- (coco-set-simple-option 70 (encode (list 'what 'lr 'deviance
                                                  'chisq 'pearson 'power) code 
                                          (list -1 1 1 2 2 3) 1)
				      :coco-id (slot-value 'identification)))))

(defmeth coco-proto :set-power-lambda (&optional (lambda 0.666667))
  (coco-set-real-options 71 (cond ((eq lambda 'null) 1.0)
				  ((numberp lambda) lambda)
				  (t lambda)) 1 nil
				  :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-ic (&optional (code 'aic) (kappa 2.0))
  (if (numberp code)
      (coco-set-real-options 72 code 1 nil
			     :coco-id (slot-value 'identification))
    (if (eq code 'kappa)
	(if (equalp 'what kappa)
	    (coco-set-real-options 72 kappa 1 nil
				   :coco-id (slot-value 'identification))
	  (coco-set-real-options 72 kappa 1 4
				 :coco-id (slot-value 'identification)))
     (coco-set-real-options 72 (list kappa) 1 (encode (list 'what 'aic 'bic
                                                            'off 'on) code 
                                               (list -1 1 2 3 5) 1)
			     :coco-id (slot-value 'identification)))))

(defmeth coco-proto :set-components (&optional (components-limit 0.01))
  (coco-set-real-options 73 components-limit 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-separators (&optional (separators-limit 0.001))
  (coco-set-real-options 74 separators-limit 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-exact-test (&optional (hit 'flop))
  (select (list 'off 'flop 'on 'all 'deviance)
	  ;;; Error !!!!!!
	  (1- (coco-set-simple-option 75 (encode (list 'what 'off 'flop 'on
                                                       'all 'deviance)
                                                 hit (list -1 1 2 3 4 5) 2)
				      :coco-id (slot-value 'identification)))))

(defmeth coco-proto :set-asymptotic (&optional (limit 0.25))
  (coco-set-real-options 76 limit 1 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-seed (&optional (seed 'random))
  (if (eq seed 'random)
      (coco-set-long-options 78 0 1 2
			     :coco-id (slot-value 'identification))
    (coco-set-long-options 78 seed 1 nil
			   :coco-id (slot-value 'identification))))

(defmeth coco-proto :set-number-of-tables (&optional (number 1000))
  (if (eq number 'variating)
      (coco-set-long-options 80 0 1 1 :coco-id (slot-value 'identification)))
  (coco-set-long-options 80 number 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-list-of-number-of-tables (&optional (list-of-numbers
					'what))
  (let ((result
	 (coco-set-long-options 85 list-of-numbers 25 nil
				:coco-id (slot-value 'identification))))
;;;    (select (nth 6 result) (iseq (nth 1 (nth 4 result))))
    result
    ))

(defmeth coco-proto :set-exact-epsilon (&optional (epsilon 0.0000001))
  (coco-set-real-options 84 epsilon 1 nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-data ()
  (coco-simple-command 88 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-specification ()
  (coco-simple-command 89 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-factors ()
  (coco-simple-command 90 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-names ()
  (coco-simple-command 91 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-read (&optional (hit 'all) (set ";"))
  (if (eq hit 'what)
      (coco-set-char-options 92 'what nil
			     :coco-id (slot-value 'identification))
    (if (eq hit 'all)
	(coco-simple-command 92 1 :coco-id (slot-value 'identification))
      (coco-set-char-options 92 set nil
			     :coco-id (slot-value 'identification))))
  )

(defmeth coco-proto :set-datastructure (&optional (code 'all))
  (select (list 'all 'necessary 'file 'large)
	  (1- (coco-set-simple-option 93 (encode (list 'what  'all
                                                       'necessary 'file 'large)
                                                 code (list -1 1 2 3 4) 1)
				      :coco-id (slot-value 'identification)))))

;;; Method :enter-names is updated in "cococg.lsp":

(defmeth coco-proto :enter-names (names levels
                                        &optional (missing-levels nil))
  (let ((names (split-name-string names)))
    (cond ((not (= (length names) (length levels)))
	   (format t "Wrong number of number of levels ~%"))
	  ((and missing-levels (not (= (length names)
				       (length missing-levels))))
	   (format t "Wrong number of factors to be marked ~%"))
	  (t (ok-coco
	      (call-coco 91 1
			 :arg-char (to-string names "")
			 :arg-long (concatenate 'list
						(list (length
						       (to-string names ""))
						      (length levels)
						      (length missing-levels))
						levels missing-levels)
			 :coco-id (slot-value 'identification))))))
  )

;;; Update of :enter-names form "cocometh.lsp":

(defmeth coco-proto :enter-names (names levels
                                        &optional (missing-levels nil))
  (let ((names (split-name-string names)))
    (cond ((not (= (length names) (length levels)))
	   (format t "Wrong number of number of levels ~%"))
	  ((and missing-levels (not (= (length names)
				       (length missing-levels))))
	   (format t "Wrong number of factors to be marked ~%"))
	  (t (ok-coco
	      (call-coco 91 1
			 :arg-char (to-string names "")
			 :arg-long (concatenate 'list
						(list (length
						       (to-string names ""))
						      (length levels)
						      (length missing-levels))
                                    (mapcar #'(lambda (i)
                                                (if (eq 'continuous i) 0 i))
                                     levels)
                                    missing-levels)
			 :coco-id (slot-value 'identification))))))
  )




(defmeth coco-proto :dimension (&key (full nil))
  (let ((result (coco-set-long-options 146 '(0) 1 (if full 2 1)
				       :coco-id (slot-value 'identification))))
    (if result (car result))))

(defmeth coco-proto :return-marginal-dimension (set &key (full nil)
						    (total nil))
  (let ((result
	 (call-coco 142 (+ (if full 2 1) (if total 2 0))
		    :arg-char set
		    :arg-long '(0)
		    :coco-id (slot-value 'identification))))
    (if (ok-coco result) (car (nth 6 result)))))

(defmeth coco-proto :return-factor-type-list (&key (full nil))
  (let ((dimension (send self :dimension :full full)))
    (coco-set-long-options 145 (repeat 0 dimension) dimension (if full 3 6)
			   :coco-id (slot-value 'identification))))

(defmeth coco-proto :return-level-list (&key (full nil))
  (let ((dimension (send self :dimension :full full)))
    (coco-set-long-options 145 (repeat 0 dimension) dimension (if full 1 4)
			   :coco-id (slot-value 'identification))))

(defmeth coco-proto :return-missing-list (&key (full nil))
  (let ((dimension (send self :dimension :full full)))
    (coco-set-long-options 145 (repeat 0 dimension) dimension (if full 2 5)
			   :coco-id (slot-value 'identification))))

(defmeth coco-proto :return-name-list-string (&key (full nil))
  (let ((result (coco-enter-string 144 'what (if full 2 1)
				   :coco-id (slot-value 'identification))))
    result))


;;; Method :return-name-list is updaated in "cococg.lsp":

(defmeth coco-proto :return-name-list-1 (&key (full nil))
  (let ((names (split-string
		(send self :return-name-list-string :full full)
		:cococg (eq *coco-type* "cococg"))))
    (mapcar #'(lambda (i) (if (listp i)
			      (concatenate 'string ":" i)
			    (concatenate 'string (list i))))
	    (if (= 3 (length names))
		(car (cdr names))
	      (reverse (cdr (reverse (cdr (cdr names)))))))
    )
  )

;;; Update of :return-name-list from "cocometh.lsp":

(defmeth coco-proto :return-name-list-2 (&key (full nil))
  (let ((names (split-string
		(cadr (split-string-chr
		       (send self :return-name-list-string :full full)
		       #\[ #\] nil)) :cococg (equal *coco-type* "cococg"))))
    (mapcar #'(lambda (i) (if (listp i)
			      (concatenate 'string ":" i)
			    (if (stringp i) i
			      (concatenate 'string (list i)))))
	    (car names))
;;    (car names)
    )
  )

(defmeth coco-proto :return-name-list (&key (full nil))
  (if  (equal *coco-type* "cococg")
      (send self  :return-name-list-2 :full full)
    (send self  :return-name-list-1 :full full))
  )

(defmeth coco-proto :return-names (&key (full nil))
  (mapcar #'(lambda (i j)
	      (list
	       ;; Name:
	       i
	       ;; Variable-label:
	       nil
	       ;; (concatenate 'string "Variable " (list (int-char i)))
	       ;; Variable-type (discrete, continuous, ordinal, etc.):
	       j
	       ;; Stratum:
	       0))
	  (send self :return-name-list :full full)
	  (send self :return-factor-type-list :full full))
  )

(defmeth coco-proto :set-ordinal (set)
  (coco-set-char-options 176 set nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :select-cases (name cell &key (silent nil))
  (if (and (stringp name) (listp cell) (numberp (car cell)))
      (call-coco-message 96 nil
			 :arg-char name
			 :arg-long cell
			 :coco-id (slot-value 'identification))))

(defmeth coco-proto :or-select-cases (name cell)
  (if (and (stringp name) (listp cell) (numberp (car cell)))
      (call-coco-message 97 nil
			 :arg-char name
			 :arg-long cell
			 :coco-id (slot-value 'identification))))

(defmeth coco-proto :reject-cases (name cell)
  (if (and (stringp name) (listp cell) (numberp (car cell)))
      (call-coco-message 98 nil
			 :arg-char
			 name :arg-long cell
			 :coco-id (slot-value 'identification))))

(defmeth coco-proto :or-reject-cases (name cell)
  (if (and (stringp name) (listp cell) (numberp (car cell)))
      (call-coco-message 99 nil
			 :arg-char name
			 :arg-long cell
			 :coco-id (slot-value 'identification))))

(defmeth coco-proto :redefine-factor (name levels &optional (missing-levels 0))
  (if (and (stringp name) (numberp levels) (numberp missing-levels))
      (call-coco-message 100 nil
			 :arg-char name
			 :arg-long (list levels missing-levels)
			 :coco-id (slot-value 'identification))))

(defmeth coco-proto :cutpoints (name cutpoints)
  (if (and (stringp name) (listp cutpoints) (numberp (car cutpoints)))
      (call-coco-message 101 nil
			 :arg-char name
			 :arg-double cutpoints
			 :coco-id (slot-value 'identification))
    (if (and (stringp name) (eq 'what cutpoints))
	(call-coco 101 -1
		   :arg-char name
		   :arg-double (repeat 0 32) ;;; Number of levels -1 !!!
		   :coco-id (slot-value 'identification)))))

(defmeth coco-proto :skip-missing ()
  (coco-simple-command 102 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-observations ()
  (coco-simple-command 103 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-table (&key (silent nil))
  (coco-simple-command 104 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-list (&key (silent nil))
  (coco-simple-command 105 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :enter-table (counts &key (silent nil))
  (cond ((not (= (length counts)
		 (send self :return-marginal-dimension "*" :full T ; :total T
		       )))
	 (format t "Wrong number of cells ~%"))
	((< (min counts ) -1)
	 (format t "Invalid cell count ~%"))
	((> (max counts )  2147483640)
	 (format t "Invalid cell count ~%"))
	((> (sum (select counts (which (and (< 0 counts)
					    (< counts 2147483641)))))
	    2147483647)
	 (format t "Too many observations ~%"))
	(t (coco-set-long-options 104
				  (if-else (= counts -1) 2147483644 counts)
				  (length counts) (if silent 2 1)
				  :coco-id (slot-value 'identification))))
  )

(defun table-simple-list (x ncol number-of-levels)
  (if (= 0 (mod (length x) ncol))
      (if (= (length number-of-levels) ncol)
	  (let* ((result-table (repeat 0 (prod number-of-levels)))
		 (prodi (select (accumulate #'* (cons 1 number-of-levels))
				(iseq (length number-of-levels)))))
	    (dolist (case (split-list x ncol) result-table)
		    (if (which (>= case number-of-levels))
			(format t "Invalid case: ~a ~%" case)
		      (let ((pos (sum (* case prodi))))
			(setf (nth pos result-table)
			      (1+ (nth pos result-table)))))))
	(progn (format T "Number of levels wrong~%") nil))
    (progn (format T "Number of values not multi-plum of number of columns~%")
	   nil))
  )

(defun list-select-cases (x ncol select-case-fun)
  (if (= 0 (mod (length x) ncol))
      (let ((result-list nil))
	(dolist (case (split-list x ncol) result-list)
		(if (funcall select-case-fun case)
		    (setf result-list (concatenate 'list case result-list)))))
    (progn (format T "Number of values not multi-plum of number of columns~%")
	   nil))
  )

(defun select-columns (x ncol columns)
  (if (= 0 (mod (length x) ncol))
      (if (<= (length columns) ncol)
	  (let ((nrow (/ (length x) ncol)))
	    (select x (+ (repeat columns nrow)
			 (* ncol (repeat (iseq nrow)
					 (repeat (length columns) nrow)))))
	    )
	(progn (format T "To many columns selected~%") nil))
    (progn (format T "Number of values not multi-plum of number of columns~%")
	   nil))
  )

(defun case-list-to-table (x names levels &optional
			     &key (missing nil)
			     (select-case-fun nil) (columns nil))
  (let ((names (split-name-string names)))
    (if (= 0 (mod (length x) (length levels)))
	(if (= (length names) (length levels))
	    (if (or (not missing) (= (length missing) (length levels)))
		(if (<= (length columns) (length levels))
		    (let* ((a (if select-case-fun
				  (list-select-cases x (length levels)
						     select-case-fun) x))
			   (b (if columns
				  (select-columns a (length levels) columns)
				a))
			   (n (if columns
				  (select names columns) names))
			   (l (if columns
				  (select levels columns) levels))
			   (m (if missing
				  (if columns (select missing columns) missing)
				nil))
			   (tt (table-simple-list b (length l) l)))
		      (list tt n l m))
		  (progn (format T "To many columns selected~%") nil))
	      (progn (format T "Length(Missing) <> Length(Levels)~%") nil))
	  (progn (format T "Length(Names) <> Length(Levels)~%") nil))
      (progn (format T "Number of values not multi-plum of number of names~%")
	     nil)))
  )

(defun case-list-to-list (x names levels &optional
			     &key (missing nil) (accumulated nil)
			     (select-case-fun nil) (columns nil))
  (let ((names (split-name-string names)))
    (if (= 0 (mod (length x) (length names)))
	(if (= (length names) (length levels))
	    (if (or (not missing) (= (length missing) (length levels)))
		(if (<= (length columns) (length levels))
		    (let* ((a (if select-case-fun
				  (list-select-cases x (length levels)
						     select-case-fun) x))
			   (b (if columns
				  (select-columns a (length levels) columns)
				a))
			   (columns
			    (if accumulated
				(cdr (if columns columns (iseq (length levels))))
			      columns))
			   (n (if columns
				  (select names columns) names))
			   (l (if columns
				  (select levels columns) levels))
			   (m (if missing
				  (if columns (select missing columns) missing)
				nil)))
		      (list b n l m))
		  (progn (format T "To many columns selected~%") nil))
	      (progn (format T "Length(Missing) <> Length(Levels)~%") nil))
	  (progn (format T "Length(Names) <> Length(Levels)~%") nil))
      (progn (format T "Number of values not multi-plum of number of names~%")
	     nil)))
  )

(defmeth coco-proto :enter-names-and-list
  (simple-case-list names levels &optional &key
		    (missing-levels nil) (accumulated nil)
		    (select-case-fun nil set) (columns nil subset))
  (let* ((result (case-list-to-list simple-case-list
				    names levels
				    :missing missing-levels
				    :accumulated accumulated
				    :select-case-fun select-case-fun
				    :columns columns))
	 (b (car result))
	 (n (cadr result))
	 (l (caddr result))
	 (m (cadddr result)))
    (if m
	(send self :enter-names n l m)
      (send self :enter-names n l))
    (send self :enter-list b :accumulated accumulated)
    )
  )

(defmeth coco-proto :enter-names-and-list-as-table
  (simple-case-list names levels &optional &key
		    (missing-levels nil) (accumulated nil)
		    (select-case-fun nil set) (columns nil subset))
  (let* (;;; (names (split-name-string names))
	 (result (case-list-to-table simple-case-list
				     names levels
				     :missing missing-levels
				     :accumulated accumulated
				     :select-case-fun select-case-fun
				     :columns columns))
	 (tt (car result))
	 (n (cadr result))
	 (l (caddr result))
	 (m (cadddr result)))
    (if m
	(send self :enter-names n l m)
      (send self :enter-names n l))
    (send self :enter-table tt)
    )
  )

(defun print-names-and-list-as-table
  (x names levels &optional &key (missing nil)
     (select-case-fun nil) (columns nil))
  (let* ((names (split-name-string names))
	 (result (case-list-to-table x names levels
				     :missing missing
				     :select-case-fun select-case-fun
				     :columns columns))
	 (tt (car result))
	 (n (cadr result))
	 (l (caddr result))
	 (m (cadddr result)))
    result)
  )

(defmeth coco-proto :enter-list-as-table
  (simple-case-list &optional &key (select-case-fun nil set))
  (let* ((result (case-list-to-table simple-case-list
				     (send self :return-name-list)
				     (send self :return-level-list)
				     :select-case-fun select-case-fun))
	 (tt (car result)))
    (send self :enter-table tt)
    )
  )

(defmeth coco-proto :enter-list
  (list &key (accumulated nil) (ncol nil) (select-case-fun nil set)
	(columns nil subset) (silent nil))
  ;; (format t "Interface-procedure not fully tested ~%")
  (if (and (= 0 (mod (length list)
		     (if columns ncol
		       (+ (length (send self :return-name-list :full T))
			  (if accumulated 1 0)))))
	   (if columns
	       (= (length columns)
		  (+ (length (send self :return-name-list :full T))
		     (if accumulated 1 0))) T))
      (let* ((a (if select-case-fun
		    (list-select-cases
		     list
		     (if ncol ncol
		       (+ (length (send self :return-name-list :full T))
			  (if accumulated 1 0)))
		     select-case-fun) list))
	     (b (if columns (select-columns a ncol columns) a)))
	(coco-set-long-options 105 b (length b) (if accumulated 2 1)
			       :coco-id (slot-value 'identification)))
    (format T "List of wrong length"))
  )

(defmeth coco-proto :read-q-table (set)
  (coco-enter-string 106 set nil :coco-id (slot-value 'identification))
  )

(defmeth coco-proto :read-q-list (set)
  (coco-enter-string 107 set nil :coco-id (slot-value 'identification))
  )

(defmeth coco-proto :enter-q-table (set table)
  ;; (format t "Interface-procedure not fully tested ~%")
  (ok-coco (call-coco 106 1
		      :arg-char set
		      :arg-long table
		      :coco-id (slot-value 'identification)))
  )

(defmeth coco-proto :enter-q-list (set list)
  ;; (format t "Interface-procedure not fully tested ~%")
  (ok-coco (call-coco 107 1
		      :arg-char set
		      :arg-long list
		      :coco-id (slot-value 'identification)))
  )

(defmeth coco-proto :clean-data ()
  (coco-simple-command 108 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :substitute (&optional &key (coco-id *current-coco*))
  (coco-simple-command 109 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :exclude-missing (&optional (hit 'flop) (set ";"))
  (if (eq hit 'what)
      (coco-set-char-options 110 'what nil
			     :coco-id (slot-value 'identification))
    (if (eq hit 'in)
	(coco-set-char-options 110 set 4
			       :coco-id (slot-value 'identification))
      (coco-simple-command 110 (encode (list 'off 'flop 'on) hit (list 1 2 3)
                                       1)
			   :coco-id (slot-value 'identification))))
  )

(defmeth coco-proto :em-on (&optional (hit nil))
  (if (eq hit 'what)
      (send self :set-switch 'em hit)
    (coco-simple-command 112 nil :coco-id (slot-value 'identification))))

(defmeth coco-proto :set-current-model (&optional (model 'current)
                                        (old-current nil) (old-base nil))

 (send self :make-current
  (cond
   ((eq model 'base)
    (if old-base old-base
      (send self :return-model-number 'base)))
   ((eq model 'current)
    (if old-current old-current
      (send self :return-model-number 'current)))
   ((eq model 'last)
    (send self :return-model-number 'last))
   ((eq model 'previous)
    'previous)
   ((eq model 'next)
    'next)
   ((numberp model)
    model)
   ((stringp model)
    (progn
     (send self :read-model model)
     (send self :return-model-number 'last)))
   ((if (objectp model) (send model :has-slot 'model-number) nil)
    (send model :slot-value 'model-number))
   ((and (send self :has-slot 'model-number)
          ;; Model has to be 'nil ?!?!?
         (not model) ;; (not (eq model 'current))
         )
    (slot-value 'model-number))
   ((not model)
    (progn
     (format t
      "Argument 'nil assumed to be 'current in :set-current-model. ~%")
     model))
   (t (progn
       (format t "Unexpected argument in :set-current-model !!! ~%")
       model))))
  )

(defmeth coco-proto :before-set-current (&optional (model 'current)
                                                   (modelset nil)
                                                   (number nil))
  (if (and number (not (equalp model 'number)))
   (format t "Invalid model argument (should be 'number) !!! ~%"))
  (cond
   ((and (objectp model) (send model :has-slot 'model-number))
    (let ((old-current (send self :return-model-number 'current)))
     (send self :make-current (send model :slot-value 'model-number))
     old-current))
   ((and (send self :has-slot 'model-number)
          ;; If model is set different from 'nil
          ;; then :set-current-model is used:
         (or (not modelset) (not model)))
    (let ((old-current (send self :return-model-number 'current)))
     (if (not model)
      (format t
       "Argument 'nil given in :before-set-current: Model of object. ~%"))
     (send self :make-current (slot-value 'model-number))
     old-current))
   ((and (not (equalp model 'current)) model)
    (let ((old-current (send self :return-model-number 'current)))
     (send self :set-current-model
      (if (equalp model 'number) number model) old-current nil)
     old-current))
   (t nil)))

(defmeth coco-proto :before-set-both (&optional (model-1 'current)
                                                (model-2 'base)
                                                (model1set nil)
                                                (model2set nil))
 (let ((old-current (send self :return-model-number 'current))
       (old-base (send self :return-model-number 'base)))
  (send self :set-current-model (if model-2 model-2 'base)
        old-current old-base)
  (send self :base)
  (if old-current (send self :make-current old-current))
  (send self :set-current-model model-1 old-current old-base)
  (list old-current old-base)))

(defmeth coco-proto :after-set-current (old-current result
                                                    &optional
                                                    (type 'unconditioned)
                                                    (model nil))
 (if (stringp model)
  (send self :dispose-on-model 'current))
 (if old-current
     (send self :make-current old-current))
 (cond ((eq type 'unconditioned)
        result)
       ((eq type 'ok)
        (ok-coco result))
       ((eq type 'long-true)
        (if (ok-coco result) (equalp (car (nth 6 result)) 1)))
       ((eq type 'double)
        (if (ok-coco result) (replace-my-not-a-number (nth 7 result))))
       ((eq type 'long-and-double)
        (if (ok-coco result) (list (nth 6 result)
                              (replace-my-not-a-number (nth 7 result))))))
 )

(defmeth coco-proto :print-table (type &optional (set "*")
				       &key (model 'current modelset)
				       (random nil) (log-transformed nil)
				       (complete nil) (permuted T))
  (let ((old-current (send self :before-set-current model modelset))
	(result
         (call-coco (if permuted 113 121) -1
          :arg-char set
          :arg-long
          (encode-type-and-options type random log-transformed complete
                                   permuted
           :uniform nil :rankit nil :probit nil)
          :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'ok nil)))

(defmeth coco-proto :describe-table (type &optional (set "*")
					  &key (model 'current modelset)
					  (probit nil) (rankit nil)
					  (uniform nil)
					  (random nil) (log-transformed nil)
					  (complete nil))
  (let ((old-current (send self :before-set-current model modelset)))
    (let ((result
	   (call-coco 115 -1
		      :arg-char set
		      :arg-long
                      (encode-type-and-options type random log-transformed
                                               complete nil
                                               :uniform uniform :rankit rankit
                                               :probit probit) 
		      :coco-id (slot-value 'identification))))
      (send self :after-set-current old-current result 'ok nil))))

(defmeth coco-proto :return-vector (type &optional (set "*")
					 &key (model 'current modelset)
					 (random nil) (log-transformed nil)
					 (complete nil) (permuted T)
					 (dump nil))
  (let ((old-current (send self :before-set-current model modelset))
	(result
	   (call-coco (if permuted 119 122) (if dump 0 -1)
		      :arg-char set
		      :arg-long
                      (encode-type-and-options type random log-transformed
                                               complete permuted
                                               :uniform nil :rankit nil
                                               :probit nil)
		      :arg-double (iseq 1 (send self :return-marginal-dimension
						set))
		      :coco-id (slot-value 'identification))))
    (send self :after-set-current old-current result 'double nil)))

(defmeth coco-proto :return-matrix (type-list &optional (set "*")
					      &key (model-list nil)
					      (random-list nil)
					      (log-transformed-list nil)
					      (complete-list nil)
					      (permuted T))
  (let ((old-seed (send self :set-seed 'what))
	(old-current (send self :return-model-number 'current))
	(old-base (send self :return-model-number 'base))
	(model-list (if (send self :has-slot 'model-number)
			(mapcar #'(lambda (model)
				    (if (not model)
					(slot-value 'model-number)
				      model))
				model-list)
		      model-list))
	(result NIL))
    (mapcar #'(lambda (type m r c l)
		(send self :set-seed old-seed)
		(setf result
		      (concatenate
		       'list result
		       (list (send self :return-vector
			      type set :permuted permuted
			      :model (cond ((eq m nil) old-current)
					   ((eq m 'current) old-current)
					   ((eq m 'base) old-base)
					   (t m))
			      :random r :complete c :log-transformed l
			      :permuted permuted)))))
	    type-list
	    (if model-list model-list (repeat (list nil) (length type-list)))
	    (if random-list random-list (repeat (list nil) (length type-list)))
	    (if complete-list complete-list
	      (repeat (list nil) (length type-list)))
	    (if log-transformed-list log-transformed-list 
	      (repeat (list nil) (length type-list))))
    (if old-current (send self :make-current old-current))
    (if old-base (send self :make-base old-base))
    result)
  )

(defmeth coco-proto :print-sparse-table (&optional (set "*"))
  (coco-set-char-options 114 set nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :plot
  (x y &optional (set "*") &key
     (X-model 'current X-modelset) (X-random nil) (X-log-transformed nil)
     (Y-model 'current Y-modelset) (Y-random nil) (Y-log-transformed nil)
     (complete nil) )
 (let ((old (send self :before-set-both X-model Y-model
                  X-modelset Y-modelset)))
  (let ((result (call-coco 116 1
		       :arg-char set
		       :arg-long (list (+ (table-value x)
					  (if complete 16 0)
					  (if X-random 32 0))
				       -2
				       (if X-random 1 0)
				       (if X-log-transformed 1 0)
				       (+ (table-value y)
					  (if complete 16 0)
					  (if Y-random 32 0))
				       -1
				       (if Y-random 1 0)
				       (if Y-log-transformed 1 0)
				       (if complete 1 0))
		       :coco-id (slot-value 'identification))))
   (if (stringp Y-model) (send self :dispose-of-model 'base))
   (if (and (cadr old) Y-model) (send self :make-base (cadr old)))
   (send self :after-set-current (car old) result 'long-and-double X-model)))
 )

(defmeth coco-proto :list-values (&optional (set "*"))
  (coco-enter-string 117 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :case-list (&optional (set "*"))
  (coco-enter-string 118 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-model (model)
  (coco-enter-string 127 model nil :coco-id (slot-value 'identification)))

;; Method :return-model is updated in "cococg.lsp":

(defmeth coco-proto :return-model-1
  (&optional (model 'current modelset) number
	     &key (type 'gc))
 (let ((old-current (send self :before-set-current model modelset number)))
  (let ((result
         (coco-enter-string 127 'what -2
          :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'unconditioned nil)))
 )

;; Update of method :return-model from "cocometh.lsp":

(defmeth coco-proto :return-model-2
  (&optional (model 'current modelset) (number nil)
	     &key (type 'gc))
  (let ((old-current (send self :before-set-current model modelset number))
	(tp (encode ;; (list 'gc 'cs 'both 'discrete 'linear 'hierarchical)
		    (list 'gc 'cs 'both 'discrete 'linear 'quadratic)
		    type (1+ (iseq 6)) 3))
	(m (encode (list 'base 'current 'last) model (1+ (iseq 3)) 2)))
    (let ((result
	   (coco-enter-string 127 'what (- (+ (* 10 tp) m))
			      :coco-id (slot-value 'identification))))
      (send self :after-set-current old-current result 'unconditioned nil)))
  )

(defmeth coco-proto :return-model
  (&optional (model 'current modelset) (number nil)
	     &key (type 'gc))
  (if  (equal *coco-type* "cococg")
      (send self  :return-model-2 model number :type type)
    (send self  :return-model-1 model number :type type))
  )

(defmeth coco-proto :return-model-splitted
  (&optional (model 'current modelset) (number nil)
	     &key (type 'gc) (noted nil))
  (let* ((x (split-string-chr
	     (send self :return-model model number :type type) #\[ #\] nil))
	 (y (if (= 1 (car (which
			   (/= 0 (mapcar #'(lambda (i) (length i)) (cdr x))))))
		(cddr x) (cdddr x))))
    (if noted
	(mapcar #'(lambda (i) (list i type)) y)
      y))
  )

(defmeth coco-proto :return-generators
  (model &key (full nil) (simple nil) (noted nil))
  (if (if simple T (not (send self :is-mixed)))
      (send self :return-model-splitted model nil :type 'gc :noted noted)
    (concatenate
     'list
     (send self :return-model-splitted
	   model nil :type 'discrete :noted noted)
     (send self :return-model-splitted
	   model nil :type 'linear :noted noted)
     (send self :return-model-splitted
	   model nil :type 'quadratic :noted noted)))
  )

(defmeth coco-proto :return-model-set (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result
         (coco-set-long-options 140 'what 
          (send self :dimension :full nil) -2
          :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'unconditioned nil))))

(defmeth coco-proto :return-model-set-string (&optional (model 'current
                                                               modelset))
 (format t "This method returns a string, not a list of integers: ~%")
 (format t "(0) 1 for variables (not) in the model. ~%")
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result
         (coco-enter-string 141 'what -2
          :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'unconditioned nil))))

(defmeth coco-proto :read-n-interactions (order &optional (set "*"))
  (if (and (stringp set) (numberp order))
      (call-coco-message 128 nil :arg-char set :arg-long order
			 :coco-id (slot-value 'identification))))

(defun make-model (model coco-object &optional &key (title nil))
  (let ((x (send coco-model-proto :new model coco-object :title title)))
    x)
  )

(defmeth coco-proto :make-model (&optional (model 'current) &key (title nil))
  (make-model model self :title title)
  )

(defmeth coco-proto :just ()
  (coco-simple-command 199 1 :coco-id (slot-value 'identification)))

(defmeth coco-proto :collaps-model (set &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 129 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :normal-to-dual (&optional (just nil))
  (if just (send self :just))
  (coco-simple-command 125 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :dual-to-normal (&optional (just nil))
  (if just (send self :just))
  (coco-simple-command 126 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :base ()
  (coco-simple-command 130 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :current ()
  (coco-simple-command 131 nil :coco-id (slot-value 'identification)))

(defun encode-model-current (&optional (number nil) (current-default T))
 (encode (list 'base 'current 'last 'previous 'next) number
         (list -1 -2 -3 -4 -5)
  (if current-default -2 (if (not number) -2 number)))
 )

(defun encode-model (&optional (number nil) (current-default nil))
 (encode (list 'base 'current 'last 'previous 'next) number
         (list -1 -2 -3 -4 -5)
  (if current-default -2 (if (not number) -2 number)))
 )

(defmeth coco-proto :encode-model (&optional number)
 (encode (list 'base 'current 'last 'previous 'next) number
         (list -1 -2 -3 -4 -5)
  (send self :return-model-number number)))

(defmeth coco-proto :make-base (&optional number)
  (coco-set-long-options 132 (send self :encode-model number)
			 1 0 :coco-id (slot-value 'identification)))

(defmeth coco-proto :make-current (&optional number)
  (coco-set-long-options 133 (send self :encode-model number)
			 1 0 :coco-id (slot-value 'identification)))

(defmeth coco-proto :return-model-number (&optional (model 'current modelset))
 (if (if (numberp model) (> model 0) nil)
  model
  (if (and (if modelset (not model))
       (send self :has-slot 'model-number))
   (slot-value 'model-number)
   (let ((result
          (coco-set-long-options 133 0 1 (encode-model model)
           :coco-id (slot-value 'identification))))
    (if result (car result))))))

(defmeth coco-proto :return-model-number-with-enter (model)
  (if model
      (if (stringp model)
	  (progn (send self :read-model model)
		 (send self :return-model-number 'last))
	(if (numberp model) model
	  (send self :return-model-number model)))
    (send self :return-model-number 'current)))

(defmeth coco-proto :return-edge-list-list (&optional (model 'current modelset)
						      &key (edges nil)
						      (fix nil))
  (let ((old-current (send self :before-set-current model modelset)))
    (let* ((sub-code (encode-model-current model))
	   (edges-fix (list (encode (list 'in-model 'all
                                          'all-edges 'not-in-model) edges
                             (list 1 0 0 -1) 1)
			    (cond ((eq fix 'fix-edges) 1)
				  ((eq fix 'all) 0)
				  ((eq edges 'all-edges) 0)
				  ((eq fix 'non-fix-edges) -1)
				  (t 0))))
	   (result
	    (let ((tmp (call-coco 143 sub-code
				  :arg-long (concatenate 'list edges-fix
							 (repeat -1 4))
				  :coco-id (slot-value 'identification))))
	      (if (eq 70 (car (car tmp)))
		  (call-coco 143 sub-code
			     :arg-long (concatenate
					'list edges-fix
					(repeat -1 (- (nth 1 (nth 4 tmp)) 2)))
			     :coco-id (slot-value 'identification))
		tmp))))
      (if old-current (send self :make-current old-current))
      (if (ok-coco result)
	  (select (nth 6 result) (which (/= -1 (nth 6 result))))))))

(defmeth coco-proto :return-edge-list
  (&optional (model 'current modelset) &key (edges nil) (fix nil))
  (split-list
   (send self :return-edge-list-list model :edges edges :fix fix)
   2))

(defmeth coco-proto :return-fix (code)
  (cond ((eq 'edges code)
	 (send self :fix-edges 'what))
	((eq 'in code)
	 (send self :fix-in 'what))
	((eq 'out code)
	 (send self :fix-out 'what))))

(defmeth coco-proto :print-formula ()
  (coco-simple-command 134 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :return-expression (&optional (model 'current modelset)
                                                  number)
 (let ((result
        (let ((old-current (send self :before-set-current
                                 model modelset number)))
         (let ((result
                (coco-enter-all 134 'what nil nil -2
                 :coco-id (slot-value 'identification))))
          (send self :after-set-current old-current result 'unconditioned nil)
          ))))
  (let ((sub-lists (split-string-chr (car result) #\/))
        (factors (car (cdr result))))
   (list (mapcar #'(lambda (set factor)
                    (if (equal set "[]") (list 'total set factor)
                     (if (> factor 0) (list 'clique set factor)
                      (list 'seperator set factor))))
          (cdr (split-string-chr (car sub-lists) #\[)) factors)
    (mapcar #'(lambda (i)
               (let ((j (split-string-chr i #\,)))
                (list 'item (car (cdr (split-string-chr (car j) #\[)))
                 (cdr (split-string-chr (cadr j) #\[)))))
     (cdr (split-string-chr (cadr sub-lists) #\{ #\})))
    (list 'constant (car (car (cddr result)))))))
 )

(defmeth coco-proto :return-components (&optional (model 'current modelset)
                                                  number)
 (let ((result (if modelset (send self :return-expression model number)
                (send self :return-expression))))
   (let ((a (concatenate 'list
             (mapcar #'(lambda (i) (if (equal 'clique (car i)) (cadr i)))
                     (car result))
             (mapcar #'(lambda (i) (cadr i)) (cadr result)))))
    (if a (select a (which a)))))
 )

(defmeth coco-proto :print-vertex-order ()
  (coco-simple-command 135 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :dispose-of-formula ()
  (coco-simple-command 136 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :print-interval (a b)
  (let ((old-current (send self :before-set-current b T)))
    (do ((i b (send self :return-model-number 'current)))
	((or (not i) (< i a)) T)
	(coco-simple-command 137 2 :coco-id (slot-value 'identification))
	(send self :make-current 'previous))
    (if old-current (send self :make-current old-current)))
  )

(defmeth coco-proto :print-models (a &optional)
  (let ((old-current (if T		; (not (eq a 'current))
			 (send self :return-model-number 'current) nil))
	(result
	 (dolist
	  (i (if (atom a) (list a) a) nil)
	  (if (send self :make-current i)
	      (coco-simple-command 137 2
				   :coco-id (slot-value 'identification))))))
    (if old-current (send self :make-current old-current))
    result)
  )

(defmeth coco-proto :print-model (&optional (model 'current modelset) a b)
  (cond ((and (if modelset (not model)) (send self :has-slot 'model-number))
	 (send self :print-models (slot-value 'model-number)))
	((or (eq model 'number) (eq model 'list))
	 (send self :print-models a))
	((numberp model)
	 (send self :print-models model))
	((eq model 'interval)
	 (send self :print-interval a b))
	((listp model)
	 (send self :print-models model))
	(t (coco-simple-command 137 (encode (list 'base 'current 'last 'all)
                                            model
                                            (list 1 2 3 4) 2)
				:coco-id (slot-value 'identification))))
  )

(defmeth coco-proto :describe-interval (a b)
  (let ((old-current (send self :before-set-current b T)))
    (do ((i b (send self :return-model-number 'current)))
	((or (not i) (< i a)) T)
	(coco-simple-command 138 2 :coco-id (slot-value 'identification))
	(send self :make-current 'previous))
    (if old-current (send self :make-current old-current)))
  )

(defmeth coco-proto :describe-models (a &optional)
  (let ((old-current (if T		; (not (eq a 'current))
			 (send self :return-model-number 'current) nil))
	(result
	 (dolist
	  (i (if (atom a) (list a) a) nil)
	  (if (send self :make-current i)
	      (coco-simple-command 138 2
				   :coco-id (slot-value 'identification))))))
    (if old-current (send self :make-current old-current))
    result)
  )

(defmeth coco-proto :describe-model (&optional (model 'current modelset) a b)
  (cond ((and (if modelset (not model)) (send self :has-slot 'model-number))
	 (send self :describe-models (slot-value 'model-number)))
	((or (eq model 'number) (eq model 'list))
	 (send self :describe-models a))
	((numberp model)
	 (send self :describe-models model))
	((eq model 'interval)
	 (send self :describe-interval a b))
	((listp model)
	 (send self :describe-models model))
	(t (coco-simple-command 138 (encode (list 'base 'current 'last 'all)
                                            model
                                            (list 1 2 3 4) 2)
				:coco-id (slot-value 'identification))))
  )

(defmeth coco-proto :dispose-of-interval (a b)
  (let ((old-current (send self :before-set-current b T)))
    (do ((i b (send self :return-model-number 'current)))
	((or (not i) (< i a)) T)
	(coco-simple-command 139 2 :coco-id (slot-value 'identification))
	(send self :make-current 'previous))
    (if old-current (send self :make-current old-current)))
  )

(defmeth coco-proto :dispose-of-models (a &optional)
  (let ((old-current (if T		; (not (eq a 'current))
			 (send self :return-model-number 'current) nil))
	(result
	 (dolist
	  (i (if (atom a) (list a) a) nil)
	  (if (send self :make-current i)
	      (coco-simple-command 139 2
				   :coco-id (slot-value 'identification))))))
    (if (and old-current
	     (not (and (numberp a) (equalp old-current a))))
	(send self :make-current old-current))
					; Current disposed ??
    result)
  )

(defmeth coco-proto :dispose-of-model (&optional (model 'current modelset) a b)
  (cond ((and (if modelset (not model)) (send self :has-slot 'model-number))
	 (send self :dispose-of-models (slot-value 'model-number)))
	((or (eq model 'number) (eq model 'list))
	 (send self :dispose-of-models a))
	((numberp model)
	 (send self :dispose-of-models model))
	((eq model 'interval)
	 (send self :dispose-of-interval a b))
	((listp model)
	 (send self :dispose-of-models model))
	(t (coco-simple-command 139 (encode (list 'base 'current 'last 'all)
                                            model
                                            (list 1 2 3 4) 2)
				:coco-id (slot-value 'identification))))
  )


(defmeth coco-proto :is-graphical (&optional (model 'current modelset))
  (let ((old-current (send self :before-set-current model modelset)))
    (let ((result (call-coco 202 1 :arg-char (concatenate 'string ";")
                   :arg-long '(0) :coco-id (slot-value 'identification))))
      (send self :after-set-current old-current result 'long-true model)))
  )

(defmeth coco-proto :is-decomposable (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 203 2 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-tree (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 3 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-connected (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 5 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-MIM-model (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 11 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-degenerated (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 12 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-mean-linear (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 13 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-d-collapsible (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 14 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-q-equivalent (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 15 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-homogeneous (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 16 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-full-specified (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 17 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-discrete (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 21 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-continuous (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 22 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-mixed (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 23 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-regression (&optional (model 'current modelset))
 (let ((old-current (send self :before-set-current model modelset)))
  (let ((result (call-coco 202 24 :arg-char (concatenate 'string ";")
                 :arg-long '(0) :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :is-submodel-of
  (&optional (model-1 'current model1set) (model-2 'base model2set))
  (let ((old (send self :before-set-both model-1 model-2 model1set model2set)))
   (if (and model-1 (not model-2)) (send self :make-base (car old)))
   (let ((result (call-coco 204 0 :arg-char (concatenate 'string ";")
                  :arg-long '(0) :coco-id (slot-value 'identification))))
    (if (stringp model-2) (send self :dispose-of-model 'base))
    (if (and (cadr old) model-2) (send self :make-base (cadr old)))
    (send self :after-set-current (car old) result 'long-true model-1)))
 )

(defmeth coco-proto :is-in-one-clique
  (edge &optional (model 'current modelset) options)
  (let ((old-current (send self :before-set-current model modelset)))
   (let ((result (call-coco 205 1 :arg-char (concatenate 'string edge ";")
                  :arg-long '(7 8) :coco-id (slot-value 'identification))))
    (send self :after-set-current old-current result 'long-true model)))
 )

(defmeth coco-proto :slice (&optional a b c options)
  (coco-enter-string 159 (concatenate 'string a b "/" c ";")
		     nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :print-common-decompositions ()
  (coco-simple-command 157 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :decompose-models (set)
  (coco-enter-string 158 set nil :coco-id (slot-value 'identification)))

;;;;;;;;


(defmeth coco-proto :test ()
  (coco-simple-command 160 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :find-log-l ()
  (coco-simple-command 161 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :find-deviance ()
  (coco-simple-command 162 nil :coco-id (slot-value 'identification)))

(defun print-test (test)
  (let ((number-of-cases  (nth 0 (car test)))
	(df               (nth 1 (car test)))
	(adj              (if (> (nth 2 (car test)) 0) (nth 2 (car test)) 0))
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
	(progn      ; Goodman-Kruskal's
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

;;; Method :compute-test is updated in "cococg.lsp":

(defmeth coco-proto :compute-test (&optional (model-1 nil model1set)
                                             (model-2 nil model2set))
  (let ((old (send self :before-set-both
              ;; model-1 = nil for test of 'current in call from 
              ;; :return-test-object at model-object?
                   (if (and model-1 model1set) model-1 'current)
              ;; model-2 = nil for test against 'base in call from 
              ;; :return-test-object at model-object?
                   (if (and model-2 model2set) model-2 'base)
                   model1set model2set)))
   (let ((result (call-coco 160 1
                  :arg-long (repeat 0 4)
                  :arg-double (iseq 13) ;;; (repeat 0 13)
                  :coco-id (slot-value 'identification))))
    (if (stringp model-2) (send self :dispose-of-model 'base))
    (if (and (cadr old) model-2) (send self :make-base (cadr old)))
    (send self :after-set-current (car old) result 'long-and-double model-1)))
 )


(defmeth coco-proto :compute-test (&optional (model-1 nil model1set)
                                             (model-2 nil model2set))
  (let ((old (send self :before-set-both
              ;; model-1 = nil for test of 'current in call from 
              ;; :return-test-object at model-object?
                   (if (and model-1 model1set) model-1 'current)
              ;; model-2 = nil for test against 'base in call from 
              ;; :return-test-object at model-object?
                   (if (and model-2 model2set) model-2 'base)
                   model1set model2set)))
   (let ((result (call-coco 160 1
                  :arg-long   (repeat 0  5) ;;  4
                  :arg-double (repeat 0 14) ;; 12
                  :coco-id (slot-value 'identification))))
    (if (stringp model-2) (send self :dispose-of-model 'base))
    (if (and (cadr old) model-2) (send self :make-base (cadr old)))
    (send self :after-set-current (car old) result 'long-and-double model-1)))
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
	(log-l-current             (nth 0 (cadr x)))
	(log-l-base                (nth 1 (cadr x)))
	(deviance                  (nth 2 (cadr x)))
	(probability               (nth 3 (cadr x)))
	(adjusted-probability      (nth 4 (cadr x))))
    (format t  "Deviance:         ~12,3f ~12,5f ~12,5f ~%"
	    deviance probability adjusted-probability))
  )

;;; Method :compute-deviance is updated in "cococg.lsp":

(defmeth coco-proto :compute-deviance (&optional (model-1 nil model1set)
                                                 (model-2 nil model2set))
  (let ((old (send self :before-set-both model-1 model-2 model1set model2set)))
   (let ((result (call-coco 162 1
		    :arg-long (repeat 0 8)
		    :arg-double (iseq 5) ;;; (repeat 0 5)
                  :coco-id (slot-value 'identification))))
    (if (stringp model-2) (send self :dispose-of-model 'base))
    (if (and (cadr old) model-2) (send self :make-base (cadr old)))
    (send self :after-set-current (car old) result 'long-and-double model-1)))
 )

;;; Update of method :compute-deviance from "cocometh.lsp":

(defmeth coco-proto :compute-deviance (&optional (model-1 nil model1set)
                                                 (model-2 nil model2set))
  (let ((old (send self :before-set-both model-1 model-2 model1set model2set)))
   (let ((result (call-coco 162 1
		    :arg-long   (repeat 0 9) ;; 8
		    :arg-double (repeat 0 7) ;; 5
                  :coco-id (slot-value 'identification))))
    (if (stringp model-2) (send self :dispose-of-model 'base))
    (if (and (cadr old) model-2) (send self :make-base (cadr old)))
    (send self :after-set-current (car old) result 'long-and-double model-1)))
 )

(defmeth coco-proto :exact-test ()
  (format t "Use (set-switch 'exact-test 'all) and (test) ~%")
  (coco-simple-command 163 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :partitioning-test ()
  (coco-simple-command 164 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :test-one-edge ()
  (coco-simple-command 165 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :factorize (&optional (code 'edges) (set ";"))
  (if (eq code 'edges)
      (coco-enter-string 166 set 1 :coco-id (slot-value 'identification))
    (coco-enter-string 167 set 1 :coco-id (slot-value 'identification))))

(defmeth coco-proto :show-tests ()
  (coco-simple-command 170 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :dispose-of-tests ()
  (coco-simple-command 171 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :dispose-of-tables ()
  (coco-simple-command 172 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :dispose-of-q-table (set)
  (coco-enter-string 173 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :dispose-of-probabilities ()
  (coco-simple-command 174 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :dispose-of-all-q-tables ()
  (coco-simple-command 175 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :generate-decomposable (&optional (just nil))
  (if just (send self :just))
  (coco-enter-string 180 "@" nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :generate-graphical (&optional (just nil))
  (if just (send self :just))
  (coco-enter-string 181 "@" nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :drop-factor (factor &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 182 factor nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :drop-edges (gc &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 183 gc nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :add-edges (gc &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 184 gc nil :coco-id (slot-value 'identification)))


(defmeth coco-proto :drop-interactions (gc &optional (just nil) (type 'all))
  (if just (send self :just))
  (coco-enter-string 185 gc
		     (encode (list 'all 'discrete 'linear 'quadratic) type
			     (list 0 1 2 3) 0)
		     :coco-id (slot-value 'identification)))

(defmeth coco-proto :add-interactions (gc &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 186 gc nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :reduce-generator (set &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 187 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :remove-generator (set &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 188 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :remove-total-interaction (set &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 189 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :meet-of-models (&optional (just nil) (dispose nil))
  (if just (send self :just))
  (coco-enter-string 177 "@" nil :coco-id (slot-value 'identification))
  (let ((result (send self :return-model 'last)))
   (if dispose (send self :dispose-of-model 'last))
   result)
  )

(defmeth coco-proto :join-of-models (&optional (just nil) (dispose nil))
  (if just (send self :just))
  (coco-enter-string 178 "@" nil :coco-id (slot-value 'identification))
  (let ((result (send self :return-model 'last)))
   (if dispose (send self :dispose-of-model 'last))
   result)
  )

(defmeth coco-proto :difference-of-models (&key (edges nil) (dispose T))
 (call-coco 179 (if edges 1 2) :coco-id (slot-value 'identification))
 (let ((result (send self :return-model 'last)))
   (if dispose (send self :dispose-of-model 'last))
   result)
 )

(defmeth coco-proto :fix-edges (edges)
  (coco-enter-string 191 edges nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :and-fix-edges (edges)
  (coco-enter-string 192 edges nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :backward
  (&key (only nil) (reversed nil) (sorted nil) (short nil)
	(p-accepted 0.05 set-accepted) (p-rejected 0.05 set-rejected)
	(decomposable-mode nil set-decomposable)
	(recursive nil) (coherent nil) (headlong nil)
	(follow nil) (least-significant T) (separators nil) (edges T)
        (model 'current modelset))
  (let ((old-current (send self :before-set-current model modelset)))
    (if set-accepted
	(send self :set-acceptance p-accepted))
    (if set-rejected
	(send self :set-rejection p-rejected))
    (if set-decomposable
	(send self :set-switch 'decomposable-mode
	      (if decomposable-mode 'on 'off)))
    (if only
	(coco-simple-command 199   1 :coco-id (slot-value 'identification)))
    (if reversed
	(coco-simple-command 199   2 :coco-id (slot-value 'identification)))
    (if sorted
	(coco-simple-command 199   3 :coco-id (slot-value 'identification)))
    (if short
	(coco-simple-command 199   5 :coco-id (slot-value 'identification)))
    (if recursive
	(coco-simple-command 199  10 :coco-id (slot-value 'identification)))
    (if coherent
	(coco-simple-command 199  11 :coco-id (slot-value 'identification)))
    (if headlong
	(coco-simple-command 199  12 :coco-id (slot-value 'identification)))
    (if follow
	(coco-simple-command 199  13 :coco-id (slot-value 'identification)))
    (if (not least-significant)
	(coco-simple-command 199  20 :coco-id (slot-value 'identification)))
    (if (send self :has-slot 'model-number)
	(send self :make-current (slot-value 'model-number)))
    (coco-simple-command 200 (encode (list 'edges 'interactions)
                              edges (list 1 2) (if edges 1 2))
			 :coco-id (slot-value 'identification))
    (if old-current (send self :make-current old-current)))
  )

(defmeth coco-proto :forward
  (&key (only nil) (reversed nil) (sorted nil) (short nil)
	(p-accepted 0.05 set-accepted) (p-rejected 0.05 set-rejected)
	(decomposable-mode nil set-decomposable)
	(recursive nil) (coherent nil) (headlong nil)
	(all-significant T) (separators nil) (edges T)
        (model 'current modelset) )
  (let ((old-current (send self :before-set-current model modelset)))
    (if set-accepted
	(send self :set-acceptance p-accepted))
    (if set-rejected
	(send self :set-rejection p-rejected))
    (if set-decomposable
	(send self :set-switch 'decomposable-mode
	      (if decomposable-mode 'on 'off)))
    (if only
	(coco-simple-command 199   1 :coco-id (slot-value 'identification)))
    (if reversed
	(coco-simple-command 199   2 :coco-id (slot-value 'identification)))
    (if sorted
	(coco-simple-command 199   3 :coco-id (slot-value 'identification)))
    (if short
	(coco-simple-command 199   5 :coco-id (slot-value 'identification)))
    (if recursive
	(coco-simple-command 199  10 :coco-id (slot-value 'identification)))
    (if coherent
	(coco-simple-command 199  11 :coco-id (slot-value 'identification)))
    (if headlong
	(coco-simple-command 199  12 :coco-id (slot-value 'identification)))
    (if (not all-significant)
	(coco-simple-command 199  21 :coco-id (slot-value 'identification)))
    (if (send self :has-slot 'model-number)
	(send self :make-current (slot-value 'model-number)))
    (coco-simple-command 201 (encode (list 'edges 'interactions)
                              edges (list 1 2) (if edges 1 2))
			 :coco-id (slot-value 'identification))
    (if old-current (send self :make-current old-current)))
  )

(defmeth coco-proto :set-main-effects (&optional (set 'what))
  (coco-enter-string 206 set nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :read-base-model (&optional (model 'what))
  (coco-enter-string 207 model nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :fix-in (&optional (gc 'what))
  (coco-enter-string 208 gc nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :fix-out (&optional (gc 'what))
  (coco-enter-string 209 gc nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :add-fix-in (&optional (gc 'what))
  (coco-enter-string 210 gc nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :add-fix-out (&optional (gc 'what))
  (coco-enter-string 211 gc nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :redo-fix-in ()
  (coco-simple-command 212 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :redo-fix-out ()
  (coco-simple-command 213 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-search (code)
  (coco-simple-command (encode (list 'smallest 'graphical 'alternating
                                'hierarchical 'rough) code
                        (list 214 215 214 215 214) 214) 
                       (encode (list 'smallest 'graphical 'alternating
                                'hierarchical 'rough) 
                        code (list 1 2 3 1 3) 2)
		       :coco-id (slot-value 'identification)))

(defmeth coco-proto :set-graphical-search (&optional (code 'flop))
  (send self :set-switch 'graphical-search code))

(defmeth coco-proto :dispose-of-eh (&optional (code 'all))
  (coco-simple-command 216 (encode (list 'all 'duals 'a-duals 'r-duals
                                    'classes 'accepted 'rejected) 
                            code (list 10 1 2 3 5 6 7) 0)
		       :coco-id (slot-value 'identification)))

(defmeth coco-proto :extract (class &optional (sub-class nil)
				    &key (coco-id *current-coco*))
  (coco-simple-command 220 (+ (encode (list 'accepted 'rejected
                                            'a-duals 'r-duals
                                            'a-dual  'r-dual)
                                      class (list 1 2 3 4 3 4) class)
                              (encode (list 'decomposable 'graphical
                                            'hierarchical nil)
                                      sub-class (list 10 20 30 0)
                                      (if sub-class 0 sub-class)))
		       :coco-id (slot-value 'identification)))

(defmeth coco-proto :find-dual (&optional (dual 'both) (sub-class nil))
  (coco-simple-command 224
                        (+ (encode (list 'a-duas 'r-dual 'both 'both-duals)
                            dual (list 1 2 5 5) dual)
                         (encode (list 'decomposable 'graphical 'hierarchical)
                          sub-class (list 10 20 30)
                          (if sub-class 0 sub-class)))
		       :coco-id (slot-value 'identification))
  )

(defmeth coco-proto :eh (&key (strategy 'smallest) (sub-class nil))
  (coco-simple-command 226 (+ (encode (list 'smallest 'alternating 'rough)
                               strategy (list 1 2 3) (if strategy 5 strategy))
                            (encode (list 'decomposable 'graphical
                                          'hierarchical)
                             sub-class (list 10 20 30)
                             (if sub-class 0 sub-class)))
		       :coco-id (slot-value 'identification))
  )

(defmeth coco-proto :search-base ()
  (coco-simple-command 228 nil :coco-id (slot-value 'identification)))

(defmeth coco-proto :to-search-interval (action a b)
  (let ((old-current (send self :before-set-current b T)))
    (do ((i b (send self :return-model-number 'current)))
	((or (not i) (< i a)) T)
	(coco-simple-command (encode (list 'fit 'accept 'reject) action
                                     (list 221 222 223)
                                     action)
			     2 :coco-id (slot-value 'identification))
	(send self :make-current 'previous))
    (if old-current (send self :make-current old-current)))
  )

(defmeth coco-proto :to-search-models (action a &optional)
  (let ((old-current (if T		; (not (eq a 'current))
			 (send self :return-model-number 'current) nil))
	(result
	 (dolist
	  (i (if (atom a) (list a) a) nil)
	  (if (send self :make-current i)
	      (coco-simple-command (encode (list 'fit 'accept 'reject) action
                                           (list 221 222 223) action)
				   2 :coco-id (slot-value 'identification))))))
    (if old-current (send self :make-current old-current))
    result)
  )

(defmeth coco-proto :to-search (action model &optional a b)
  (cond ((eq model 'interval)
	 (send self :to-search-interval action a b))
	((or (eq model 'number) (eq model 'list))
	 (send self :to-search-models action a))
	((numberp model)
	 (send self :to-search-models action model))
	((listp model)
	 (send self :to-search-models action model))
	(t  (coco-simple-command (encode (list 'fit 'accept 'reject) action
                                  (list 221 222 223) action)
                                 (encode (list 'base 'current 'last 'all)
                                         model (list 1 2 3 4) 2)
				 :coco-id (slot-value 'identification))))
  )

(defmeth coco-proto :add-dual-to-class (&optional (dual 'a-dual)
						  (class 'accepted)
						  (sub-class nil))
  (coco-simple-command 227 (+ (encode (list 'r-dual 'a-dual) dual
                                      (list 1 2) dual)
                            (encode (list 'accepted 'rejected) class
                                    (list 0 2) class)
                            (encode (list 'decomposable 'graphical
                                          'hierarchical)
                             sub-class (list 10 20 30)
                             (if sub-class 0 sub-class)))
		       :coco-id (slot-value 'identification))
  )

(defmeth coco-proto :fit-EH (&optional (dual 'smallest) (sub-class nil))
  (coco-simple-command 225 (+ (encode (list 'r-dual 'a-dual
                                            'smallest-dual 'largest-dual
                                            'both-duals 'both-dual 'both) 
                               dual (list 1 2 3 4 5 5 5) dual)
                            (encode (list 'decomposable 'graphical
                                          'hierarchical)
                             sub-class (list 10 20 30)
                             (if sub-class 0 sub-class)))
		       :coco-id (slot-value 'identification))
  )

(defmeth coco-proto :fit (model &optional a b
				&key (coco-id *current-coco*))
  (cond ((stringp model)
	 (coco-enter-string 217 model nil
			    :coco-id (slot-value 'identification)))
	((and (eq model 'models) (stringp a))
	 (coco-enter-string 217 a nil :coco-id (slot-value 'identification)))
	((or (eq model 'r-dual) (eq model 'a-dual) (eq model 'both-dual)
	     (eq model 'smallest-dual) (eq model 'largest-dual))
	 (send self :fit-EH model a))
	(t (send self :to-search 'fit model a b)))
  )

(defmeth coco-proto :accept (model &optional a b
				   &key (coco-id *current-coco*))
  (cond ((eq 'what model)
	 (coco-enter-string 218 'what nil
			    :coco-id (slot-value 'identification)))
	((stringp model)
	 (coco-enter-string 218 model nil
			    :coco-id (slot-value 'identification)))
	((and (eq model 'models) (stringp a))
	 (coco-enter-string 218 a nil :coco-id (slot-value 'identification)))
	((or (eq model 'r-dual) (eq model 'a-dual))
	 (send self :add-dual-to-class model 'accepted a))
	(t (send self :to-search 'accept model a b)))
  )

(defmeth coco-proto :reject (model &optional a b
				   &key (coco-id *current-coco*))
  (cond ((eq 'what model)
	 (coco-enter-string 219 'what nil
			    :coco-id (slot-value 'identification)))
        ((stringp model)
	 (coco-enter-string 219 model nil
			    :coco-id (slot-value 'identification)))
	((and (eq model 'models) (stringp a))
	 (coco-enter-string 219 a nil :coco-id (slot-value 'identification)))
	((or (eq model 'r-dual) (eq model 'a-dual))
	 (send self :add-dual-to-class model 'rejected a))
	(t (send self :to-search 'reject model a b)))
  )



(defproto coco-model-proto '(model-number) '(instances-model) coco-proto)

(defmeth coco-model-proto :compute-test-against-model-object (base)
  (cond ((not (= (slot-value 'identification)
		 (send base :slot-value 'identification)))
	 (format t "Models not in same CoCo-object ~%"))
	(t (let ((old-current (send base :return-model-number 'current))
		 (old-base (send base :return-model-number 'base)))
	     (send self :make-current
		   (slot-value 'model-number))
	     (send self :make-base
		   (send base :slot-value 'model-number))
	     (let ((result (send self :compute-test nil nil)))
	       (send self :make-current old-current)
	       (send self :make-base old-base)
	       result))))
  )

(defmeth coco-model-proto :isnew (model coco-object &key title)
  (slot-value 'identification
	      (send coco-object :slot-value 'identification))
  (if (stringp model)
      (progn (send self :read-model model)
	     (slot-value 'model-number
			 (send self :return-model-number 'last)))
    (slot-value 'model-number
		(if (numberp model) model
		  (send self :return-model-number model))))
  (if title (send self :title title))
  (send coco-model-proto :slot-value 'instances-model
	(cons self (slot-value 'instances-model)))
  )

(provide "cocometh")

;;; 56789012345678901234567890123456789012345678901234567890123456789012345678
