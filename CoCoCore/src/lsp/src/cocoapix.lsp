
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This is the interface functions from XLISP-STAT to CoCo

(defun ok-coco (result)
  (let* ((ifail (car (car result)))
	 (error-string
	  (if (= (car (cadr result)) *ended-coco*)
	      "This CoCo-object ended ~%"
	    (if (or (= ifail *no-ifail*) (= ifail *no-ifail-return*))
		nil
	      (case ifail
		    ( 1 "This CoCo-object not started ~%")
		    ( 2 "This CoCo-object ENDED ~%")
		    (11 "No Data read ~%")
		    (12 "No Specification read ~%")
		    (13 "No Observations read ~%")
		    (15 "Factor-set not subset of ~
                            missing excluded ~%")
		    (19 "Not a generating class ~%")
		    (20 "~% No such model ~%")
		    (21 "No model with that number ~%")
		    (22 "No CURRENT model ~%")
		    (23 "No BASE model ~%")
		    (24 "No LAST model ~%")
		    (25 "No CURRENT or BASE model ~%")
		    (26 "No CURRENT and BASE model ~%")
		    (27 "CURRENT not submodel of BASE ~%")
		    (28 "CURRENT and BASE model do not have same causal structure ~%")
		    (29 "Model is not pure discrete ~%")
		    (30 "Out of space ~%")
		    (31 "Out of space: N-array ~%")
		    (32 "Out of space: P-array ~%")
		    (33 "Out of space: Q-array ~%")
		    (34 "Out of space: R-array ~%")
		    (35 "Out of space: S-array ~%")
		    (36 "Out of space: SS-array ~%")
		    (37 "Out of space: T-array ~%")
		    (41 "Out of space: CURRENT ~%")
		    (42 "Out of space: BASE ~%")
		    (43 "Out of space: Marginals ~%")
		    (44 "Out of space: Adjusted DF ~%")
		    (45 "Out of space: IPS ~%")
		    (46 "Out of space: EM ~%")
		    (46 "Out of space: Collaps ~%")
		    (50 "No such file ~%")
                    (51 "Filename not set ~%")
                    (52 "No parser table: Unable to decide on context of actions ~%")
                    (56 "Unable to write in temporary directory ~%")
                    (57 "No COCO_TMP environment variable ~%")
                    (58 "No COCO_LIB environment variable ~%")
                    (59 "(No COCO_HOME environment variable) ~%")
		    (60 "Invalid vector in argument ~%")
		    (70 "Vector in argument too short ~%")
		    (71 "To few factors: Two expected ~%")
		    (72 "Not implemented for EM ~%")
		    (73 "No such function/Not implemented ~%")
		    (80 "No such factor ~%")
		    (81 "No cutpoints for this factor ~%")
		    (t  "Error ~%"))))))
    (if error-string
	(progn (setf *coco-errors* (cons error-string *coco-errors*))
	       (format t error-string)
	       nil)
      T))
  )

;

;; Updated in "cococg.lsp":

(defun call-coco (code &optional (sub-code 0) &key (coco-id *current-coco*)
		       (arg-char NIL) (arg-long NIL) (arg-double NIL))
  (let* ((arg-long (if (listp arg-long) arg-long 
		     (if arg-long (list arg-long))))
	 (arg-double (if (listp arg-double) arg-double
		       (if arg-double (list arg-double))))
	 (arg-char-int (concatenate 'list
				    (if arg-char
					(string-int
					 (if (listp arg-char)
					     (car arg-char) arg-char))) '(0)))
	 (n-arg (mapcar #'length (list nil arg-long arg-double arg-char-int))))
    (call-cfun *coco-entry-name* *no-ifail* coco-id code sub-code n-arg
	       0 arg-long (if arg-double (float arg-double)) arg-char-int))
  )

(defun call-coco-message (code &optional (sub-code 0)
			       &key (coco-id *current-coco*)
			       (arg-char NIL) (arg-long NIL) (arg-double NIL))
  (let ((result (call-coco code sub-code :coco-id coco-id :arg-char arg-char
			   :arg-long arg-long :arg-double arg-double)))
    (ok-coco result))
  )

(defun coco-simple-command (code &optional (sub-code nil)
				 &key (coco-id *current-coco*))
  (let ((result
	 (call-coco code (if sub-code sub-code 0) :coco-id coco-id)))
    (ok-coco result))
  )

(defun coco-set-simple-option (code &optional (sub-code nil)

				    &key (coco-id *current-coco*))
  (let ((result
	 (cond ((or (not sub-code) (eq 'what sub-code))
		(call-coco code -1 :coco-id coco-id))
	       (T (call-coco code sub-code :coco-id coco-id)))))
    (if (ok-coco result) (car (nth 3 result))))
  )

(defun but-last (y)
  (select y (iseq (1- (length y)))))
;
(defun coco-set-char-options (code argument &optional (sub-code nil)
				   &key (coco-id *current-coco*))
  (let ((argument (if (listp argument) (car argument) argument)))
    (let ((result
           (cond ((and sub-code (not (eq 'what argument)))
                  (call-coco code sub-code :coco-id coco-id :arg-char argument))
                 ((stringp argument)
                  (call-coco code 0 :coco-id coco-id :arg-char (list argument)))
                 ((or (eq 'what argument) (not argument) (eq "" argument))
                  (let ((tmp
                         (call-coco code (if sub-code sub-code -1)
                                    :coco-id coco-id :arg-char
                                    (concatenate 'string (repeat #\# 10)))))
                    (if (eq 70 (car (car tmp)))
                        (call-coco code (if sub-code sub-code -1)
                                   :coco-id coco-id :arg-char
                                   (concatenate
                                    'string (repeat #\# (nth 3 (nth 4 tmp)))))
                      tmp)))
                 (T (call-coco code 0 :coco-id coco-id :arg-char argument)))))
      (if (ok-coco result)
          (int-string (but-last (select (nth 8 result)
                                        (iseq (nth 3 (nth 4 result)))))))))
  )

(defun coco-enter-string (code argument &optional (sub-code nil)
			       &key (coco-id *current-coco*))
  (let ((result
	 (coco-set-char-options code argument sub-code :coco-id coco-id)))
    (if (or (not argument) (eq 'what argument)) result (if result T)))
  )

;
(defun coco-set-all-options (code string long double &optional (sub-code nil)
                                  &key (coco-id *current-coco*))
  (let ((string (if (listp string) (car string) string)))
    (let ((result
           (cond ((and sub-code (not (eq 'what string)))
                  (call-coco code sub-code :coco-id coco-id
                             :arg-char string :arg-long long
                             :arg-double double))
                 ((stringp string)
                  (call-coco code 0 :coco-id coco-id
                             :arg-char (list string) :arg-long long
                             :arg-double double))
                 ((or (eq 'what string) (not string) (eq "" string))
                  (let ((tmp
                         (call-coco code (if sub-code sub-code -1)
                                    :coco-id coco-id
                                    :arg-char (concatenate 'string
                                                           (repeat #\# 20))
                                    :arg-long (repeat 0 10)
                                    :arg-double (repeat -1 1))))
                    (if (eq 70 (car (car tmp)))
                        (call-coco code (if sub-code sub-code -1)
                                   :coco-id coco-id :arg-char
                                   (concatenate 'string
                                    (repeat #\# (nth 3 (nth 4 tmp))))
                                   :arg-long (repeat 0  (nth 1 (nth 4 tmp)))
                                   :arg-double (repeat -1 (nth 2 (nth 4 tmp))))
                      tmp)))
                 (T (call-coco code 0 :coco-id coco-id
                               :arg-char string :arg-long long
                               :arg-double double)))))
      (if (ok-coco result)
          (list (int-string (but-last (select (nth 8 result)
                                              (iseq (nth 3 (nth 4 result))))))
                (select (nth 6 result) (iseq (nth 1 (nth 4 result))))
                (select (nth 7 result) (iseq (nth 2 (nth 4 result))))
                ))))
  )

(defun coco-enter-all (code string long double &optional (sub-code nil)
                       &key (coco-id *current-coco*))
  (let ((result
	 (coco-set-all-options code string long double sub-code :coco-id coco-id)))
   (if (or (not string) (eq 'what string)) result (if result T)))
  )
;

(defun coco-set-long-options (code argument length &optional (sub-code nil)
				   &key (coco-id *current-coco*))
  (let ((result
	 (cond ((and sub-code (not (eq 'what argument)))
		(call-coco code sub-code :coco-id coco-id
			   :arg-long (if (numberp argument) (list argument)
				       argument)))
	       ((numberp argument)
		(call-coco code 0 :coco-id coco-id :arg-long (list argument)))
	       ((or (eq 'what argument)
		    (if (listp argument) (eq 'what (car argument)))
		    (not argument)
		    (if (listp argument) (not (car argument))))
		(let ((tmp
		       (call-coco code (if sub-code sub-code -1)
				  :coco-id coco-id
				  :arg-long (repeat 0 length))))
		  (if (eq 70 (car (car tmp)))
		      (call-coco code (if sub-code sub-code -1)
				 :coco-id coco-id
				 :arg-long (repeat 0 (nth 1 (nth 4 tmp))))
		    tmp)))
	       ((listp (car argument))
		(call-coco code 0 :coco-id coco-id :arg-long (car argument)))
	       (T (call-coco code 0 :coco-id coco-id :arg-long argument)))))
    (if (ok-coco result) (nth 6 result)))
  )

(defun coco-set-real-options (code argument length &optional (sub-code nil)
				   &key (coco-id *current-coco*))
  (let ((result
	 (cond (sub-code
		(call-coco code sub-code :coco-id coco-id
			   :arg-double (if (numberp argument)
					 (list argument)
				       argument)))
	       ((numberp argument)
		(call-coco code 0 :coco-id coco-id
			   :arg-double (list argument)))
	       ((or (eq 'what argument)
		    (if (listp argument) (eq 'what (car argument)))
		    (not argument)
		    (if (listp argument) (not (car argument))))
		(call-coco code -1 :coco-id coco-id
			   :arg-double (repeat (float 0.0) length)))
	       ((listp (car argument))
		(call-coco code 0 :coco-id coco-id :arg-double (car argument)))
	       (T (call-coco code 0 :coco-id coco-id :arg-double argument)))))
    (if (ok-coco result) (nth 7 result)))
  )

(provide "cocoapix")
