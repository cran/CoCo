
;;; Copyright 1992, 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This is functions for CoCo within XLISP-STAT


(defun invalid-integer (x)
  (> x 2147483640)
  )

(def *my-not-a-number* (exp 1000))
;; #.POSITIVE-INFINITY

(defun invalid-real (x)
  (or (equalp x #.NOT-A-NUMBER)
   (not (< x #.POSITIVE-INFINITY))
   (< (abs (/ (- x *my-not-a-number*) *my-not-a-number*)) 0.0001))
  )

(defun invalid-real (x)
  (or (eq x #.NOT-A-NUMBER)
   (not (< x #.POSITIVE-INFINITY))
;;   (< (abs (/ (- x *my-not-a-number*) *my-not-a-number*)) 0.0001)
   )
  )

(defun invalid-real-list (x)
  (mapcar #'(lambda (i) (invalid-real i)) x)
  )

(defun replace-my-not-a-number (real-list)
  (mapcar #'(lambda (i) (if (invalid-real i) #.NOT-A-NUMBER i))
   ;; #.POSITIVE-INFINITY
   real-list)
  )

(defun string-int (str)
  (map-elements #'char-int (map-elements #'char str (iseq (length str))))
  )

(defun string-int-0 (str)
  (concatenate 'list (string-int str) '(0))
  )

(defun int-string (x)
  (if x (concatenate 'string (map-elements #'int-char x)))
  )

(defun encode (x a &optional (y nil) (d nil))
  (if y
      (if (not (eql (length x) (length y)))
          (stop "Encoding errror!!!")))
  (let ((b (which (map-elements #'equalp x a))))
   (if b (select (if y y (1+ (iseq (length x)))) (car b))
    (if d d a)))
 )

(defun on-off (hit)
 (if (stringp hit)
  (encode (list 'what 'off 'flop 'on) hit (list 2 -1 0 1) 2)
  (if hit 3 1)
  )
 )


;;; Function table-value is updaten in "cococg.lsp":

(defun table-value (code)
 (let ((x (list 'counts 'observed 'probabilities
		'expected 'unadjusted 'absolute
                'f-res 'r-f 'g-res 'r-g 'adjusted 'leverage 'c-res 'm-res
                'standardized 'standard 'x-res 'deviance '-2log 'l-res
                'freeman-tukey '2n-m 'sqrt 'power 'index 'zero 'error))
       (y (list 0 0 1 2 3 3 4 5 6 7 8 16 8 9 9 9 9
		10 10 10 11 12 12 13 14 15 63)))
 (encode x code y code))
 )

;;; Update of "table-value" from cocofunc.lsp:

(defun table-value (code)
  (let ((x (list 'counts 'observed 'probabilities
		 'expected 'unadjusted 'absolute
		 'f-res 'r-f 'g-res 'r-g 'adjusted 'leverage 'c-res 'm-res
		 'standardized 'standard 'x-res 'deviance '-2log 'l-res
		 'freeman-tukey '2n-m 'sqrt 'power 'index 'zero 'error
		 'canonical 'gs 'hs 'ks 'moment 'means 'covariance
		 'raw 'total 'ss 'ssds 'sigma 'determinants 'mk 'ms))
	(y (list 0 0 1 2 3 3 4 5 6 7 8 16 8 9 9 9 9 10 10 10 11 12 12 13 14 15
                63 2 3 4 5 6 7 8 9 10 11 12 12 13 14 15)))
    (encode x code y code))
  )


(defun encode-type-and-options (type &optional (random nil)
                                     (log-transformed nil)
                                (complete nil) (permuted T)
                                &key (uniform nil) (rankit nil) (probit nil))
    (list (+ (table-value type) (if complete 16 0) (if random 32 0)) -2
     (if random 1 0) (if log-transformed 1 0)
     (if complete 1 0) (if permuted 1 0) 
     (if uniform 1 0) (if rankit 1 0) (if probit 1 0))
  )

(defun to-string (a sep)
  (let ((result (if (stringp (car a)) (car a) (string (car a)))))
    (dolist (i (cdr a) result)
      (setf result (concatenate 'string result sep
				(if (stringp i) i (string i))))))
  )

(defun split-string-1 (str)
  (let ((result nil)
	(block nil))
    (dolist (c (map-elements #'char str (iseq (length str))) result)
	    (if (not (eql c #\Space))
		(if (or (eql c #\<) (eql c #\\) (eql c #\>) (eql c #\,)
			(eql c #\:) (eql c #\[) (eql c #\]))
		    (progn
		      (setf result (concatenate 'list result (list block)))
		      (setf block nil))
		  (setf block (concatenate 'list block (list c))))))
    (concatenate 'list result (list block)))
  )

(defun split-string-2 (str)
  (let ((result nil)
	(block nil)
	(long-names nil)
        (name nil))
    (dolist (c (map-elements #'char str (iseq (length str))) result)
      (if (not (eql c #\Space))
	  (if (eql c #\:)
	      (progn
		(setf long-names T)
		(if name
		    (setf block
			  (concatenate 'list block
				       (list (concatenate 'string name)))))
		;; (print (list "block" block))
		(setf name (list #\:)))
	    (if (or (eql c #\<) (eql c #\\) (eql c #\>)
		    (eql c #\,) (eql c #\[) (eql c #\]))
		(progn
		  (if long-names
		      (if name
			  (progn
			    (setf block
				  (concatenate 'list block
					       (list
						(concatenate 'string name))))
			    (setf result
				  (concatenate 'list result (list block)))
			    (setf name nil)))
		    (setf result (concatenate 'list result (list block))))
		  ;; (print (list "result" result))
		  (setf block nil))
	      (if long-names
		  (setf name (concatenate 'list name (list c)))
                (setf block (concatenate 'list block (list c))))))))
;;    (concatenate 'list result (list block))
    (if long-names
	(if name
	    (concatenate 'list result
			 (list (concatenate 'list block
					    (list
					     (concatenate 'string name))))))
      (concatenate 'list result (list block)))
    )
  )

(defun split-string (str &key (cococg nil))
  (if cococg (split-string-2 str)
	  (split-string-1 str))
  )

(defun split-string-chr (str chr &optional (exclude nil) (withchr T))
  (if (> (length str) 0)
      (let ((result nil)
	    (block (list chr)))
	(dolist (c (map-elements #'char str (iseq (length str))) result)
	  (if (and (not (eql c #\Space))
		   (or (not exclude) (not (equal c exclude))))
	      (if (eql c chr)
		  (progn
		    (setf result (concatenate 'list result (list block)))
		    (setf block (if withchr (list chr) nil)))
		(setf block (concatenate 'list block (list c))))))
	(setf result (concatenate 'list result (list block)))
	(mapcar #'(lambda (i) (concatenate 'string  i)) result)))
  )

(defun split-block-string (block) ;;  &key (cococg nil) ?
  (let* ((a (split-string (concatenate 'string block)
			  :cococg (equal *coco-type* "cococg")))
	 (b (select a (which a))))
    (mapcar #'(lambda (i) (if (listp i)
			      (concatenate 'string ":" i)
			      (if (stringp i) i ;; Updated here !!!
			    (concatenate 'string (list i)))))
	    (if (= 1 (length a)) (car b) b)))
  )

(defun split-name-string (names) ;;  &key (cococg nil)
  (if (listp names)
      names
    (let* ((a (split-string names :cococg (equal *coco-type* "cococg")))
	   (b (select a (which a))))
      (mapcar #'(lambda (i) (if (listp i)
				(concatenate 'string ":" i)
			      (if (stringp i) i ;; Updated here !!!
				(concatenate 'string (list i)))))
	      (if (= 1 (length a)) (car b) b))))
  )

(defun string-to-block-list (str)
  (let ((result nil)
	(block nil)
	(nanny nil))
    (dolist (c (map-elements #'char str (iseq (length str))) result)
	    (if (not (eql c #\Space))
		(if (or (eql c #\<) (eql c #\>) (eql c #\,))
		    (progn
		      (if (eql c #\>) (setf nanny T))
		      (setf result
			    (concatenate 'list result
					 (list (split-block-string block))))
		      (setf block nil))
		  (setf block (concatenate 'list block (list c))))))
    (list (concatenate 'list result (list (split-block-string block))) nanny))
  )

(defun copy-list-list (list)
  (let ((result nil))
    (dolist (i list result)
	    (setf result (concatenate 'list result (list (copy-list i)))))
    result)
  )

(defun copy-list-list (list)
  (mapcar #'(lambda (i) (copy-list i)) list)
  )

(provide "cocofunc")
