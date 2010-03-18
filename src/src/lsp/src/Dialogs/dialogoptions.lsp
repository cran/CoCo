
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for calling submenus for options.

(defun which-index (x i)
  (car (which (mapcar #'(lambda (j k)
			  (if (equalp i j) k)) x (iseq (length x))))))

(defun get-choice-dialog (label items &optional (initial 0))
  (let ((result
	 (let* ((label-item (send text-item-proto :new label))
		(value-item
		 (send choice-item-proto :new (mapcar #'string items)
		       :value (which-index items initial)))
		(cancel (send modal-button-proto :new "Cancel"))
		(ok (send  modal-button-proto :new "Ok"
			   :action #'(lambda ()
				       (nth (send value-item :value) items))))
		(get-choice-item-window
		 (send modal-dialog-proto :new
		       (list (list label-item)
			     (list value-item)
			     (list cancel ok)))))
	   (send get-choice-item-window :modal-dialog))))
    (if result result initial))
  )

(defun get-linked-toggle-item (graph action)
  (send linked-toggle-item-proto :new
	(toggle-text (format nil "~d" action))
	(send graph :slot-value 'identification) action
	:value (send graph :set-switch action)
	:action #'(lambda () (send graph :set-switch action
				   (not (send graph :set-switch action)))))
  )

(defun number-dec (x &optional (i (iseq 10)) (epsilon 0.000001))
  (let ((result (car (select i (which (< (abs (- (round (* (^ 10 i) x)) 
						 (* (^ 10 i) x)))
					 (* (abs x) epsilon)))))))
    (if result (max 2 result) 6))
  )

(defun get-real-dialog (text &key (initial nil))
  (let ((result (get-string-dialog
		 text
		 :initial (format nil "~,vf" (number-dec initial) initial))))
    (if result
	(eval (with-input-from-string (s result) (read s)))
      initial))
  )

(defun options-dialog (x)

  (let* ((dismiss-options
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda () (send options-dialog-window :remove))))

	 (formats-item
	  (send modal-button-proto :new
		(modal-text "Options for Formats")
		:action #'(lambda ()
			    (option-formats-dialog x))))
	 (tests-item
	  (send modal-button-proto :new
		(modal-text "Options for Tests and Model Search")
		:action #'(lambda () (option-tests-dialog x))))

	 (exact-item
	  (send modal-button-proto :new
		(modal-text "Options for Exact Tests")
		:action #'(lambda () (option-exact-dialog x))))

	 (ips-item
	  (send modal-button-proto :new
		(modal-text "Options for the Ips algorithm")
		:action #'(lambda ()
			    (option-ips-dialog x))))

	 (em-item
	  (send modal-button-proto :new
		(modal-text "Options for the EM algorithm")
		:action #'(lambda ()
			    (option-em-dialog x))))

	 (specification-item
	  (send modal-button-proto :new
		(modal-text "%%% Options for Specification")
		:action #'(lambda ()
			    (send x :status 'specification)
			    (option-specification-dialog x))))

	 (observation-item
	  (send modal-button-proto :new
		(modal-text "(-) Options for Observations and Limits")
		:action #'(lambda ()
			    (option-limits-dialog x))))

	 (files-item
	  (send modal-button-proto :new
		(modal-text "Options for Files")
		:action #'(lambda () (option-files-dialog x))))

	 (other-item
	  (send modal-button-proto :new
		(modal-text "Other Options")
		:action #'(lambda () (option-other-dialog x))))

	 (options-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-options)
		      formats-item tests-item exact-item
		      ips-item em-item
		      observation-item specification-item
		      files-item other-item
		      )))))
  )
