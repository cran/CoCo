
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the editing model:


(defun split-dialog (x)
  (let* ((dismiss-split
	  (send modal-button-proto :new (modal-text "Dismiss split dialog")
		:action #'(lambda () (send model-dialog-window :remove))))
	
	 (by-argument-label
	  (send text-item-proto :new "By-Argument, set:"))

	 (by-argument-item
	  (let ((current-number (send x :return-model-number 'current))
		(result (send edit-text-item-proto :new
			      (to-string (send x :return-name-list) "")
			      :text-length 66)))
	    (send x :make-current current-number)
	    result))
	
	 (ignore-argument-label
	  (send text-item-proto :new "Ignore-Argument, set:"))

	 (ignore-argument-item
	  (let ((current-number (send x :return-model-number 'current))
		(result (send edit-text-item-proto :new "" :text-length 66)))
	    (send x :make-current current-number)
	    result))

	 (copy-vertices-item
	  (send toggle-item-proto :new
		(modal-text "Share vertices, (i.e. positions)")
		:value T))
	
	 (split-item
	  (send modal-button-proto :new
		(modal-text "Split <by-argument> <ignore-argument>")
		:action
		#'(lambda ()
		    (let ((by-argument
			    (send by-argument-item :gc x))
			  (ignore-argument
			    (send ignore-argument-item :gc x)))
		      (send x :split (split-name-string by-argument)
			    :ignore-name-list 
			    (split-name-string ignore-argument))))))

	 (split-block-item
	  (send modal-button-proto :new
		(modal-text "Split block recursive model <by-argument> ")
		:action
		#'(lambda ()
		    (send x :split-in-block-recursive
			  (send by-argument-item :gc x)))))

	 (model-dialog-window
	  (send modal-dialog-proto :new
		(if (kind-of-p x manager-proto)
		    (list
		     (list dismiss-split)
		     by-argument-label by-argument-item
		     ignore-argument-label ignore-argument-item
		     ;; create-graph-item
		     ;; copy-vertices-item
		     split-item
		     split-block-item
		     )
		    (list
		     (list dismiss-split)
		     by-argument-label by-argument-item
		     ignore-argument-label ignore-argument-item
		     ;; create-graph-item
		     copy-vertices-item
		     split-item
		     split-block-item
		     ))))
	 )

    (defmeth by-argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (defmeth by-argument-item :gc (graph)
      (let ((number (send self :value)))
	(if number
	    (send graph :return-model number)
	  (let ((gc (send self :text)))
	    (cond
	     ((equalp gc "'current")
	      (send graph :return-model 'current))
	     ((equalp gc "'base")
	      (send graph :return-model 'base))
	     ((equalp gc "'last")
	      (send graph :return-model 'last))
	     (t gc))))))

    (defmeth ignore-argument-item :value ()
      (let ((text (send self :text)))
	(if (not (equalp text ""))
	    (let ((digits (- (string-int text) 48)))
	      (if (and (<= 0 (min digits)) (<= (max digits) 9) )
		  (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))))
      )

    (defmeth ignore-argument-item :gc (graph)
      (let ((number (send self :value)))
	(if number
	    (send graph :return-model number)
	  (let ((gc (send self :text)))
	    (cond
	     ((equalp gc "'current")
	      (send graph :return-model 'current))
	     ((equalp gc "'base")
	      (send graph :return-model 'base))
	     ((equalp gc "'last")
	      (send graph :return-model 'last))
	     (t gc))))))
    )
  )
