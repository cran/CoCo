;;; Copyright 1993 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.

;;;  Creates a ``modal-dialog'' for getting one value.

(defun one-value-dialog (&optional (model-number -1) (label "Value")
				   (all-values nil))
  (let* ((label-item (send text-item-proto :new label))

	 (*value-names*
	  (if all-values
	      (list
	       "'observed  " "'probabilities" "'expected     " "'unadjusted   "
	       "'f-res     " "'r-f          " "'g-res        " "'r-g          "
	       "'adjusted  " "'standardized " "'deviance     " "'freeman-tukey"
	       "'2n-m      " "'sqrt         " 
	       "'power     " "'index        "
	       "'zero      " "'leverage     " "'error         " )
	    (list
	     "'observed    " "'probabilities" "'expected     " "'unadjusted   "
	     "'adjusted    " "'standardized " "'deviance     " "'freeman-tukey"
	     "'power       " "'index        "
	     "'zero        " "'leverage     " "'error         " )))

	  (*value-list*
	   (if all-values
	       (list
		'observed      'probabilities  'expected       'unadjusted     
		'f-res         'r-f            'g-res          'r-g            
		'adjusted      'standardized   'deviance       'freeman-tukey  
		'2n-m          'sqrt           
		'power         'index          
		'zero          'leverage       'error          )
	     (list
	      'observed       'probabilities  'expected       'unadjusted     
	      'adjusted       'standardized   'deviance       'freeman-tukey  
	      'power          'index          
	      'zero           'leverage       'error          )))

	 (value-item (send choice-item-proto :new *value-names* :value 0))

	 (*model-names*
	  (list "'current" "'base" "'last"
		"'graph" "'window" "'number:"))

	 (*model-list*
	  (list 'current 'base 'last 'graph nil 'number))

	 (model-item (send choice-item-proto :new *model-names* :value 3))

	 (argument-item
	  (send edit-text-item-proto :new
		(format nil "~d" model-number) :text-length 3))

	 (complete-item
	  (send toggle-item-proto :new "Complete cells  "
		:value nil))	      
				      
	 (random-item		      
	  (send toggle-item-proto :new "Random table    "
		:value nil))	      
				      
	 (log-trans-item	      
	  (send toggle-item-proto :new "Log-transformed "
		:value nil))	      
				      
	 (permuted-item		      
	  (send toggle-item-proto :new "Permuted cells  "
		:value T))


	 (cancel (send modal-button-proto :new "Cancel"))

	 (ok (send  modal-button-proto :new "Ok"
		    :action
		    #'(lambda ()
			(list
			 (nth (send value-item :value) *value-list*)
			 (let ((model (nth (send model-item :value) 
					   *model-list*)))
			   (if (equal 'number model)
			       (send argument-item :value)
			     (if (equal 'graph model) model-number
			       model)))
			 (list
			  (send complete-item :value)
			  (send random-item :value)
			  (send log-trans-item :value)
			  (send permuted-item :value))))
		    ))
	
	 (one-value-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list label-item)
		 (list
		  (list value-item)
		  (list model-item argument-item
			complete-item random-item log-trans-item
			permuted-item))
		 (list cancel ok)
		 ))))

    (defmeth argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (send one-value-dialog-window :modal-dialog)
    )
  )
