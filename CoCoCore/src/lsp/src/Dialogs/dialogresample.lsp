;;; Copyright 1993 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.

;;;  Creates a ``modal-dialog'' for resampling.

(defun resample-dialog (&optional x)

  (let* ((dismiss-resample
	  (send modal-button-proto :new
		(modal-text "Dismiss resample dialog")
		:action #'(lambda () (send resample-dialog-window :remove))))

	 (replacement-item
	  (send toggle-item-proto :new
		(modal-text "Apply Replacement for Resampling") :value nil))

	 (fraction-label
	  (send text-item-proto :new "Number of cases to sample, Fraction:"))

	 (fraction-item (send edit-text-item-proto :new "0.50" :text-length 6))

	 (number-label
	  (send text-item-proto :new "Number of iteration:"))

	 (number-item (send edit-text-item-proto :new "10" :text-length 4))

;;;	 (clear-edges-item
;;;	  (send modal-button-proto :new
;;;		(modal-text "Clear edges: labels, width and resampled")
;;;		:action #'(lambda ()
;;;			    (if (send x :has-slot 'resampling-edges)
;;;				(send x :slot-value 'resampling-edges nil))
;;;			    (send x :remove-tests)
;;;			    (send x :redraw))))

	 (resample-item
	  (send modal-button-proto :new
		(modal-text "Backward Elimination with Resampling")
		:action
		#'(lambda ()
		    (send x :resampling-backward
			  (send x :return-case-list)
			  (send fraction-item :value)
			  (send number-item :value)
			  :replacement (send replacement-item :value)
			  :only nil :reversed nil :sorted nil :short T
			  :recursive T
			  :least-significant (send extreme-item :value)
			  :p-accepted (send p-accepted-item :value)
			  :p-rejected (send p-rejected-item :value)
			  :coherent (send coherent-item :value)
			  :headlong (send headlong-item :value)
			  :random-order (send random-item :value)
			  :follow (send follow-item :value)
			  :decomposable-mode (send decomposable-item :value)
			  :separators nil :edges T
			  :edge-lists
			  (if (and (send x :has-slot 'resampling-edges)
				   (send x :slot-value 'resampling-edges))
			      (send x :slot-value 'resampling-edges)
			    nil)))))

	 (cancel (send modal-button-proto :new "Cancel"))

	 (ok (send  modal-button-proto :new "Ok"
		    :action
		    #'(lambda ()
			(list
			 (send fraction-item :value)
			 (send number-item :value)
			 (send replacement-item :value)
			 ))
		    ))
	
	 (resample-dialog-window
	  (send modal-dialog-proto :new
		(list ;;; (list dismiss-resample)
		      replacement-item
		      (list fraction-label fraction-item)
		      (list number-label number-item)
		(list cancel ok)
;;;		resample-item
		)))
	 )

    (defmeth fraction-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))
    (defmeth number-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))

    (send resample-dialog-window :modal-dialog)
    )
  )
