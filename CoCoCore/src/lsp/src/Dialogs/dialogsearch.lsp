
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the stepwise model selection:



(defun stepwise-dialog (x)

  (def *x-move* 0)

  (let* ((dismiss-stepwise
	  (send modal-button-proto :new
		(modal-text "Dismiss stepwise selection dialog")
		:action #'(lambda () (send stepwise-dialog-window :remove))))

	 (exact-test-item
	  (send linked-toggle-item-proto :new
		(modal-text "Exact-Test (CoCo-object)")
		(send x :slot-value 'identification) 'exact-test
		:value (send x :set-exact-test)
		:action #'(lambda ()
			    (send x :set-exact-test
				  (not (send x :set-exact-test))))))

	 (fix-edges-item
	  (send modal-button-proto :new
		(modal-text "Fix edges (CoCo-object) ...")
		:action #'(lambda ()
			    (send x :fix-edges
				  (get-string-dialog
				   "Edges (as generating class):"
				   :initial (send x :return-fix 'edges))))))

	 (rejected-edges-item
	  (send modal-button-proto :new
		(modal-text "Clear edges rejected by coherence")
		:action #'(lambda ()
			    (send x :rejected-edges nil))))

	 (accepted-edges-item
	  (send modal-button-proto :new
		(modal-text "Clear edges accepted by coherence")
		:action #'(lambda ()
			    (send x :accepted-edges nil))))

	 (p-accepted-label
	  (send text-item-proto :new "P-value accepted (remove edge):"))
	 (p-accepted-item
	  (send edit-text-item-proto :new "0.10" :text-length 6))

	 (p-rejected-label
	  (send text-item-proto :new "P-value rejected (add edge):   "))
	 (p-rejected-item
	  (send edit-text-item-proto :new "0.05" :text-length 6))

	 (decomposable-item
	  (send toggle-item-proto :new
		(modal-text "Only Decomposable Models (unlinked!)") :value T))

	 (extreme-item
	  (send toggle-item-proto :new
		(modal-text "Only Most Extreme Edge Removed/Added") :value T))
	
	 (coherent-item
	  (send toggle-item-proto :new
		(modal-text "Apply Coherence") :value T))

	 (headlong-item
	  (send toggle-item-proto :new
		(modal-text "Headlong Backward/Forward") :value T))
	
	 (random-item
	  (send toggle-item-proto :new
		(modal-text "Visit Edges in Random Order") :value T))
	
	 (follow-item
	  (send toggle-item-proto :new
		(modal-text "Follow: Local tests") :value T))
	
	 (in-graph-item
	  (send toggle-item-proto :new
		(modal-text "Model selection in graph window") :value T))

	 (recursive-item
	  (send toggle-item-proto :new
		(modal-text "Recursive Backward/Forward") :value T))

	 (blockwise-item
	  (send toggle-item-proto :new
		(modal-text "Blockwise Backward/Forward") :value nil))
	
	 (label-all-edges-item
	  (send modal-button-proto :new (modal-text "`L': Label all edges")
		:action #'(lambda () (send x :label-all-edges
					   :decomposable-mode
					   (send decomposable-item :value)
					   :make-graph nil))))
	
	 (graph-backward-item
	  (send
	   modal-button-proto :new (modal-text "Backward elimination")
	   :action
	   #'(lambda ()
	       (if (or (send x :blocks) (send in-graph-item :value))
		   (progn
		     (if (send x :static)
			 (send x :location
			       (- (+ (car (send x :location)) *x-move*) 1)
			       (- (car (cdr (send x :location))) 1)))
		     (if (send recursive-item :value)
			 (if (send blockwise-item :value)
			     (send x :block-backward
				   :block nil
				   :recursive T
				   :p-accepted (send p-accepted-item :value)
				   :p-rejected (send p-rejected-item :value)
				   :coherent (send coherent-item :value)
				   :headlong (send headlong-item :value)
				   :random-order (send random-item :value)
				   :follow (send follow-item :value)
				   :decomposable-mode
				   (send decomposable-item :value)
				   :least-significant
				   (send extreme-item :value)
				   :x-move *x-move*)
			   (send x :drop-least-significant-edge
				 :recursive T
				 :p-accepted (send p-accepted-item :value)
				 :p-rejected (send p-rejected-item :value)
				 :coherent (send coherent-item :value)
				 :headlong (send headlong-item :value)
				 :random-order (send random-item :value)
				 :follow (send follow-item :value)
				 :decomposable-mode
				 (send decomposable-item :value)
				 :least-significant (send extreme-item :value)
				 :x-move *x-move*))
		       (send x :drop-least-significant-edge
			     :recursive nil
			     :p-accepted (send p-accepted-item :value)
			     :p-rejected (send p-rejected-item :value)
			     :coherent (send coherent-item :value)
			     :random-order (send random-item :value)
			     :follow (send follow-item :value)
			     :decomposable-mode (send decomposable-item :value)
			     :least-significant (send extreme-item :value))))
		 (let ((current-number
			(send x :return-model-number 'current)))
		   (if (send x :make-graph-current-model :redraw-plots nil)
		       (send x :backward
			     :only nil :reversed nil :sorted T :short nil
			     :recursive (send recursive-item :value)
			     :p-accepted (send p-accepted-item :value)
			     :p-rejected (send p-rejected-item :value)
			     :coherent (send coherent-item :value)
			     :headlong (send headlong-item :value)
			     :follow (send follow-item :value)
			     :decomposable-mode (send decomposable-item :value)
			     :least-significant (send extreme-item :value)))
		   (send x :make-current current-number))))))
	 
	 (graph-forward-item
	  (send
	   modal-button-proto :new (modal-text "Forward selection")
	   :action
	   #'(lambda ()
	       (if (or (send x :blocks) (send in-graph-item :value))
		   (progn
		     (if (send x :static)
			 (send x :location
			       (- (+ (car (send x :location)) *x-move*) 1)
			       (- (car (cdr (send x :location))) 1)))
		     (if (send recursive-item :value)
			 (if (send blockwise-item :value)
			     (send x :block-forward
				   :block nil
				   :recursive T
				   :most-significant (send extreme-item :value)
				   :p-accepted (send p-accepted-item :value)
				   :p-rejected (send p-rejected-item :value)
				   :coherent (send coherent-item :value)
				   :headlong (send headlong-item :value)
				   :random-order (send random-item :value)
				   :follow (send follow-item :value)
				   :decomposable-mode
				   (send decomposable-item :value)
				   :x-move *x-move*)
			   (send x :add-most-significant-edge
				 :recursive T
				 :most-significant (send extreme-item :value)
				 :p-accepted (send p-accepted-item :value)
				 :p-rejected (send p-rejected-item :value)
				 :coherent (send coherent-item :value)
				 :headlong (send headlong-item :value)
				 :random-order (send random-item :value)
				 :follow (send follow-item :value)
				 :decomposable-mode
				 (send decomposable-item :value)
				 :x-move *x-move*))
		       (send x :drop-least-significant-edge
			     :recursive nil
			     :p-accepted (send p-accepted-item :value)
			     :p-rejected (send p-rejected-item :value)
			     :coherent (send coherent-item :value)
			     :random-order (send random-item :value)
			     :headlong nil
			     :follow (send follow-item :value)
			     :decomposable-mode (send decomposable-item :value)
			     :most-significant (send extreme-item :value))))
		 (let ((current-number
			(send x :return-model-number 'current)))
		   (if (send x :make-graph-current-model :redraw-plots nil)
		       (send x :forward
			     :only nil :reversed nil :sorted T :short nil
			     :recursive (send recursive-item :value)
			     :p-accepted (send p-accepted-item :value)
			     :p-rejected (send p-rejected-item :value)
			     :coherent (send coherent-item :value)
			     :headlong (send headlong-item :value)
			     :random-order (send random-item :value)
			     :follow (send follow-item :value)
			     :decomposable-mode (send decomposable-item :value)
			     :all-significant
			     (not (send extreme-item :value))))
		   (send x :make-current current-number))))))

	 (replacement-item
	  (send toggle-item-proto :new
		(modal-text "Apply Replacement for Resampling") :value nil))

	 (fraction-label
	  (send text-item-proto :new "Number of cases to sample, Fraction:"))
	 (fraction-item (send edit-text-item-proto :new "0.50" :text-length 6))

	 (number-label
	  (send text-item-proto :new "Number of iteration:"))
	 (number-item (send edit-text-item-proto :new "10" :text-length 4))

	 (clear-edges-item
	  (send modal-button-proto :new
		(modal-text "Clear edges: labels, width and resampled")
		:action #'(lambda ()
			    (if (send x :has-slot 'resampling-edges)
				(send x :slot-value 'resampling-edges nil))
			    (send x :remove-tests)
			    (send x :redraw))))

	 (resample-item
	  (send modal-button-proto :new
		(modal-text "Backward Elimination with Resampling ...")
		:action
		#'(lambda ()
		    (let ((arguments (resample-dialog x)))
		      (if arguments 
			  (send x :resampling-backward
				(send x :return-case-list)
				(car arguments)
				(cadr arguments)
				:replacement (caddr arguments)
				:only nil
				:reversed nil :sorted nil :short T
				:recursive T
				:least-significant
				(send extreme-item :value)
				:p-accepted (send p-accepted-item :value)
				:p-rejected (send p-rejected-item :value)
				:coherent (send coherent-item :value)
				:headlong (send headlong-item :value)
				:random-order (send random-item :value)
				:follow (send follow-item :value)
				:decomposable-mode
				(send decomposable-item :value)
				:separators nil :edges T
				:edge-lists
				(if (and (send x :has-slot
					       'resampling-edges)
					 (send x :slot-value
					       'resampling-edges))
				    (send x :slot-value 'resampling-edges)
				  nil)))))))

	 (resample-old-item
	  (send modal-button-proto :new
		(modal-text "Backward Elimination with Resampling")
		:action
		#'(lambda ()
		    (progn
		      (send x :show-window)
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
			      nil))))))
	
	 (stepwise-dialog-window
	  (send modal-dialog-proto :new
		(list (list (list
			     (list dismiss-stepwise)
			     exact-test-item fix-edges-item
			     clear-edges-item
			     rejected-edges-item accepted-edges-item
			     (list p-rejected-label p-rejected-item)
			     (list p-accepted-label p-accepted-item)
			     label-all-edges-item
;;;			     replacement-item
;;;			     (list fraction-label fraction-item)
;;;			     (list number-label number-item)
			     resample-item
			     )
			    (list
			     coherent-item headlong-item
			     random-item follow-item
			     decomposable-item extreme-item in-graph-item
			     recursive-item blockwise-item
;;;			     label-all-edges-item
			     graph-backward-item graph-forward-item
			     )))))
	 )

    (defmeth p-rejected-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))
    (defmeth p-accepted-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))
    (defmeth fraction-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))
    (defmeth number-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))
    )
  )
