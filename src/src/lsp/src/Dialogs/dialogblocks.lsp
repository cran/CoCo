
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for defining a causal structure.

(def *my-trace* 0)

  (defmeth drag-graph-proto :my-trace (&optional (val nil set))
    (if set (if val (setf *my-trace* 1) (setf *my-trace* 0)))
    (if (>= *my-trace* 1) T)
    )

(defun blocks-dialog (x)

  (let* ((dismiss-blocks
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda () (send blocks-dialog-window :remove))))

	 (visual-item
	  (send toggle-item-proto :new (modal-text "Define with visual blocks")
		:value T))
	
	 (define-blocks-item
	   (send modal-button-proto :new
		 (modal-text "Define causal structure ...")
		 :action
		 #'(lambda ()
		     (let ((blocks
			    ;; Better (?): Let the string stay in the 
			    ;; dialog-window:
			    (get-string-dialog
			     "Blocks: " :initial
			     (to-string (reverse
					 (send x :return-name-list)) "<"))))
		       (if blocks (send x :define-blocks blocks nil nil
					(send visual-item :value)))))))

	 (add-block-item
	  (send modal-button-proto :new (modal-text "Add new block ...")
		:action
		#'(lambda ()
		    (let ((block
			   (car (get-value-dialog
				 "Block key: " :initial
				 (if (send x :blocks)
				     (1+ (length (send x :blocks))) 1)))))
		      (if block (send x :add-block block))))))

	 (visual-blocks-item
	  (send linked-toggle-item-proto :new
		(modal-text "`v': Toggle visibility of blocks")
		x 'visual-blocks
		:value (send x :visual-blocks)
		:action #'(lambda ()
			    (send x :visual-blocks
				  (not (send x :visual-blocks)))
			    (send linked-toggle-item-proto :update
				  x 'visual-blocks (send x :visual-blocks)))))

	 (my-trace-item
	  (send linked-toggle-item-proto :new
		(modal-text "Toggle Show Graph for Test")
		x 'my-trace
		:value (send x :my-trace)
		:action #'(lambda ()
			    (send x :my-trace
				  (not (send x :my-trace)))
			    (send linked-toggle-item-proto :update
				  x 'my-trace (send x :my-trace)))))

	 (blocks-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-blocks)
		      visual-item define-blocks-item
		      add-block-item visual-blocks-item my-trace-item)))))

  )
