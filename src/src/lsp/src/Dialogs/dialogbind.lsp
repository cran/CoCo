
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the drag-graph:


(defun bind-dialog (x)
  (let* ((dismiss-bind-dialog
	  (send modal-button-proto :new (modal-text "Dismiss bind window")
		:action #'(lambda () (send bind-dialog-window :remove))))

	 (name-item
	  (send modal-button-proto :new 
		(modal-text "Bind graph to variable ...")
		:action #'(lambda ()
			    (let ((name (get-value-dialog
					 "Name to give object"
					 :initial
					 ''last-graph-object)))
			      (if name (setf (symbol-value (car name)) x))))))

	 (vertices-item
	  (send modal-button-proto :new
		(modal-text "Bind vertices to *temporary*")
		:action #'(lambda ()
			    (setf *temporary* (send x :vertices)))))
	
	 (names-item
	  (send modal-button-proto :new
		(modal-text "Bind names to *temporary*")
		:action #'(lambda ()
			    (setf *temporary* (send x :names)))))
	
	 (labels-item
	  (send modal-button-proto :new
		(modal-text "Bind labels to *temporary*")
		:action #'(lambda ()
			    (setf *temporary* (send x :labels)))))

	 (positions-item
	  (send modal-button-proto :new
		(modal-text "Bind positions to *temporary*")
		:action #'(lambda ()
			    (setf *temporary* (send x :positions)))))
	
	 (label-positions-item
	  (send modal-button-proto :new
		(modal-text "Bind label-positions to *te ...")
		:action #'(lambda ()
			    (setf *temporary* (send x :label-positions)))))

	 (edges-item
	  (send modal-button-proto :new
		(modal-text "Bind edges to *temporary*")
		:action #'(lambda ()
			    (setf *temporary* (send x :edges)))))

	 (blocks-item
	  (send modal-button-proto :new
		(modal-text "Bind blocks to *temporary*")
		:action #'(lambda ()
			    (setf *temporary* (send x :blocks)))))

	 (bind-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-bind-dialog)
		 name-item vertices-item edges-item blocks-item names-item
			labels-item positions-item label-positions-item
			)))))
  )
