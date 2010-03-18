
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a help dialog window for the drag-graph:

(defun help-dialog (x)
  (let* ((dismiss-help
	  (send modal-button-proto :new (modal-text "Dismiss help window")
		:action #'(lambda () (send help-dialog :remove))))

	 (drop-edge-item
	  (send modal-button-proto :new (modal-text "Drop edge")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Click the edge with the mouse. ~%~
                     In ``Static'' mode a new window ~%~
                     with the new model will appear."))))
	
	 (add-edge-item
	  (send modal-button-proto :new (modal-text "Add edge")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a point, ~%~
                     press a mouse button and drag a line to an ~%~
                     other point, and release the mouse button. ~%~
                     In ``Static'' mode a new graph window ~%~
                     with the new model will appear."))))
	
	 (delete-item
	  (send modal-button-proto :new (modal-text "Delete variable")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a point, press the SHIFT ~%~
                     (extend modifier) button and a mouse button, ~%~
                     drag the point to the wastebasket; ~%~
                     right to and below the right lower corner of the plot, ~%~
                     and release the mouse button. ~%~
                     Add vertices by dragging lines from ``ghost-points'' ~%~
                     (invisible points at the positions of delete variables) ~%~
                     or by reading models."))))

	 (label-edge-item
	  (send modal-button-proto :new
		(modal-text "`e/d': Label an edge with P-value")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at an edge, ~%~
                     press a mouse button while pressing the ~%~
                     SHIFT (extend modifier) button, and release. ~%~
                     If the mouse is move then a point will be moved. ~%~
                     The key-event `e' on any edge will ~%~
                     label the edge with a P-value. ~%~
                     The key-event `d' will only label an edge ~%~
                     with a P-value, if the model resulting from ~%~
                     removing the edge is decomposable."))))
	
	 (drop-edge-label-position-item
	  (send modal-button-proto :new
		(modal-text "`E': Remove an edge label")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at an edge, ~%~
                     press a mouse button while pressing both the SHIFT ~%~
                     (extend modifier) and the CONTROL (option modifier) ~%~
                     buttons and release. ~%~
                     If the mouse is move then the label will be dragged."))))

	 (drag-point-item
	  (send modal-button-proto :new (modal-text "Move point")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a point, press the SHIFT ~%~
                     (extend modifier) button and a mouse button, ~%~
                     drag the point, and release the mouse button. ~%~
                     In ``Drag-graph'' mode the graph will be updated ~%~
                     and redrawn continuously when dragging the vertex. ~%~
                     In ``Edit'' mode the SHIFT (extend modifier) button ~%~
                     do not have to be pressed, and dropping of edges,~%~
                     computation of tests/label, etc. is disabled."))))
	
	 (drag-label-item
	  (send modal-button-proto :new (modal-text "Move label for point")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a point, ~%~
                     press a mouse button while pressing both the SHIFT ~%~
                     (extend modifier) and the CONTROL (option modifier) ~%~
                     buttons, drag the label and release the mouse button."))))

	 (label-point-item
	  (send modal-button-proto :new
		(modal-text "`l': Set label for variable")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a point, ~%~
                     press `l' and release."))))

	 (define-block-item
	   (send modal-button-proto :new (modal-text "Define blocks")
		 :action #'(lambda ()
			     (message-dialog
			      "Press OK and - ~%~
                     Select the item ``Block recursive models'' ~%~
                     from the graph menu."))))

	 (resize-block-item
	  (send modal-button-proto :new
		(modal-text "Resize block, drag block corner")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a corner of the block, ~%~
                     drag the corner while pressing a mouse button ~%~
                     and release."))))

	 (drag-block-item
	  (send modal-button-proto :new (modal-text "Drag block with vertices")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a corner of the block, ~%~
                     press a mouse button while pressing SHIFT ~%~
                     (extend modifier), drag the block and release."))))

	 (drag-block-label-item
	  (send modal-button-proto :new (modal-text "Drag block label")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Position the mouse at a corner of the block, ~%~
                     press a mouse button while pressing both the SHIFT ~%~
                     (extend modifier) and the CONTROL (option modifier) ~%~
                     buttons, drag the block-label and release."))))

	 (delete-block-item
	  (send modal-button-proto :new (modal-text "`W': Delete block")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Press `W' at a corner of the block to delete."))))

	 (rotate-item
	  (send modal-button-proto :new
		(modal-text "Rotate the 3-dimensional graph")
		:action #'(lambda ()
			    (message-dialog
			     "Press OK and - ~%~
                     Press the CONTROL (option modifier) button and ~%~
                     a mouse button, drag the graph and release. ~%~
                     Undo the rotation by the key-event `u'. ~%~
                     Undo the rotation by the key-event `u'. ~%~
                     After adding idling by the method :add-idle ~%~
                     idling is started by pressing the CONTROL ~%~
                     (option modifier) button and a mouse button ~%~
                     while holding the mouse fixed. ~%~
                     Idling is stopped by rotating the graph."))))
	
	 (help-dialog
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-help)
		 drop-edge-item add-edge-item delete-item
		 label-edge-item drop-edge-label-position-item
		 drag-point-item drag-label-item label-point-item
		 define-block-item resize-block-item drag-block-item
		 drag-block-label-item delete-block-item
		 rotate-item
		 )))))
  )
