
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the drag-graph:


(defun graph-dialog (x)
  (let* ((dismiss-graph
	  (send modal-button-proto :new (modal-text "Dismiss graph window")
		:action #'(lambda () (send graph-dialog-window :remove))))

	 (space-item (send text-item-proto :new ""))

	 (retitle-item
	  (send modal-button-proto :new
		(modal-text "Set new title graph ...")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current)))
			      (send x :title (get-string-dialog
					      "New title:"
					      :initial (send x :return-model)))
			      (send x :make-current current-number)))))

	 (bind-item
	  (send modal-button-proto :new 
		(modal-text "Bind graph (or parts of) to variable ...")
		:action #'(lambda () (bind-dialog x))))
	 #|
	 (name-item
	  (send modal-button-proto :new 
		(modal-text "Bind graph to variable ...")
		:action #'(lambda ()
			    (let ((name (get-value-dialog
					 "Name to give object"
					 :initial
					 ''last-graph-object)))
			      (if name (setf (symbol-value (car name)) x))))))
	 |#
	 (save-item
	  (send modal-button-proto :new
		(modal-text "Save image in PostScript ...")
		:action #'(lambda ()
			    (let ((file (get-string-dialog 
					 "Name of file:"
					 :initial "image.ps")))
			      (if file (send x :save-image file))))))
	
	 (tex-item
	  (send modal-button-proto :new 
		(modal-text "Save image in TeX ...")
		:action #'(lambda ()
			    (let ((file (get-string-dialog
					 "Enter a file name for TeX output:"
					 :initial  "image.tex")))
			      (if (= (length file) 0)
				  (send x :dump-tex)
				(send x :dump-tex :file-name file))))))

	 (undo-item
	  (send modal-button-proto :new (modal-text "`u': Undo edit of layout")
		:action #'(lambda () (send x :undo-move))))
	
	 (redo-item
	  (send modal-button-proto :new (modal-text "`z': Redo edit of layout")
		:action #'(lambda () (send x :redo-move))))
	
	 (skip-undo-item
	  (send modal-button-proto :new (modal-text "`U': Skip Undo item")
		:action #'(lambda () (send x :skip-undo-move))))
	
	 (skip-redo-item
	  (send modal-button-proto :new (modal-text "`Z': Skip Redo item")
		:action #'(lambda () (send x :skip-redo-move))))
	
	 (set-position-item
	  (send modal-button-proto :new (modal-text "`V': Open vertex")
		:action #'(lambda () nil)))
	
	 (round-item
	  (send modal-button-proto :new
		(modal-text "`y': Adjust vertices to grid")
		:action #'(lambda ()
			    (send x :adjust-vertices-to-grid :delta 5 :redraw T)
			    (send x :adjust-blocks-to-grid :delta 5 :redraw T)
			    )))
	 #|
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
          |#

	 (drag-graph-item
	 (setf drag-graph-item
	  (send linked-toggle-item-proto :new
		(modal-text "Drag graph mode: Draw graph continuously")
		x 'drag-graph
		:value (send x :drag-graph)
		:action #'(lambda ()
			    (send x :drag-graph (not (send x :drag-graph)))
			    (send linked-toggle-item-proto :update
				  x 'drag-graph (send x :drag-graph))))))
	
	 (edit-graph-item
	  (send linked-toggle-item-proto :new
		(modal-text "Edit graph mode: Disable edges and tests")
		x 'edit-graph
		:value (send x :edit-graph)
		:action #'(lambda ()
			    (send x :edit-graph  (not (send x :edit-graph)))
			    (send linked-toggle-item-proto :update
				  x 'edit-graph (send x :edit-graph)))))
	
	 (static-item
	  (send linked-toggle-item-proto :new
		(modal-text "`s': Static: Create new windows")
		x 'static
		:value (send x :static)
		:action #'(lambda ()
			    (send x :static (not (send x :static)))
			    (send linked-toggle-item-proto :update
				  x 'static (send x :static)))))
	
	 (grid-item
	  (send linked-toggle-item-proto :new (modal-text "`g': Draw grid")
		x 'grid
		:value (send x :grid)
		:action #'(lambda ()
			    (send x :grid (not (send x :grid)))
			    (send linked-toggle-item-proto :update
				  x 'grid (send x :grid))
;;;			    (send x :redraw-graph)
			    )))
	
	 (redraw-item
	  (send modal-button-proto :new (modal-text "`r': Redraw window")
		:action #'(lambda () (send x :redraw-graph))))
	
	 (idle-item
	  (send modal-button-proto :new (modal-text "Add methods for idling")
		:action #'(lambda ()
			    (send x :add-idle))))

	 (controls-item
	  (send modal-button-proto :new (modal-text "Add controls")
		:action #'(lambda ()
			    (send x :add-controls))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object ... ")
		:action #'(lambda ()
			    (setf *this-graph* x)
			    (get-value-dialog
			     "Evaluate this expression" :initial
			     '(send *this-graph* :status 'all)))))

	 (graph-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-graph)
		 retitle-item bind-item save-item tex-item
		 undo-item redo-item skip-undo-item skip-redo-item
		 round-item ;;; set-position-item
		 redraw-item idle-item controls-item status-item
		 drag-graph-item edit-graph-item static-item grid-item
		 #|
		 (list
		  (list retitle-item name-item save-item tex-item
			undo-item redo-item skip-undo-item skip-redo-item
			round-item ;;; set-position-item
			redraw-item manager-item idle-item controls-item)
		  (list status-item
			vertices-item edges-item blocks-item names-item
			labels-item positions-item label-positions-item
			drag-graph-item edit-graph-item
			static-item grid-item
			))
                 |#
		 )))))
  )
