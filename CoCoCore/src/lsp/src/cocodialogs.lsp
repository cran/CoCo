
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for creating a model manager.

;;; Update managers?

(defun manager-dialog (x)

  (let* ((dismiss-manager
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda () (send manager-dialog-window :remove))))

	 (end-item
	  (send modal-button-proto :new
		(if (kind-of-p x manager-proto)
		    (modal-text "End for CoCo-object of Manager")
		  (modal-text "End CoCo-object of graph"))
		:action #'(lambda ()
			    (send x :end)
			    (send manager-dialog-window :remove))))

	 (manager-item
	  (send modal-button-proto :new
		(if (kind-of-p x manager-proto)
		    (modal-text "Copy Manager for CoCo-object of graph")
		  (modal-text "Create Manager for CoCo-object of graph"))
		:action #'(lambda ()
			    (send x :return-manager
				  :location (send x :location)))))

	 (show-manager-item
	  (send modal-button-proto :new
		(if (or T (kind-of-p x manager-proto))
		    (modal-text "Show Manager for this CoCo-object"))
		:action #'(lambda ()
			    (dolist (i (send manager-proto :slot-value
					     'instances-manager) T)
				    (if (equalp 
					 (send i :slot-value 'identification)
					 (send x :slot-value 'identification))
					(send i :show-window))))))

	 (show-all-manager-item
	  (send modal-button-proto :new
		(if (or T (kind-of-p x manager-proto))
		    (modal-text "Show Manager for all CoCo-objects"))
		:action #'(lambda () (send *manager* :show-window))))

	 (show-family-item
	  (send modal-button-proto :new (modal-text "`1': Show family")
		:action #'(lambda () (send x :show-family :redraw T
					   :offsprings T :ancestors T))))

	 (show-offsprings-item
	  (send modal-button-proto :new (modal-text "`2': Show offsprings")
		:action #'(lambda () (send x :show-family :redraw T
					   :offsprings T :ancestors nil))))

	 (show-ancestors-item
	  (send modal-button-proto :new (modal-text "`3': Show ancestors")
		:action #'(lambda () (send x :show-family :redraw T
					   :offsprings nil :ancestors T))))

	 (hide-family-item
	  (send modal-button-proto :new (modal-text "`4': Hide family")
		:action #'(lambda () (send x :hide-family :redraw T
					   :offsprings T :ancestors T))))
	 
	 (hide-offsprings-item
	  (send modal-button-proto :new (modal-text "`5': Hide offsprings")
		:action #'(lambda () (send x :hide-family :redraw T
					   :offsprings T :ancestors nil))))

	 (hide-ancestors-item
	  (send modal-button-proto :new (modal-text "`6': Hide ancestors")
		:action #'(lambda () (send x :hide-family :redraw T
					   :offsprings nil :ancestors T))))

	 (close-family-item
	  (send modal-button-proto :new (modal-text "`7': Close family")
		:action #'(lambda () (send x :close-family :redraw T
					   :offsprings T :ancestors T))))

	 (close-offsprings-item
	  (send modal-button-proto :new (modal-text "`8': Close offsprings")
		:action #'(lambda () (send x :close-family :redraw T
					   :offsprings T :ancestors nil))))

	 (close-ancestors-item
	  (send modal-button-proto :new (modal-text "`9': Close ancestors")
		:action #'(lambda () (send x :close-family :redraw T
					   :offsprings nil :ancestors T))))

	 (update-managers-item
	  (send modal-button-proto :new
		(modal-text "`>': Update managers")
		:action #'(lambda () (send x :update-managers))))
	
	 (update-screen-item
	  (send modal-button-proto :new
		(modal-text "`<': Update screen (unnecessary)")
		:action #'(lambda ()
			    (dolist (i (send manager-proto :slot-value
					     'instances-manager) T)
				    (send i :update-screen)))))
	
	 (help-item
	  (send text-item-proto :new
		"`O': Open; `H': Hide; `C': Close;"))

	 (if (kind-of-p x manager-proto)
	     (progn
	       (send manager-item :enabled nil)))

	 (manager-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-manager)
		 manager-item
		 show-manager-item
		 show-all-manager-item
		 show-family-item show-offsprings-item show-ancestors-item
		 hide-family-item hide-offsprings-item hide-ancestors-item
		 close-family-item close-offsprings-item close-ancestors-item
		 update-managers-item update-screen-item help-item
		 end-item)))))
  )

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

;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for describing models:


;;; :is-in-one-clique, :is-submodel-of?


(def *model-names*
     (list "'current" "'base" "'last" "'all" "'graph" "'number:"))

(def *model-list*
     (list 'current 'base 'last 'all 'graph 'number))

(defun model-description-dialog (x)
  (let* ((dismiss-model-description
	  (send modal-button-proto :new
		(modal-text "Dismiss model dialog")
		:action #'(lambda ()
			    (send model-description-dialog-window :remove))))

	 (current-item ;;; Marker ???
	  (send modal-button-proto :new
		(modal-text "`c': Make model of graph ``Current''")
		:action #'(lambda () (send x :make-graph-current-model
					   :redraw-plots T))))
	
	 (base-item ;;; Marker ???
	  (send modal-button-proto :new
		(modal-text "`b': Make model of graph ``Base''")
		:action #'(lambda () (send x :make-graph-base-model
					   :redraw-plots T))))

	 (model-item (send choice-item-proto :new *model-names* :value 0))

	 (argument-item
	  (send edit-text-item-proto :new
		(format nil "~d"
			(send x :return-model-number 'current))
		:text-length 3))

	 (is-decomposable-item
	  (send modal-button-proto :new
		(modal-text "Is decomposable ? <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (print 
			       (if (equal 'graph model)
				   (if (send x :make-graph-current-model
					     :redraw-plots nil)
				       (send x :is-decomposable))
				 (send x :is-decomposable
				       (if (equal 'number model)
					   (send argument-item :value)
					 model))))
			      (send x :make-current current-number)))))

	 (is-graphical-item
	  (send modal-button-proto :new
		(modal-text "Is graphical ? <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (print 
			       (if (equal 'graph model)
				   (if (send x :make-graph-current-model
					     :redraw-plots nil)
				       (send x :is-graphical))
				 (send x :is-graphical
				       (if (equal 'number model)
					   (send argument-item :value)
					 model))))
			      (send x :make-current current-number)))))

	 (print-formula-item
	  (send modal-button-proto :new
		(modal-text "Print formula <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :print-formula))
				(progn
				  (send x :make-current
				        (if (equal 'number model)
					    (send argument-item :value)
					  model))
				  (send x :print-formula)))
			      (send x :make-current current-number)))))

	 (print-vertex-order-item
	  (send modal-button-proto :new
		(modal-text "Print vertex order <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :print-vertex-order))
				(progn
				  (send x :make-current
				        (if (equal 'number model)
					    (send argument-item :value)
					  model))
				  (send x :print-vertex-order)))
			      (send x :make-current current-number)))))
	
	 (print-item
	  (send modal-button-proto :new
		(modal-text "Print model <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :print-model 'current))
				(send x :print-model
				      (if (equal 'number model)
					  (send argument-item :value)
					model)))
			      (send x :make-current current-number)))))

	 (describe-item
	  (send modal-button-proto :new
		(modal-text "Describe model <argument>  ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :describe-model 'current))
				(send x :describe-model
				      (if (equal 'number model)
					  (send argument-item :value)
					model)))
			      (send x :make-current current-number)))))

	 (dispose-of-item
	  (send modal-button-proto :new
		(modal-text "Dispose of model <argument>  ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :dispose-of-model 'current))
				(send x :dispose-of-model
				      (if (equal 'number model)
					  (send argument-item :value)
					model)))
			      (send x :make-current current-number)))))


	 (model-description-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-model-description)
		 current-item base-item
		 (list
		  (list
		   print-formula-item print-vertex-order-item
		   print-item describe-item dispose-of-item
		   is-decomposable-item is-graphical-item)
		  (list model-item argument-item))
		 ))))

    (defmeth argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )
    )
  
  )

;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the editing model:


(defun model-dialog (x)
  (let* ((dismiss-model
	  (send modal-button-proto :new (modal-text "Dismiss model dialog")
		:action #'(lambda () (send model-dialog-window :remove))))
	
	 (number-label (send text-item-proto :new "Number/Order:"))

	 (number-item
	  (send edit-text-item-proto :new
		(format nil "~d" 2) :text-length 3))
	
	 (argument-label
	  (send text-item-proto :new
		"Argument, GC (or 'graph, 'current, 'base, 'last, number or set):"))

	 (argument-item
	  (let ((current-number (send x :return-model-number 'current))
		(result (send edit-text-item-proto :new (send x :return-model)
			      :text-length 66)))
	    (send x :make-current current-number)
	    result))
	
	 (create-graph-item
	  (send toggle-item-proto :new
		(modal-text "Make graph of the created model")
		:value T))

	 (copy-vertices-item
	  (send toggle-item-proto :new
		(modal-text "Share vertices, (i.e. positions)")
		:value T))

	 (copy-graph-item
	  (send modal-button-proto :new (modal-text "Copy graph")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current)))
		      (progn
			(send x :make-graph-current-model)
			(send x :return-child-coco-graph-window
			      :copy-vertices
			      (send copy-vertices-item :value)
			      :title (send argument-item :gc x)
			      :sibling T))
		      (send x :make-current current-number)))))

	 (graph-number-item
	  (send modal-button-proto :new
		(modal-text "Make graph for model number <number>")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (number (send number-item :value)))
			      (if (and number (send x :make-current number))
				  (if (send create-graph-item :value)
				      (send x :return-child-coco-graph-window
					    :copy-vertices
					    (send copy-vertices-item :value)
					    :title
					    (send x :return-model number)
					    :sibling T)))
			      (send x :make-current current-number)))))

	 (read-n-interactions-item
	  (send modal-button-proto :new
		(modal-text "<order>-order interactions on <argument>")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (number (send number-item :value))
				  (set (send argument-item :text)))
			      (if number
				  (progn
				    (send x :read-n-interactions number set)
				    (if (send create-graph-item :value)
					(send x :return-child-coco-graph-window
					      :copy-vertices
					      (send copy-vertices-item :value)
					      :title
					      (concatenate 'string
							   " Interactions: "
							   (format nil "~d" 
								   number) ";")
					      :sibling T))))
			      (send x :make-current current-number)))))

	 (read-model-item
	  (send modal-button-proto :new (modal-text "Read model <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :read-model gc)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title gc
				      :sibling T))))
		      (send x :make-current current-number)))))

	 (generate-decomposable-item
	  (send modal-button-proto :new
		(modal-text "Generate decomposable: Add Fill In")
		:action
		#'(lambda ()
		    (send x :add-fill-in
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))

	 (generate-graphical-item
	  (send modal-button-proto :new (modal-text "Generate graphical model")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    ;; (send x :make-graph-current-model)
			    (send x :generate-graphical)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title " -> Graphical;"
				      :child T))))
		      (send x :make-current current-number)))))

	 (dual-item
	  (send modal-button-proto :new (modal-text "Transform Normal to Dual")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    ;; (send x :make-graph-current-model)
			    (send x :normal-to-dual)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title " -> Dual;"
				      :sibling T))))
		      (send x :make-current current-number)))))

	 (normal-item
	  (send modal-button-proto :new (modal-text "Transform Dual to Normal")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    ;; (send x :make-graph-current-model)
			    (send x :dual-to-normal)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title " -> Normal;"
				      :sibling T))))
		      (send x :make-current current-number)))))

	 (collaps-item
	  (send modal-button-proto :new (modal-text "Collaps onto <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :make-graph-current-model)
			    (send x :collaps-model gc)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title (concatenate 'string " Collaps: "
							  gc ";")
				      :parant T))))
		      (send x :make-current current-number)))))

	 (meet-item
	  (send modal-button-proto :new (modal-text "Meet with <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (base-number 
			   (send x :return-model-number 'base))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :make-graph-base-model)
			    (send x :read-model gc)
			    (send x :meet-of-models)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title (concatenate 'string " Meet: "
							  gc ";")
				      :offset (list 50 -10)
				      :parant T))))
		      (send x :make-current current-number)
		      (send x :make-base base-number)
		      ))))

	 (join-item
	  (send modal-button-proto :new (modal-text "Join with <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (base-number 
			   (send x :return-model-number 'base))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :make-graph-base-model)
			    (send x :read-model gc)
			    (send x :join-of-models)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title (concatenate 'string " Join: "
							  gc ";")
				      :offset (list 50 -110)
				      :child T))))
		      (send x :make-current current-number)
		      (send x :make-base base-number)
		      ))))

	 (drop-edges-item
	  (send modal-button-proto :new (modal-text "Drop edges <argument>")
		:action
		#'(lambda ()
		    (send x :graph-drop-gc (send argument-item :gc x) nil
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))

	 (add-edge-item
	  (send modal-button-proto :new (modal-text "Add edges <argument>")
		:action
		#'(lambda ()
		    (send x :graph-add-gc (send argument-item :gc x)
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))
	
	 (drop-interactions-item
	  (send modal-button-proto :new
		(modal-text "Drop interactions <argument>")
		:action
		#'(lambda ()
		    (send x :graph-drop-gc (send argument-item :gc x) T
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))

	 (add-interactions-item
	  (send modal-button-proto :new
		(modal-text "Add interactions <argument>")
		:action
		#'(lambda ()
		    (send x :graph-add-gc (send argument-item :gc x)
			  :hierarchical T
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))
	
	 (split-item
	  (send modal-button-proto :new
		(modal-text "Split <argument> ")
		:action
		#'(lambda ()
		    (let ((argument (string-to-block-list
				     (send argument-item :gc x))))
		    (send x :split (car (car argument))
			  :ignore-name-list (cadr (car argument)))))))

	 (split-block-item
	  (send modal-button-proto :new
		(modal-text "Split block recursive model <argument> ")
		:action
		#'(lambda ()
		    (send x :split-in-block-recursive
			  (send argument-item :gc x)))))
	 

	 (model-dialog-window
	  (send modal-dialog-proto :new
		(if (kind-of-p x manager-proto)
		    (list
		     (list dismiss-model)
		     argument-label argument-item
		     create-graph-item ;; copy-vertices-item
		     (list
		      (list read-model-item read-n-interactions-item
			    graph-number-item
			    generate-decomposable-item generate-graphical-item
			    dual-item normal-item
			    ;; split-item
			    )
		      (list  collaps-item meet-item join-item
			     drop-edges-item add-edge-item
			     drop-interactions-item add-interactions-item
			     ;; split-block-item
			     ))
		     )
		  (list
		   (list dismiss-model)
		   argument-label argument-item
		   create-graph-item copy-vertices-item 
		   (list number-label number-item)
		   (list
		    (list read-model-item
			  graph-number-item
			  generate-decomposable-item generate-graphical-item
			  dual-item normal-item
			  read-n-interactions-item
			  ;; split-item
			  )
		    (list  meet-item join-item
			   drop-edges-item add-edge-item
			   drop-interactions-item add-interactions-item
			   collaps-item
			   ;; split-block-item
			   ))
		   )))))

    (defmeth argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (defmeth number-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (defmeth argument-item :gc (graph)
      (let ((number (send self :value)))
	(if number
	    (send graph :return-model number)
	  (let ((gc (send self :text)))
	    (cond
	     ((equalp gc "'graph")
	      (send graph :return-model))
	     ((equalp gc "'current")
	      (send graph :return-model 'current))
	     ((equalp gc "'base")
	      (send graph :return-model 'base))
	     ((equalp gc "'last")
	      (send graph :return-model 'last))
	     (t gc))))))
    )
  )

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

;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for describing values:


;;; :plot and :return-matrix ???

(defmeth coco-proto :new-return-vector (arguments set)
  (send self :return-vector (car arguments) set
	:model (cadr arguments)
	:complete (car (caddr arguments))
	:random (cadr (caddr arguments))
	:log-transformed (caddr (caddr arguments))
	:permuted (cadddr (caddr arguments)))
  )

(defun encode-return-vector (arguments set)
  `(send *this-graph* :return-vector (quote ,(car arguments)) ,set
	 :model ,(if (numberp (cadr arguments))
		     (cadr arguments)
		   (backquote (quote ,(cadr arguments))))
	 :complete ,(car (caddr arguments))
	 :random ,(cadr (caddr arguments))
	 :log-transformed ,(caddr (caddr arguments))
	 :permuted ,(cadddr (caddr arguments)))
  )

(defun encode-return-vector (arguments set)
  (eval
   `#'(lambda (graph)
	(send graph :return-vector (quote ,(car arguments)) ,set
	      :model ,(if (numberp (cadr arguments))
			  (cadr arguments)
			(if (not (cadr arguments))
			    nil
			  (backquote (quote ,(cadr arguments)))))
	      :complete ,(car (caddr arguments))
	      :random ,(cadr (caddr arguments))
	      :log-transformed ,(caddr (caddr arguments))
	      :permuted ,(cadddr (caddr arguments)))))
  )

(defun encode-argument (arguments)
  (concatenate 'string (string (car arguments))
	       (format nil ": ~a" (cadr arguments))
	       (if (car (caddr arguments)) ", Complete")
	       (if (cadr (caddr arguments)) ", Random")
	       (if (caddr (caddr arguments)) ", Log-transformed"))
  )

(defun values-dialog (x)
  (let* ((dismiss-values
	  (send modal-button-proto :new (modal-text "Dismiss values window")
		:action #'(lambda () (send values-dialog-window :remove))))

	 (argument-label
	  (send text-item-proto :new
		"Vertex-set:"))

	 (argument-set-item
	  (send edit-text-item-proto :new 
		(to-string (send x :return-name-list) "")
		:text-length 10
		))

	 (all-values-item
	  (send toggle-item-proto :new
		(modal-text "Present all possible values")
		:value nil))

	 (list-item
	  (send modal-button-proto :new
		(modal-text "List values <set> ")
		:action #'(lambda () (send x :list-values
					   (send argument-set-item :text)))))

	 (case-list-item
	  (send modal-button-proto :new
		(modal-text "Case list <set> ")
		:action #'(lambda () (send x :case-list
					   (send argument-set-item :text)))))

	 (print-table-item
	  (send modal-button-proto :new
		(modal-text "Print table <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value))))
		      (if arguments
			  (send x :print-table
				(car arguments)
				(send argument-set-item :text)
				:model (cadr arguments)
				:complete (car (caddr arguments))
				:random (cadr (caddr arguments))
				:log-transformed (caddr (caddr arguments))
				:permuted (cadddr (caddr arguments))
				))))))


	 (describe-table-item
	  (send modal-button-proto :new
		(modal-text "Describe table <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value))))
		      (if arguments
			  (send x :describe-table
				(car arguments)
				(send argument-set-item :text)
				:model (cadr arguments)
				:complete (car (caddr arguments))
				:random (cadr (caddr arguments))
				:log-transformed (caddr (caddr arguments))
				:permuted (cadddr (caddr arguments))
				))))))

	 (return-vector-item
	  (send modal-button-proto :new
		(modal-text "Bind and Return-vector <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value))))
		      (if arguments
			  (setf *vector*
				(send x :new-return-vector arguments
				      (send argument-set-item :text))))))))

	 (histogram-item
	  (send modal-button-proto :new
		(modal-text "Histogram <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if arguments
			  (setf *histogram*
				(histogram
				 (send x :new-return-vector arguments factors)
				 :title
				 (format nil "~a: ~a" factors (cadr arguments))
				 :variable-labels
				 (list (encode-argument arguments)))))))))


	 (plot-item
	  (send modal-button-proto :new
		(modal-text "X-Y-plot of two cell-values ... ... ")
		:action
		#'(lambda ()
		    (let ((X-args (one-value-dialog
				   (send x :slot-value 'model-number) "X"
				   (send all-values-item :value)))
			  (Y-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if (and X-args Y-args)
			  (setf *plot*
				(plot-points
				 (send x :new-return-vector X-args factors)
				 (send x :new-return-vector Y-args factors)
				 :title
				 (format nil "~a: ~a, ~a"
					 factors (cadr X-args) (cadr Y-args))
				 :variable-labels
				 (list (encode-argument X-args)
				       (encode-argument Y-args)))))))))
	
	 (spin-item
	  (send modal-button-proto :new
		(modal-text "Spin-plot of three cell-values .. .. .. ")
		:action
		#'(lambda ()
		    (let ((X-args (one-value-dialog
				   (send x :slot-value 'model-number) "X"
				   (send all-values-item :value)))
			  (Y-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (Z-args (one-value-dialog
				   (send x :slot-value 'model-number) "Z"
				   (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if (and X-args Y-args Z-args)
			  (setf *spin-plot*
				(spin-plot
				 (list
				  (send x :new-return-vector X-args factors)
				  (send x :new-return-vector Y-args factors)
				  (send x :new-return-vector Z-args factors))
				 :title
				 (format nil "~a: ~a, ~a, ~a"
					 factors (cadr X-args)
					 (cadr Y-args) (cadr Z-args))
				 :variable-labels
				 (list (encode-argument X-args)
				       (encode-argument Y-args)
				       (encode-argument Z-args)))))))))

	 (dynamic-spin-item
	  (send modal-button-proto :new
		(modal-text "Dynamic Spin-plot ... ... ...")
		:action
		#'(lambda ()
		    (let ((X-args (one-value-dialog
				   (send x :slot-value 'model-number) "X"
				   (send all-values-item :value)))
			  (Y-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (Z-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if (and X-args Y-args Z-args)
			  (setf *dynamic-spin-plot*
				(send x :return-dynamic-coco-spin-plot
				      (list
				       (encode-return-vector X-args factors)
				       (encode-return-vector Y-args factors)
				       (encode-return-vector Z-args factors))
				      :title
				      (format nil "~a: ~a, ~a, ~a"
					      factors (cadr X-args)
					      (cadr Y-args) (cadr Z-args))
				      :variable-labels
				      (list (encode-argument X-args)
					    (encode-argument Y-args)
					    (encode-argument Z-args)))))))))

	 (old-dynamic-spin-item
	  (send modal-button-proto :new
		(modal-text "Dynamic Spin-plot .?. .?. .?.")
		:action #'(lambda ()
			    (setf *this-graph* x)
			    (let ((a (get-value-dialog
				      "X-value" :initial
				      ''(send *this-graph*
					      :return-vector 'adjusted "*"
					      :model 'current)))
				  (b (get-value-dialog
				      "Y-value" :initial
				      ''(send *this-graph*
					      :return-vector 'adjusted "*"
					      :model 'base)))
				  (c (get-value-dialog
				      "Z-value" :initial
				      ''(send *this-graph*
					      :return-vector 'observed "*"
					      :model nil))))
			      (setf *dynamic-spin-plot*
				    (send x :return-dynamic-coco-spin-plot
					  (list (car a) (car b) (car c))))))))
	
	 (values-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-values)
		      all-values-item
		      (list (list argument-label) (list argument-set-item))
		      list-item case-list-item
		      print-table-item describe-table-item return-vector-item
		      histogram-item plot-item spin-item dynamic-spin-item)))))
  )

;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the tests:


;;; :print-common-decompositions, :decompose-models?


(defun test-dialog (x)
  (let* ((dismiss-test
	  (send modal-button-proto :new (modal-text "Dismiss test window")
		:action #'(lambda () (send test-dialog-window :remove))))

	 (test-item
	  (send modal-button-proto :new 
		(modal-text "`t': Test graph against `Base'")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (send x :test))
			      (send x :make-current current-number)))))

	 (argument-item
	  (send edit-text-item-proto :new 
		(to-string (send x :return-name-list) "")
	        ;;; :text-length 66
		))
	
	 (edge-item
	  (send toggle-item-proto :new "Edges (else interactions)" :value T))

	 (factorize-item
	  (send modal-button-proto :new 
		"Factorize test: "
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (send x :factorize
					(if (send edge-item :value)
					    'edges 'interactions) set))
			      (send x :make-current current-number)))))

	 (find-likelihood-item
	  (send modal-button-proto :new 
		(modal-text "Find log(Likelihood)")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :find-log-l))
				)
			      (send x :make-current current-number)))))

	 (find-deviance-item
	  (send modal-button-proto :new 
		(modal-text "Find deviance")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :find-deviance))
				)
			      (send x :make-current current-number)))))

	 (bind-deviance-item
	  (send modal-button-proto :new 
		(modal-text "Bind values of find deviance to *test*")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :compute-deviance))
				)
			      (send x :make-current current-number)))))

	 (bind-test-item
	  (send modal-button-proto :new 
		(modal-text "Bind values of compute test to *test*")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :compute-test))
				)
			      (send x :make-current current-number)))))

	 (bind-test-object-item
	  (send modal-button-proto :new 
		(modal-text "Bind values of test to *test* object")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :return-test-object))
				)
			      (send x :make-current current-number)))))

	 (show-tests-item
	  (send modal-button-proto :new 
		(modal-text "Show computed tests")
		:action #'(lambda () (send x :show-tests))))

	 (dispose-of-tests-item
	  (send modal-button-proto :new 
		(modal-text "Dispose of computed tests")
		:action #'(lambda () (send x :dispose-of-tests))))

	 (argument-slice-item
	  (send edit-text-item-proto :new 
		(to-string (send x :return-name-list) "")
	        ;;; :text-length 66
		))

	 (slice-item
	  (send modal-button-proto :new 
		"Meassures of associations: "
		:action #'(lambda ()
			    (send x :slice (send argument-slice-item :text)))))

	 (test-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-test)
		 test-item
		 find-likelihood-item find-deviance-item
		 bind-test-object-item bind-test-item bind-deviance-item
		 ;;; argument-label
		 (list factorize-item (list edge-item argument-item))
		 show-tests-item dispose-of-tests-item
		 (list slice-item argument-slice-item)
		 )))))
  )

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

;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the EH-procedure:

;;; Base-model and fixing?
;;; fit/accept/reject more models ?
;;; Force add duals ???

(defun eh-dialog (x)
  (let* ((dismiss-eh
	  (send modal-button-proto :new (modal-text "Dismiss eh search window")
		:action #'(lambda () (send eh-dialog-window :remove))))


;;;	 (significance-label
;;;	  (send text-item-proto :new "Significance level:  "))

	 (significance-label
	  (send modal-button-proto :new
		(modal-text "Significance level: ")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    )))

	 (significance-item
	  (send edit-text-item-proto :new "0.05" :text-length 6))
	
	 (class-label (send text-item-proto :new "<Class>:         "))

	 (*class-list*
	  (list 'all 'duals 'a-dual 'r-dual 'classes 'accepted 'rejected))

	 (*class-names*
	  (mapcar #'string *class-list*))
	 
	 (class-item
	  (send choice-item-proto :new
		(mapcar #'string *class-list*) :value 0))

	 (dual-label (send text-item-proto :new "<Dual>:           "))

	 (*dual-names*
	  (list "'a-dual" "'r-dual"
		"'smallest-dual" "'largest-dual" "'both-duals"))

	 (*dual-list*
	  (list 'a-dual 'r-dual 'smallest-dual 'largest-dual 'both-duals))

	 (dual-item
	  (send choice-item-proto :new
		(mapcar #'string *dual-list*) :value 0))

	 (sub-class-label (send text-item-proto :new "<Sub-Class>: "))

	 (*sub-class-names*
	  (list "'decomposable" "'graphical" "'hierarchical"))

	 (*sub-class-list*
	  (list 'decomposable 'graphical 'hierarchical))

	 (sub-class-item
	  (send choice-item-proto :new
		(mapcar #'string *sub-class-list*) :value 1))

	 (strategy-label (send text-item-proto :new "<Strategy>:   "))

	 (*strategy-names*
	  (list "'smallest" "'alternating" "'rough"))

	 (*strategy-list*
	  (list 'smallest 'alternating 'rough))

	 (strategy-item
	  (send choice-item-proto :new
		(mapcar #'string *strategy-list*) :value 0))
	
	 (model-label (send text-item-proto :new "<Model>:         "))

	 (*model-names*
	  (list "'current" "'base" "'last" "'graph" "'all" "'number:"))

	 (*model-list*
	  (list 'current 'base 'last 'graph 'all 'number))

	 (model-item (send choice-item-proto :new *model-names* :value 3))

	 (argument-item
	  (send edit-text-item-proto :new
		(format nil "~d" (send x :slot-value 'model-number))
		:text-length 3))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status")
		:action #'(lambda () (send x :status 'eh))))

	 (find-item
	  (send modal-button-proto :new
		(modal-text "Find dual <dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :find-dual
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value) *sub-class-list*))
		    (send x :status 'eh))))

	 (dispose-item
	  (send modal-button-proto :new
		(modal-text "Dispose of eh <class>")
		:action
		#'(lambda ()
		    (send x :dispose-of-eh
			  (nth (send class-item :value) *class-list*))
		    (send x :status 'eh))))

	 (fit-item
	  (send modal-button-proto :new
		(modal-text "Fit <dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    (send x :fit
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value) *sub-class-list*))
		    (send x :status 'eh))))

	 (accept-item
	  (send modal-button-proto :new
		(modal-text "% Accept <a-/r-dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :accept
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value)*sub-class-list*))
		    (send x :status 'eh))))

	 (reject-item
	  (send modal-button-proto :new
		(modal-text "% Reject <a-/r-dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :reject
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value) *sub-class-list*))
		    (send x :status 'eh))))

	 (fit-model-item
	  (send modal-button-proto :new
		(modal-text "Fit <Model>")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    (send x :fit (send model-item :model
				       (send x :slot-value 'model-number)))
		    (send x :status 'eh))))

	 (accept-model-item
	  (send modal-button-proto :new
		(modal-text "Accept <Model>")
		:action #'(lambda ()
			    (send x :accept
				  (send model-item :model
					(send x :slot-value 'model-number)))
			    (send x :status 'eh))))

	 (reject-model-item
	  (send modal-button-proto :new
		(modal-text "Reject <Model>")
		:action #'(lambda ()
			    (send x :reject
				  (send model-item :model
					(send x :slot-value 'model-number)))
			    (send x :status 'eh))))

	 (search-item
	  (send modal-button-proto :new
		(modal-text "Recursive search <strategy> <sub-class>")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    (send x :eh
			  :strategy  (nth (send strategy-item :value)
					  *strategy-list*)
			  :sub-class (nth (send sub-class-item :value)
					  *sub-class-list*))
;;;		    (send x :status 'eh)
		    )))

	 (plot-search-result-item
	  (send modal-button-proto :new (modal-text "Plot EH result <class>")
		:action
		#'(lambda ()
		    (let ((current-number
			   (send x :return-model-number 'current)))
		      (send x :plot-EH-search-result
			    (nth (send class-item :value) *class-list*))
		      (send x :make-current current-number)))))

	 (eh-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-eh)
		 (list significance-label significance-item)
		 (list (list sub-class-label sub-class-item)
		       (list strategy-label strategy-item)
		       (list dual-label dual-item)
		       (list class-label class-item)
		       (list model-label model-item argument-item)
		       )
		 (list find-item dispose-item)
		 (list
		  (list fit-item accept-item reject-item)
		  (list fit-model-item accept-model-item reject-model-item))
		 (list search-item plot-search-result-item)
		 status-item
		 ))))

    (defmeth significance-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))

    (defmeth argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (defmeth model-item :model (&optional (model-number -1))
      (let ((model (nth (send model-item :value) *model-list*)))
	(if (equal 'number model)
	    (send argument-item :value)
	  (if (equal 'graph model) model-number model)))
      )
    )
  )

;;

(provide "cocodialogs")
