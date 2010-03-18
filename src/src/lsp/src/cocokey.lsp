
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This is a version of the do-key method for a coco-graph-window



(defmeth coco-graph-window-proto :do-key (c m1 m2)
  (let ((result
	 (case c
	       (#\T (trace send))
	       (#\N (untrace send))
	       (#\i (send self :idle-on (not (send self :idle-on))))
	       (#\v (send self :visual-blocks
			  (not (send self :visual-blocks))))
	       (#\s (send self :static (not (send self :static))))
	       (#\S (send self :static T))
	       (#\f (send self :return-child-coco-factor-graph-window 
			  ;; :model (send self :return-model-number)
			  ))
	       (#\D (not (send self :static nil)))
	       (#\L (send self :label-all-edges :decomposable-mode nil
			  :follow T :make-graph nil))
	       (#\l (let ((vertex
			   (send self :return-closest-vertex-in-canvas
				 (send self :x) (send self :y))))
		      (if vertex
			  (send vertex :label
				(get-string-dialog
				 "Lable:"
				 :initial (concatenate
					   'string
					   (string (send vertex :name)) ": "
					   (if (send vertex :label)
					       (send vertex :label)
					     "                    ")))
				:redraw self))))
	       (#\- (let ((edge (send self :return-closest-edge-in-canvas
				      (send self :x) (send self :y))))
		      (if edge (send edge :label-arrow
				     (not (send edge :label-arrow))))))
	       (#\= (let ((edge (send self :return-closest-edge-in-canvas
				      (send self :x) (send self :y))))
		      (if edge (send edge :fix-label-position
				     (not (send edge :fix-label-position))))))
	       (#\_ (let ((vertex
			   (send self :return-closest-vertex-in-canvas
				 (send self :x) (send self :y))))
		      (if vertex
			  (send self :vertex-label-arrow vertex
				(not (send self :vertex-label-arrow
					   vertex))))))
	       (#\+ (let ((vertex
			   (send self :return-closest-vertex-in-canvas
				 (send self :x) (send self :y))))
		      (if vertex
			  (send self
				:fix-vertex-label-position vertex
				(not (send self
					   :fix-vertex-label-position
					   vertex))))))
	       (#\d (let ((edge (send self :return-closest-edge-in-canvas
				      (send self :x) (send self :y))))
		      (if edge (send self :set-edge-label edge
				     :follow T :decomposable-mode T))))
	       (#\e (let ((edge (send self :return-closest-edge-in-canvas
				      (send self :x) (send self :y))))
		      (if edge (send self :set-edge-label edge
				     :follow T :decomposable-mode nil))))
	       (#\E (let ((edge (send self :return-closest-edge-in-canvas
				      (send self :x) (send self :y))))
		      (if edge (send self :drop-edge-label edge))))
	       (#\W (let ((block-number
			   (send self :return-closest-block-point-in-canvas
				 (send self :x) (send self :y))))
                     (if (car block-number)
                      (send self :delete-block
                       (send (car (car block-number)) :stratum)
                       ))))
	       (#\c (send self :make-graph-current-model :redraw-plots T
			  :key-event T))
	       (#\b (send self :make-graph-base-model :redraw-plots T
			  :key-event T))
	       (#\u (send self :undo-move))
	       (#\U (send self :skip-undo-move))
	       (#\z (send self :redo-move))
	       (#\Z (send self :skip-redo-move))
	       (#\y (progn 
		      (send self :adjust-vertices-to-grid :delta 5 :redraw T)
		      (send self :adjust-blocks-to-grid :delta 5 :redraw T)))
	       (#\p (let ((current-number
			   (send self :return-model-number 'current)))
		      (if (send self :make-graph-current-model
				:redraw-plots nil)
			  (send self :print-model 'current))
		      (send self :make-current current-number)))
	       (#\t (let ((current-number
			   (send self :return-model-number 'current)))
		      (if (send self :make-graph-current-model
				:redraw-plots nil)
			  (send self :test))
		      (send self :make-current current-number)))
	       (#\a (send self :print-model 'all))
	       (#\g (send self :grid (not (send self :grid))))
	       (#\r (send self :redraw-graph))
	       (#\< (send self :update-screen))
	       (#\> (send self :update-managers))
	       (#\O (if (kind-of-p self manager-proto)
			(let ((vertex
			       (send self :return-closest-vertex-in-canvas
				     (send self :x) (send self :y))))
			  (if vertex
			      (send self :show-vertex vertex :redraw T)))
		      (send self :show-window)))
	       (#\H (if (kind-of-p self manager-proto)
			(let ((vertex
			       (send self :return-closest-vertex-in-canvas
				     (send self :x) (send self :y))))
			  (if vertex
			      (send self :hide-vertex vertex :redraw T)))
		      (send self :hide-window)))
	       (#\C (if (kind-of-p self manager-proto)
			(let ((vertex
			       (send self :return-closest-vertex-in-canvas
				     (send self :x) (send self :y))))
			  (if vertex
			      (send self :close-vertex vertex :redraw T)))
		      (send self :close)))
	       (#\1 (send self :show-family :redraw T
			  :offsprings T :ancestors T :siblings T))
	       (#\2 (send self :show-family :redraw T
			  :offsprings T :ancestors nil))
	       (#\3 (send self :show-family :redraw T
			  :offsprings nil :ancestors T))
	       (#\4 (send self :hide-family :redraw T
			  :offsprings T :ancestors T :siblings T))
	       (#\5 (send self :hide-family :redraw T
			  :offsprings T :ancestors nil))
	       (#\6 (send self :hide-family :redraw T
			  :offsprings nil :ancestors T))
	       (#\7 (send self :close-family :redraw T
			  :offsprings T :ancestors T :siblings T))
	       (#\8 (send self :close-family :redraw T
			  :offsprings T :ancestors nil))
	       (#\9 (send self :close-family :redraw T
			  :offsprings nil :ancestors T))
	       (#\Q (let ((vertex
			   (send self :return-closest-vertex-in-canvas
				 (send self :x) (send self :y))))
		      (if vertex
			  (send self
				:split-in-block-recursive vertex))))
	       (t nil)))
	(text (case c
		    (#\i "Idle-On: ")
		    (#\v "Visual blocks: ")
		    (#\s "Static: ")
		    (#\f "Factor graph: ")
		    (#\S "Static: ")
		    (#\D "Dynamic: ")
		    (#\L "Label all edges\n")
		    (#\l "Vertex-label\n")
		    (#\- "Edge label arrow\n")
		    (#\= "Fix Edge label\n")
		    (#\_ "Vertex label arrow\n")
		    (#\+ "Fix Vertex label\n")
		    (#\d "P-value at (decomposable) edge\n")
		    (#\e "P-value at edge\n")
		    (#\E "Remove P-value at edge\n")
		    (#\W "Delete block\n")
		    (#\c "Current\n")
		    (#\b "Base\n")
		    (#\u "Undo\n")
		    (#\U "Skip Undo\n")
		    (#\z "Redo\n")
		    (#\Z "Skip Redo\n")
		    (#\y "Smooth graph\n")
		    (#\p "Print graph\n")
		    (#\t "Test graph\n")
		    (#\a "Print all models\n")
		    (#\g "Grid\n")
		    (#\r "Redraw\n")
		    (#\< "Update screen\n")
		    (#\> "Update manager\n")
		    (#\O "Show graph\n")
		    (#\H "Hide graph\n")
		    (#\C "Close graph\n")
		    (#\1 "Show family\n")
		    (#\2 "Show offsprings\n")
		    (#\3 "Show ancestors\n")
		    (#\4 "Hide family\n")
		    (#\5 "Hide offsprings\n")
		    (#\6 "Hide ancestors\n")
		    (#\7 "Close family\n")
		    (#\8 "Close offsprings\n")
		    (#\9 "Close ancestors\n")
		    (#\Q "Split\n")
		    (t   "Invalid key\n"))))
    (format t "~a ~a ~%" text result))
  )


(provide "cocokey")
