
;;; cocometh.lsp

(defmeth coco-proto :drop-edges (gc &optional (just nil))
  (if just (send self :just))
  (coco-enter-string 183 gc nil :coco-id (slot-value 'identification)))

;;; cocotest.lsp

(defmeth coco-graph-window-proto :test-remove-edge-from-current
  (string edge &key (decomposable-mode nil))
  (if (send self :is-graphical 'current)
      (send self :drop-edges string T)
    (send self :drop-interactions string T))
  (send self :current)
;;;  (send self :print-model 'all)
  (let ((result
	 (if (send self :is-submodel-of)
	     (if (or (not decomposable-mode)
		     (send self :is-decomposable 'current))
		 (let ((test (send self :return-test-object nil nil)))
		   (if (and test (< 0 (send test :df)))
		       (progn
			 (format t "Testing edge: ~4a~%" string)
			 (send edge :width
			       (send self :p-to-width
				     (send self :select-p-value
					   test :print-test T)))
			 (send edge :dashed nil)
			 test)
		     'no-degrees-of-freedom))
	       'non-decomposable)
	   'not-submodel-of-base)))
    (send self :dispose-of-model 'current)
    result)
  )

;;; blocks.lsp

(defun inner (str) (concatenate 'string
                    (but-last (cdr (map-elements #'char
                                    str (iseq (length str)))))))

(defmeth drag-graph-proto :define-blocks
  (block-list &optional (labels nil) (nanny nil) (draw-blocks T))
  (let ((new self)
        (current-number (send self :return-model-number 'current)))
    (let ((causal-model (concatenate 'string
                                     (inner (send self :return-model 'current 'gc))
                                     "|" block-list)))
      (send self :make-graph-current-model :redraw-plots nil)
      (if T
          (progn
            (if (send self :static)
                (if (or T draw-blocks)
                    (progn
                      (setf new
                            (send new :make-graph
                                  :model causal-model
                                  :location (+ (list 20 0)
                                               (send self :location))
                                  :title (concatenate 'string " + Blocks")))
;;;		    (send self :add-manager-edge self new :type 'sibling)
                     )
                 (setf new
                  (send new :return-child-coco-graph-window
                   :model causal-model
                   :copy-slots nil
                   :offset (list 20 0)
                   :title (concatenate 'string " + Blocks")
                   :sibling T)))
             (progn
              (send self :remove-tests)
              (send self :title (concatenate
                                 'string (send self :title) " + Blocks"))))
           (send new :set-blocks block-list labels nanny draw-blocks))
       (format t "Blocks already added. ~%")))
   (send self :make-current current-number)
   (send self :redraw-graph)
   new)
 )

;;; (trace :update-arrows)
;;; (trace :stratas)
;;; (trace :update-model)
;;; (trace :cs-string-from-blocks)

(defmeth drag-graph-proto :stratas (&optional (val nil))
  (if val
      (mapcar #'(lambda (i j) (send i :stratum j))
	      (send self :vertices) val)
    (mapcar #'(lambda (i) (send i :stratum)) (send self :vertices)))
  )

(defmeth drag-graph-proto :cs-string-from-blocks (&optional &key (redraw nil))
  (let ((cs-string "")
        (names (send self :names))
        (stratas (send self :stratas)))
    (mapcar #'(lambda (stratum)
               (if (< 0 (length (select names (which (= stratum stratas)))))
                (setf cs-string
                 (concatenate 'string cs-string
                  (concatenate 'string "["
                   (to-string (select names (which (= stratum stratas))) "")
                   "]")))))
            (1+ (iseq (length (send self :blocks)))))
    cs-string)
  )

(defmeth drag-graph-proto :update-model-mixed (&optional &key (redraw nil))
 (send self :read-model
  (concatenate 'string (inner (send self :return-model 'current 'gc))
   "|" (send self :cs-string-from-blocks)))
 (slot-value 'model-number (send self :return-model-number 'last))
 )

(defmeth drag-graph-proto :update-arrows
  (&optional (x nil) &key (redraw nil))
  (if (or x (and (send self :visual-blocks) (send self :blocks)))
      (let ((remove nil))
	(mapcar #'(lambda (vertex)
		    (let ((b -1))
		      (mapcar #'(lambda (block)
				  (if (and (in-block (send vertex :position)
						     (send block :position))
					   (send block :visual))
				      (setf b (send block :stratum))))
			      (send self :blocks))
		      (if (and (not (equal (send vertex :stratum) b))
			       (not (equal -1 b)))
			  (progn (setf remove T)
				 (send vertex :stratum b)))))
		(send self :vertices))
        (send self :update-model-mixed)
	(if remove (send self :remove-tests))
	(if redraw (send self :redraw-graph))))
  )

;;; (defmeth drag-graph-proto :add-block
;;;   (n &key (a (list -45 -45 -50)) (b (list -40 -40  50)) (label nil))
;;;   (if (not (and (2-3-list a) (2-3-list b))) (error "Invalid argument"))
;;;   (send self :blocks
;;; 	(concatenate 'list
;;; 		     (list (send block-proto
;;; 				 :new n (length (send self :blocks))
;;; 				 (list a b) label (send self :visual-blocks)))
;;; 		     (send self :blocks)))
;;;   (send self :update-arrows) ;;; ???
;;;   (send self :redraw-graph)
;;;   )
;;; 
;;; (defmeth drag-graph-proto :delete-block (n)
;;;   (let ((blocks nil))
;;;     (mapcar #'(lambda (i)
;;; 		(if (not (if (listp n)
;;; 			     (eq (car n) (send self :block-index i))
;;; 			   (eq (if (numberp n)
;;; 				   (send i :stratum) (send i :label))
;;; 			       n)))
;;; 		    (setf blocks (cons i blocks))))
;;; 	    (send self :blocks))
;;;     (send self :blocks (reverse blocks)))
;;;   (send self :update-arrows T) ;;; ???
;;;   (send self :redraw-graph)
;;;   )
;;; 
;;; (defmeth block-proto :delete ()
;;;   (send graph :delete-block (send self :stratum))
;;;   )
;;; 
;;; ;;; cocograph.lsp:
;;; 
;;; (defun return-coco-graph-window
;;;   (identification model-number vertices edges blocks use-variables
;;; 		  &key (location nil) (size nil) (title nil))
;;;   (let ((x (send coco-graph-window-proto :new identification
;;; 		 :location location :title title)))
;;;     (if size
;;; 	(send x :size (car size) (cadr size)))
;;;     (send x :slot-value 'identification)
;;;     (send x :slot-value 'model-number model-number)
;;;     (send x :slot-value 'vertices vertices)
;;;     (send x :slot-value 'use-variables use-variables)
;;;     (send x :slot-value 'edges edges)
;;;     (send x :slot-value 'undo-positions nil)
;;;     (send x :slot-value 'redo-positions nil)
;;;     (send x :slot-value 'rejected-edges nil)
;;;     (send x :slot-value 'accepted-edges nil)
;;;     (send x :slot-value 'drag-graph T)
;;;     (send x :slot-value 'edit-graph nil)
;;;     (send x :slot-value 'static T)
;;;     (send x :slot-value 'colors *default-colors*) ;copy?
;;;     (send x :slot-value 'transformation nil)
;;;     (send x :slot-value 'angle 0.01)
;;;     (send x :slot-value 'overlays nil)
;;;     (send x :slot-value 'blocks blocks)
;;;     (send x :slot-value 'grid nil)
;;;     (send x :use-color (screen-has-color))
;;;     (add-coco-graph-menu x)
;;;     (if blocks
;;; 	(mapcar #'(lambda (item)
;;; 		    (if (or (equalp (send item :title) "Tests ...")
;;; 			    (equalp (send item :title)
;;; 				    "Description of fitted values ...")
;;; 			    (equalp (send item :title) "The EH procedure ..."))
;;; 			(send item :enabled nil)))
;;; 		(send (send x :slot-value 'menu) :slot-value 'items)))
;;;     (send x :update-arrows)
;;;     (send x :redraw-graph)
;;;     x)
;;;   )
;;; 
;;; ;;; ...


(defmeth coco-proto :make-graph
  (&key (model nil) (location nil) (size nil) (title nil))
  (let* ((vertices (return-default-vertices (send self :return-names)))
         (model-number (if model
                           (if (stringp model)
                               (progn (send self :read-model model)
                                      (send self :return-model-number 'last))
                             (if (numberp model) model
                               (send self :return-model-number model)))
                         (send self :return-model-number nil)))
;;;         (x (format t "Model:  ~%"))
         (x (print model-number))
;;;         (x (format t (send self :return-model model-number 'gc)))
;;;         (x (format t (send self :return-model model-number 'cs)))
;;;         (x (format t "~% Causal structure: ~%"))
         (cs (send self :return-model model-number 'cs))
;;;         (x (format t cs))

;;;         (blocks (caar (string-to-block-list cs)))
;;;         (x (print blocks))

         (a (split-string cs))
;;;         (x (print a))
         (blocks (select a (which a)))
;;;         (x (print blocks))

;;;         (x (format t "~% THE CAUSAL MODEL !!! ~%"))
         (graph (return-coco-graph-window
                 (slot-value 'identification) model-number vertices
                 (return-edge-list (send self :return-edge-list model-number)
                                   (car vertices))
                 nil (send self :return-model-set model-number)
                 :location location :size size :title title))
         (graph (if blocks (progn
                            (send graph :set-blocks blocks) graph) graph))
         )
    graph
    )
  )

(defun make-coco-model-graph-window
  (coco-model-object &key (model nil) (location nil) (size nil) (title nil))
 (call-method coco-proto :make-graph
  :model (if model model
          (send coco-model-object :slot-value 'model-number))
  :location location :size size :title title)
 )

;;; (defun make-coco-model-graph-window
;;;   (coco-model-object &key (model nil) (location nil) (size nil) (title nil))
;;;   (send coco-model-object :make-current
;;; 	(if model
;;; 	    (if (stringp model)
;;; 		(progn (send self :read-model model)
;;; 		       (send self :return-model-number 'last))
;;; 	      (if (numberp model) model
;;; 		(send coco-model-object :return-model-number model)))
;;; 	  (send coco-model-object :slot-value 'model-number)))
;;;   (let ((vertices 
;;; 	 (return-default-vertices (send coco-model-object :return-names))))
;;;     (return-coco-graph-window
;;;      (send coco-model-object :slot-value 'identification)
;;;      (send coco-model-object :return-model-number 'current)
;;;      vertices
;;;      (return-edge-list (send coco-model-object :return-edge-list 'current)
;;; 		       (car vertices))
;;;      nil
;;;      (send coco-model-object :return-model-set 'current)
;;;      :location location :size size :title title))
;;;   )
;;; 
;;; ;;;

(defmeth coco-graph-window-proto :return-child-coco-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil))
  (let* ((model-number (if model
		 (if (stringp model)
		     (progn (send self :read-model model)
			    (send self :return-model-number 'last))
		   (if (numberp model) model
		     (send self :return-model-number model)))
	       (send self :return-model-number 'current)))
;;;         (x (format t "Model:  ~%"))
;;;         (x (format t (send self :return-model model-number 'gc)))
;;;         (x (format t "~% Causal structure: ~%"))
         (cs (send self :return-model model-number 'cs))
;;;         (x (format t cs))

;;;         (blocks (car (string-to-block-list cs)))
;;;         (x (print blocks))

         (a (split-string cs))
;;;         (x (print a))
         (blocks (select a (which a)))
;;;         (x (print blocks))

;;;         (x (format t "~% THE CAUSAL MODEL !!! ~%"))
	 (vertices (if (and copy-vertices (not blocks) (not (slot-value 'blocks)))
		       (slot-value 'vertices)
		     (return-default-vertices (send self :return-names))))
	 (graph
	  (return-coco-graph-window
	   (slot-value 'identification) model-number vertices
	   (return-edge-list (send self :return-edge-list model-number)
                             (car vertices))
	   nil ;;; (slot-value 'blocks)
           (send self :return-model-set model-number)
	   :size size :location (if location location
				  (+ offset (send self :location)))
	   :title (concatenate 'string (send self :title) title)))
         (graph (if blocks (progn (send graph :set-blocks blocks) graph)
                  graph))
         )
    (if parant
	(send self :add-manager-edge self graph)
      (if child
	  (send self :add-manager-edge graph self)
	(if sibling
	    (send self :add-manager-edge self graph :type 'sibling))))
    (if copy-slots
	(progn
	  (send graph :slot-value 'colors (slot-value 'colors))
	  (send graph :rejected-edges (send self :rejected-edges))
	  (send graph :accepted-edges (send self :accepted-edges))))
    graph)
  )

(defmeth coco-graph-window-proto :return-child-coco-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil)
        (proto-type coco-graph-window-proto))
  (let* ((model-number (if model
		 (if (stringp model)
		     (progn (send self :read-model model)
			    (send self :return-model-number 'last))
		   (if (numberp model) model
		     (send self :return-model-number model)))
	       (send self :return-model-number 'current)))
         (cs (send self :return-model model-number 'cs))
         (a (split-string cs))
         (blocks (select a (which a)))
	 (vertices (if (and copy-vertices (not blocks) (not (slot-value 'blocks)))
		       (slot-value 'vertices)
		     (return-default-vertices (send self :return-names))))
	 (graph
	  (return-coco-graph-window
	   (slot-value 'identification) model-number vertices
	   (return-edge-list (send self :return-edge-list model-number)
                             (car vertices))
	   nil ;;; (slot-value 'blocks)
           (send self :return-model-set model-number)
	   :size size :location (if location location
				  (+ offset (send self :location)))
	   :title (concatenate 'string (send self :title) title)
           :proto-type (if proto-type proto-type
                         (eval (send self :slot-value 'proto-name)))))
         (graph (if blocks (progn (send graph :set-blocks blocks) graph)
                  graph)))
    (if parant
	(send self :add-manager-edge self graph)
      (if child
	  (send self :add-manager-edge graph self)
	(if sibling
	    (send self :add-manager-edge self graph :type 'sibling))))
    (if copy-slots
	(progn
	  (send graph :slot-value 'colors (slot-value 'colors))
	  (send graph :rejected-edges (send self :rejected-edges))
	  (send graph :accepted-edges (send self :accepted-edges))))
    graph)
  )

(defmeth coco-graph-window-proto :return-history-and-future (block &key (redraw nil))
 (if (< 0 (length (which (= (send self :return-level-list :full nil) 0))))
  (format t "Should not use :return-history-and-future for causal mixed models!!! ~%"))
 (if (and block (send self :blocks))
   (let ((history nil)
         (current nil)
         (future nil))
    (mapcar #'(lambda (vertex use)
               (if (< 0 use)
                (if (< (send vertex :stratum) block)
                    (setf history (concatenate
                                   'list history
                                   (list (send vertex :name))))
                  (if (> (send vertex :stratum) block)
                      (setf future (concatenate
                                    'list future
                                    (list (send vertex :name))))
                    (setf current (concatenate
                                   'list current
                                   (list (send vertex :name))))))))
            (send self :vertices)
            (slot-value 'use-variables))
    (list history current future)))
 )
