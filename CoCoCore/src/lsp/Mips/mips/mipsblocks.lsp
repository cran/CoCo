
;;; blocks.lsp

(defmeth drag-graph-proto :blocks (&optional (val nil set))
  (if set
      (progn 
	;; < This I do not understand:
	;; (setf (car (slot-value 'blocks)) val)
	(send self :slot-value 'blocks (list val))
	;; >
	(send self :update-arrows T)))
  (car (slot-value 'blocks))
  )

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
                                     (inner (send self :return-model 'current nil :type 'gc))
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

(defmeth drag-graph-proto :update-causal-model (&optional &key (redraw nil))
 (send self :read-model
  (concatenate 'string (inner (send self :return-model 'current nil :type 'gc))
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
        (send self :update-causal-model)
	(if remove (send self :remove-tests))
	(if redraw (send self :redraw-graph))))
  )

(defmeth coco-proto :make-graph
  (&key (model nil) (location nil) (size nil)
	(title nil) (permit-factor-graph T))
  (let ((vertices (return-default-vertices (send self :return-names)))
	(nr (send self :return-model-number-with-enter model)))
    (if (or (send self :is-graphical nr) (not permit-factor-graph))
	(let* ((x (print nr))
	       (cs (send self :return-model nr nil :type 'cs))
	       (a (split-string cs))
	       (blocks (select a (which a)))
	       (graph (return-coco-graph-window
		       (slot-value 'identification) nr vertices
		       (return-edge-list
			(send self :return-edge-list nr)
			(car vertices))
		       nil (send self :return-model-set nr)
		       :location location :size size :title title))
	       (graph
		(if blocks (progn
			     (send graph :set-blocks blocks) graph) graph)))
	  graph)
      (send self :make-factor-graph :model model :location location
	    :size size :title title)))
  )

(defun make-coco-model-graph-window
  (coco-model-object &key (model nil) (location nil) (size nil) (title nil))
 (call-method coco-proto :make-graph
  :model (if model model
          (send coco-model-object :slot-value 'model-number))
  :location location :size size :title title)
 )

(defmeth coco-graph-window-proto :return-child-coco-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil)
        (permit-factor-graph T) (proto-type coco-graph-window-proto))
  (let ((nr (send self :return-model-number-with-enter model)))
    (if (or (send self :is-graphical 'current) (not permit-factor-graph))
	(let* ((cs (send self :return-model nr nil :type 'cs))
	       (a (split-string cs))
	       (blocks (select a (which a)))
	       (vertices (if (and copy-vertices
				  (not blocks) (not (slot-value 'blocks)))
			     (slot-value 'vertices)
			   (return-default-vertices
			    (send self :return-names))))
	       (graph
		(return-coco-graph-window
		 (slot-value 'identification) nr vertices
		 (return-edge-list (send self :return-edge-list nr)
				   (car vertices))
		 (slot-value 'blocks)
		 (send self :return-model-set nr)
		 :size size :location (if location location
					(+ offset (send self :location)))
		 :title (concatenate 'string (send self :title) title)
		 :proto-type (if proto-type proto-type
			       (eval (send self :slot-value 'proto-name)))))
	       (graph
		(if blocks (progn (send graph :set-blocks blocks) graph)
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
      (send self :return-child-coco-factor-graph-window
	    :model model :copy-slots copy-slots :copy-vertices copy-vertices
	    :parant parant :child child :sibling sibling :location location
	    :offset offset :size size :title title
	    :proto-type coco-factor-graph-window-proto)))
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
