
;;; Update of method :define-blocks of "blocks.lsp":

(defun inner (str) (concatenate 'string
                    (but-last (cdr (map-elements #'char
                                    str (iseq (length str)))))))

(defmeth drag-graph-proto :define-blocks
  (block-list &optional (labels nil) (nanny nil) (draw-blocks T))
  (let ((new self)
        (current-number (send self :return-model-number 'current)))
    (let ((causal-model
	   (concatenate 'string
			(inner (send self :return-model
				     'current nil :type 'gc))
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

;;; The method :stratas is new in "cgblocks.lsp":

(defmeth drag-graph-proto :stratas (&optional (val nil))
  (if val
      (mapcar #'(lambda (i j) (send i :stratum j))
	      (send self :vertices) val)
    (mapcar #'(lambda (i) (send i :stratum)) (send self :vertices)))
  )

;;; The method :sc-string-from-blocks is new in "cgblocks.lsp":

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

;;; The method :update-causal-model is new in "cgblocks.lsp":

;;; (defmeth coco-proto :xxx
;;;   (&optional (model 'current modelset) number
;;; 	     &key (type 'gc))
;;;  (let ((old-current (send self :before-set-current model modelset number)))
;;;   (let ((result (xxx)))
;;;    (send self :after-set-current old-current result 'unconditioned nil)))
;;;  )

(defmeth drag-graph-proto :update-causal-model (&optional &key (redraw nil))
 (send self :read-model
;;; 13. september 2002: Argument "self" to :return-model
;;; for  :before-set-current other places ?!?!?
  (concatenate 'string (inner (send self :return-model self nil :type 'gc))
   "|" (send self :cs-string-from-blocks)))
 (slot-value 'model-number (send self :return-model-number 'last))
 )

;;; Update of the method :update-arrows of "blocks.lsp":

(defmeth drag-graph-proto :update-arrows
  (&optional (x nil) &key (redraw nil))
  (if (or x (and (send self :visual-blocks) (send self :blocks)))
      (let ((remove nil))
	(mapcar #'(lambda (vertex)
		    (if (not (send vertex :is-generator))
		    (let ((b -1))
		      (mapcar #'(lambda (block)
				      (if (and (in-block 
				             (send vertex :position)
						     (send block :position))
					   (send block :visual))
				      (setf b (send block :stratum))))
			      (send self :blocks))
		      (if (and (not (equal (send vertex :stratum) b))
			       (not (equal -1 b)))
			  (progn (setf remove T)
				 (send vertex :stratum b))))))
		(send self :vertices))
        (send self :update-causal-model)
	(if remove (send self :remove-tests))
	(if redraw (send self :redraw-graph))))
  )

;;; Update of the method :make-graph of "cocograph.lsp":

(defmeth coco-proto :make-graph
  (&key (model nil) (location nil) (size nil)
	(title nil) (permit-factor-graph T))
  (let ((vertices (return-default-vertices (send self :return-names)))
	(nr (send self :return-model-number-with-enter model)))
    (if (or (send self :is-graphical nr) (not permit-factor-graph))
	(let* (;; (x (print nr))
	       (cs (send self :return-model nr nil :type 'cs))
	       (a (split-string cs :cococg (equal *coco-type* "cococg")))
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


;;; Update of the function make-coco-model-graph-window of "cocograph.lsp":

(defun make-coco-model-graph-window
  (coco-model-object &key (model nil) (location nil) (size nil) (title nil))
 (call-method coco-proto :make-graph
  :model (if model model
          (send coco-model-object :slot-value 'model-number))
  :location location :size size :title title)
 )

;;; Update of the method :return-child-coco-graph-window of "cocograph.lsp":

(defmeth coco-graph-window-proto :return-child-coco-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil)
        (permit-factor-graph T) (proto-type coco-graph-window-proto))
  (let ((nr (send self :return-model-number-with-enter model)))
    (if (or (send self :is-graphical 'current) (not permit-factor-graph))
	(let* ((cs (send self :return-model nr nil :type 'cs))
	       (a (split-string cs :cococg (equal *coco-type* "cococg")))
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


;;; Update of the method  :return-history-and-future of "cocotest.lsp":

(defmeth coco-graph-window-proto :return-history-and-future
  (block &key (redraw nil))
 (if (< 0 (length (which (= (send self :return-level-list :full nil) 0))))
  (format t
   "Should not use :return-history-and-future for causal mixed models!!! ~%"))
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


;; Update of "Mips" to "Factor-graph":
;; Could be put in, e.g., "mipsblocks.lsp", but not "mips.lsp".

(defmeth coco-proto :return-gcs (model &key (full nil) (simple nil)) ;; !!!!!
  (mapcar #'(lambda (i)
	      (list
	       ;; Name:
	       (car i)
	       ;; Variable-label:
	       nil
	       ;; (concatenate 'string "Variable" (list (int-char i)))
	       ;; Variable-type (discrete, continuous, ordinal, etc.):
	       (case (cadr i)
		 ('gc        'generator)
		 ('generator 'generator)
		 ('discrete  'discrete-generator)
		 ('linear    'linear-generator)
		 ('quadratic 'quadratic-generator)) ;; 'generator
	       ;; Stratum:
	       0))
	  (send self :return-generators
		model :full full :simple simple :noted T))
  )

(defmeth coco-proto :return-model-vertices 
  (&optional (vertices nil) (model 'current modelset) number
	     &key (simple nil)) ;; !!!!!
  (let ((vertices (if vertices vertices (send self :vertices)))
	(generators (send self :return-generators model :simple simple)))
    (mapcar #'(lambda (char) (send self :string-to-vertices char vertices))
	    generators))
  )
