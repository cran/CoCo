
;;; Copyright 2003 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the coco-factor-graph-window-proto
;;; and adds the basic methods to this proto.


(setf *coco-graph-window-proto-version* "1.4")

(require "cocoapix")
(require "cocometh")
(require "association-diagram" "assocdiag")
(require "cocograph")

(defproto coco-factor-graph-window-proto
  '() '(instances-coco-factor-graph)
  (list coco-graph-window-proto))

(send coco-factor-graph-window-proto :documentation
      'proto "CoCo Factor Graph Window based on~
              CoCo Graph Window prototype")

(defmeth coco-factor-graph-window-proto
  :isnew (identification &key location title)
  (let ((x (call-method association-diagram-proto :isnew
			:location location :title title)))
    (send coco-factor-graph-window-proto :slot-value
                                         'instances-coco-factor-graph
	  (cons x (slot-value 'instances-coco-factor-graph)))
    (send x :slot-value 'identification identification)
    x)
  )

(defmeth coco-factor-graph-window-proto :generator-to-vertices (generator)
  (mapcar #'(lambda (char) (send self :vertex char))
	  (split-name-string (send generator :name)))
  )

(defmeth edge-proto :is-factor-edge ()
    (send (cadr (send self :vertices)) :is-generator)
    )

;;; To :return-model-indices,
;;; "patch" since not returning models as indices from CoCo.

;; (defmeth drag-graph-proto :vertex (variable-name)
;;   (if (objectp variable-name)
;;       variable-name
;;     (if (numberp variable-name)
;; 	(nth variable-name (send self :vertices))
;;       (if (characterp variable-name)
;; 	  (send self :vertex (concatenate 'string (list variable-name)))
;; 	(do ((i (send self :vertices) (cdr i)))
;; 	    ((if (car i) (equal variable-name (send (car i) :name)) T)
;; 	     (if (if (car i) (equal variable-name (send (car i) :name)))
;; 		 (car i)))))))
;;   )

(defmeth coco-proto :vertex (variable-name &optional (vertices nil))
  (if (objectp variable-name)
      variable-name
    (if (numberp variable-name)
	(nth variable-name (if vertices vertices (send self :vertices)))
      (if (characterp variable-name)
	  (send self :vertex (concatenate 'string (list variable-name))
		vertices)
	(do ((i (if vertices vertices (send self :vertices)) (cdr i)))
	    ((if (car i) (equal variable-name (send (car i) :name)) T)
	     (if (if (car i) (equal variable-name (send (car i) :name)))
		 (car i)))))))
  )

;; (defmeth coco-graph-window-proto :string-to-vertices (string)
;;  (mapcar #'(lambda (char) (send self :vertex char))
;;	  (split-name-string string))
;;  )

;; Update of same in "cocotest.lsp":

(defmeth coco-graph-window-proto :string-to-vertices (string
						      &optional (vertices nil))
  (mapcar #'(lambda (char) (send self :vertex char vertices))
	  (split-name-string string))
  )

(defmeth coco-proto :string-to-vertices (string &optional (vertices nil))
  (mapcar #'(lambda (char) (send self :vertex char vertices))
	  (split-name-string string))
  )

;; Update of same in "mips-factor.lsp":

(defmeth coco-proto :return-model-vertices 
  (&optional (vertices nil) (model 'current modelset) number &key (simple T)) ;; !!!!!
  (let ((vertices (if vertices vertices (send self :vertices)))
	(generators (send self :return-generators model :simple simple)))
    (mapcar #'(lambda (char) (send self :string-to-vertices char vertices))
	    generators))
  )

(defmeth coco-proto :return-model-indices
  (&optional (vertices nil) (l nil) (model 'current modelset) number)
  (let ((vertices (if vertices vertices (send self :vertices)))
        (l  (if l l (length vertices)))
        (a  (send self :return-model-vertices vertices model number))
        (result nil))
    (mapcar #'(lambda (g i)
		(mapcar #'(lambda (object)
			    (setf result (cons (list (send object :index) i)
					       result ))) g))
	    a (+ l (iseq (length a))))
    result)
  )

;; 


(defmeth vertex-proto :is-generator ()
  (or (eq 'generator (send self :type))
      (eq 'discrete-generator  (send self :type))
      (eq 'linear-generator    (send self :type))
      (eq 'quadratic-generator (send self :type))
      )
  )


;; Updated in mips-factor.lsp

(defmeth coco-proto :return-gcs (model &key (full nil))
  (mapcar #'(lambda (i)
	      (list
	       ;; Name:
	       i
	       ;; Variable-label:
	       nil
	       ;; (concatenate 'string "Variable " (list (int-char i)))
	       ;; Variable-type (discrete, continuous, ordinal, etc.):
	       'generator
	       ;; Stratum:
	       0))
          (cddr (split-string-chr (send self :return-model model)
                                  #\[  #\] nil)))
  )

(defmeth coco-proto :return-gcs (model &key (full nil) (simple nil))
  (mapcar #'(lambda (i)
	      (list
	       ;; Name:
	       (car i)
	       ;; Variable-label:
	       nil
	       ;; (concatenate 'string "Variable " (list (int-char i)))
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
		model :full full :simple T ;; !!!! simple
		:noted T))
  )

;; New method:

(defmeth coco-proto :make-factor-graph
  (&key (model nil) (location nil) (size nil) (title nil))
  (let* ((nr (send self :return-model-number-with-enter model))
         (names (send self :return-names))
	 (generators (send self :return-gcs nr))
         (vertices (return-default-vertices (concatenate 'list names
							 generators)))
         (graph-edges (return-edge-list (send self :return-edge-list nr)
                                        (car vertices)))
         (factor-pairs (send self :return-model-indices (car vertices)
                             (length names) nr))
         (factor-edges (return-edge-list factor-pairs (car vertices)
                                         :type 'factor-edge)))
    (return-coco-graph-window ;; return-coco-factor-graph-window
     (slot-value 'identification)
     nr
     vertices
     (list (concatenate 'list (car graph-edges) (car factor-edges)))
     nil
     (concatenate 'list (send self :return-model-set nr)
		  (repeat 1 (length generators)))
     :location location :size size :title title
     :proto-type coco-factor-graph-window-proto))
  )

;; New method:

(defmeth coco-graph-window-proto :update-factor-positions ()
  (let ((a (send self :vertices)))
    (mapcar
     #'(lambda (generator)
	 (if (and (send generator :is-generator) (not (send self :blocks)))
	     (let* ((vertices (split-name-string (send generator :name)))
		    (p (mapcar #'(lambda (i)
				   (send (send self :vertex i a) :position))
			       vertices)))
	       (let ((new-position 0))
		 (mapcar #'(lambda (i)
			     (setf new-position (+ new-position i))) p)
		 (send generator :position (/ new-position (length p)))))
	   )) a))
  )

(defmeth coco-graph-window-proto :update-factor-positions ()
  (let ((a (send self :vertices)))
    (mapcar
     #'(lambda (generator)
	 (if (and (send generator :is-generator) (not (send self :blocks)))
	     (let* ((vertices (split-name-string (send generator :name)))
		    (p (mapcar #'(lambda (i)
				   (send (send self :vertex i a) :position))
			       vertices)))
	       (let ((new-position 0))
		 (mapcar #'(lambda (i)
			     (setf new-position (+ new-position i))) p)
		 (let* ((position (/ new-position (length p)))
			(type (send generator :type))
			(z (if (eq 'gc type) 20
			     (if (eq 'linear-generator type) 40
			       (if (eq 'discrete-generator type) -20
				 (if (eq 'quadratic-generator type) -40)))))
			(xyz (if (eq 0 (caddr position))
				 (list (car position) (cadr position) z) position)))
		   (send generator :position xyz))))
	   )) a))
  )

;; New method:

(defmeth coco-graph-window-proto :return-child-coco-factor-graph-window
  (&key (model 'current modelset) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil)
	(permit-factor-graph T) (proto-type coco-factor-graph-window-proto))
  (let* ((old-current (send self :before-set-current model modelset))
	 (nr (send self :return-model-number-with-enter model))
         (names (send self :return-names))
	 (graph-vertices (if copy-vertices
			     (list (select (car (slot-value 'vertices))
				     (which (mapcar
					     #'(lambda (i)
						 (not (send i :is-generator)))
					     (car (slot-value 'vertices))))))
			   (return-default-vertices names)))
	 (generators (send self :return-gcs nr))
	 (generator-vertices (return-default-vertices generators
						      (length names)
						      T))
         (vertices (list (concatenate 'list (car graph-vertices)
				      (car generator-vertices))))
         (graph-edges (return-edge-list (send self :return-edge-list nr)
                                        (car vertices)))
         (factor-pairs (send self :return-model-indices (car vertices)
                             (length names) nr))
         (factor-edges (return-edge-list factor-pairs (car vertices)
                                         :type 'factor-edge))
	 (graph
	  (return-coco-graph-window ;; return-coco-factor-graph-window
	   (slot-value 'identification) nr vertices
           (list (concatenate 'list (car graph-edges) (car factor-edges)))
	   (slot-value 'blocks)
           (concatenate 'list (send self :return-model-set nr)
                        (repeat 1 (length generators)))
	   :size size :location (if location location
				  (+ offset (send self :location)))
	   :title (concatenate 'string (send self :title) title)
           :proto-type (if proto-type proto-type
                         (eval (send self :slot-value 'proto-name))))))
    ;;                    ^- The prototype of self!!!
    (send graph :update-factor-positions)
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
    (send self :after-set-current old-current graph 'unconditioned nil))
  )

;; Update of same in "draggraph.lsp":

(defmeth coco-factor-graph-window-proto :drag-point (x y m1 m2 vertex)
  (send self :start-buffering)
  (if vertex
      (let* ((position (send vertex :position))
	     (new-pos (send self :find-move
			    (send self :from-x-pixel x)
			    (send self :from-y-pixel y) position)))
	(if (and m1 m2)
	    (send vertex :label-position
		  (if (send vertex :fix-label-position)
		      (+ position new-pos) new-pos))
	  (if m2
	      nil			; (send self :do-hand-rotate x y m1 m2)
	    (if m1
		(progn
		  (send vertex :position (+ position new-pos))
		  (if (not (send vertex :is-generator))
		      (send self :update-factor-positions))
		  )
	      nil)))))			; (send self :drag-line-from-point x y)
  (send self :redraw-graph)
  (send self :buffer-to-screen)
  )

;; New method:

(defmeth coco-factor-graph-window-proto :return-child-coco-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil)
        (proto-type nil))
  (send self :return-child-coco-factor-graph-window
	:model model :copy-slots copy-slots :copy-vertices copy-vertices
	:parant parant :child child :sibling sibling :location location
	:offset offset :size size :title title :proto-type proto-type)
   )

;; New method:

(defmeth coco-factor-graph-window-proto :update-model
  (&key (model nil) (hierarchical T) (copy-vertices T) (title ""))
  (let* ((nr (send self :return-model-number-with-enter model))
         (names (send self :return-names))
	 (graph-vertices (if copy-vertices
			     (list (select (car (slot-value 'vertices))
				     (which (mapcar
					     #'(lambda (i)
						 (not (send i :is-generator)))
					     (car (slot-value 'vertices))))))
			   (return-default-vertices names)))
	 (generators (send self :return-gcs nr))
	 (generator-vertices (return-default-vertices generators
						      (length names)
						      T))
         (vertices (list (concatenate 'list (car graph-vertices)
				      (car generator-vertices))))
         (graph-edges (return-edge-list (send self :return-edge-list nr)
                                        (car vertices)))
         (factor-pairs (send self :return-model-indices (car vertices)
                             (length names) nr))
         (factor-edges (return-edge-list factor-pairs (car vertices)
                                         :type 'factor-edge)))
    (slot-value 'vertices vertices)
    (slot-value 'edges 
           (list (concatenate 'list (car graph-edges) (car factor-edges))))
    (if hierarchical
	(send self :use-variables 
	      (concatenate 'list (send self :return-model-set nr)
			   (repeat 1 (length generators)))))
    (if (not (equalp title "")) (send self :title title))
    (send self :set-new-model-number nr)
    (slot-value 'model-number nr)
    (send self :update-factor-positions)
    self)
  )

;; Update of same in "vertices.lsp":

(defmeth coco-factor-graph-window-proto :update-vertices-of-edges (&optional (vertices nil))
  (send self :update-model :model (send self :return-model-number nil))
  )

;; Update of same in "cocotest.lsp":


(defmeth coco-factor-graph-window-proto :test-remove-edge-from-current
  (string edge &key (decomposable-mode nil))
  (if (and (send self :is-graphical 'current)
	   (not (send edge :is-factor-edge)))
      (send self :drop-edges string T)
    (if (if edge
	    (eq 'quadratic-generator 
		(send (cadr (send edge :vertices)) :type)))
	(send self :drop-interactions string T 'quadratic)
      (send self :drop-interactions string T)))
  (send self :current)
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
		     'not-submodel-of-base))
	       'non-decomposable)
	   'not-submodel-of-base)))
    (send self :dispose-of-model 'current)
    result)
  )
