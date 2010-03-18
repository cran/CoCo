
;;; Copyright 1993 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the manager-proto for coco-graph-windows from the
;;; association-diagram-proto and adds the basic methods to this proto.


(defproto manager-proto '(identification) '(instances-manager)
;;  (list coco-model-proto association-diagram-proto)
  (list coco-graph-window-proto)
  )

(send manager-proto :documentation
      'proto "CoCo manager based on Association Diagram Proto Type")

(defmeth manager-proto :isnew (identification &key location (title nil))
  (let ((plot (apply #'call-method association-diagram-proto :isnew
		     :location location
		     :title (list (if title
                                      (concatenate 'string
                                                   "Manager of "
                                                   title)
                                    "Model Manager")))))
    (send plot :slot-value 'identification identification)
    (send manager-proto :slot-value 'instances-manager
	  (cons self (slot-value 'instances-manager)))
    plot)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defproto manager-vertex-proto '(graph) '(instances-manager-vertices)
  (list vertex-proto))

(send manager-vertex-proto :documentation
      'proto "Manager vertex based on vertex prototype")

(defmeth manager-vertex-proto :isnew
  (name label type stratum index position graph)
  (let ((x (call-method vertex-proto :isnew
			name label type stratum index position)))
    (send manager-vertex-proto :slot-value 'instances-manager-vertices
	  (cons x (slot-value 'instances-manager-vertices)))
    (send x :slot-value 'graph graph)
    x)
  )

(defun return-manager-vertices (graphs)
  (list
   (mapcar
    #'(lambda (i graph)
	(send manager-vertex-proto :new
	      ;; Name:
	      (int-char (+ 65 i))
	      ;; Vertex-label:
	      (send graph :title)
	      ;; Vertex-type (continuous, discrete, ordinal, etc.):
	      0              ;;; Error !!!  Closed, Hidden windows ?
	      ;; Stratum:
	      0
	      ;; Index:
	      i
	      ;; Variable position (x,y,z):
	      (list (car (send graph :location))
		    (cadr (send graph :location)) 0)
	      graph))
    (iseq (length graphs)) graphs))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#|

(defun return-graphs ()
  (reverse (send association-diagram-proto :slot-value 'instances-graph))
  )

(defmeth association-diagram-proto :isnew (&key location title)
  (let ((x (apply #'call-next-method
		  :location location :title (list title))))
    (send association-diagram-proto :slot-value 'instances-graph
	  (cons x (slot-value 'instances-graph)))
    x)
  )

|#

(defun return-graphs ()
  (reverse (send coco-graph-window-proto :slot-value 'instances-coco-graph))
  )

(defmeth coco-graph-window-proto :isnew (identification &key location title)
  (let ((x (call-method association-diagram-proto :isnew
			:location location :title title)))
    (send coco-graph-window-proto :slot-value 'instances-coco-graph
	  (cons x (slot-value 'instances-coco-graph)))
    (send x :slot-value 'identification identification)
    (send x :add-graph-to-managers)
    x)
  )

(defmeth manager-proto :return-graphs ()
  (let ((graphs (return-graphs)))
    (if (eq nil (slot-value 'identification))
	graphs
      (if graphs
	  (let ((valid-graphs
		 (mapcar #'(lambda (i)
			     (if (and (send i :has-slot 'identification)
				      (eq (send i :slot-value 'identification)
					  (slot-value 'identification)))
				 i nil)) graphs)))
	    (select valid-graphs (which valid-graphs))))))
  )

(defmeth manager-proto :graph
  (variable-name &optional (graph nil set) &key (redraw nil))
  (if set
      (send (send self :vertex variable-name) :graph graph)
    (send (send self :vertex variable-name) :graph))
  )

(defmeth manager-vertex-proto :graph (&optional (val nil set))
  (if set (setf (slot-value 'graph) val)) (slot-value 'graph)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun return-manager
  (&optional (id nil) &key (location nil) (size nil) (title nil)
	     (hide-window nil))
  (let* ((x (send manager-proto :new id :location location :title title))
	 (graphs (send x :return-graphs)))
    (if size (send x :size (car size) (cadr size)))
    (send x :slot-value 'vertices (return-manager-vertices graphs))
    (send x :slot-value 'edges (list nil))
    (send x :slot-value 'use-variables (repeat 1 (length graphs)))
    (send x :slot-value 'undo-positions nil)
    (send x :slot-value 'redo-positions nil)
    (send x :slot-value 'rejected-edges nil)
    (send x :slot-value 'accepted-edges nil)
    (send x :slot-value 'drag-graph T)
    (send x :slot-value 'edit-graph nil)
    (send x :slot-value 'static T)
    (send x :slot-value 'colors *default-colors*)
    (send x :slot-value 'transformation nil)
    (send x :slot-value 'angle 0.01)
    (send x :slot-value 'overlays nil)
    (send x :slot-value 'blocks nil)
    (send x :slot-value 'grid nil)
    (send x :use-color (screen-has-color))
    (add-coco-graph-menu x)
    (if hide-window
	(send x :hide-window)
      (send x :redraw-graph))
    x)
  )

(defmeth coco-proto :return-manager
  (&key (location nil) (size nil) (title nil))
  (return-manager (slot-value 'identification)
		  :location location :size size :title title)
  )

(defmeth manager-proto :to-x-pixel (x)
  (floor (* (/ (send self :canvas-width) 1152) (+ 0 x)))
  )    
(defmeth manager-proto :to-y-pixel (y)
  (floor (* (/ (send self :canvas-height) 900) (+ 0 y)))
  )    
(defmeth manager-proto :from-x-pixel (x)
  (- (* (/ x (send self :canvas-width)) 1152) 0)
  )    
(defmeth manager-proto :from-y-pixel (y)
  (- (* (/ y (send self :canvas-height)) 900) 0)
  )

(defmeth manager-proto :x-pos-to-tex (x)
  x
  )

(defmeth manager-proto :y-pos-to-tex (y)
  (- y)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :add-graph-to-manager (graph)
  (let* ((vertices (send self :vertices))
	 (new (send manager-vertex-proto :new
		    ;; Name:
		    (int-char (+ 65 (length vertices)))
		    ;; Vertex-label:
		    (send graph :title)
		    ;; Vertex-type:
		    0              
		    ;; Error !!!  Closed, Hidden windows ?
		    ;; Stratum:
		    0
		    ;; Index:
		    (length vertices)
		    ;; Variable position (x,y,z):
		    (list (car (send graph :location))
			  (cadr (send graph :location)) 0)
		    graph)))
    (send self :vertices (concatenate 'list vertices (list new)))
    (send self :use-variables
	  (concatenate 'list (send self :use-variables) (list 1)))
    (send self :redraw-graph))
  )

(defmeth association-diagram-proto :add-graph-to-managers ()
  (dolist (manager (send manager-proto :slot-value 'instances-manager) T)
	  (if (or (not (send manager :slot-value 'identification))
		  (equalp (send manager :slot-value 'identification)
			  (slot-value 'identification)))
	      (send manager :add-graph-to-manager self)))
  )

(defmeth association-diagram-proto :add-graph-to-managers ()
  (mapcar #'(lambda (manager)
	      (if (or (not (send manager :slot-value 'identification))
		      (equalp (send manager :slot-value 'identification)
			      (slot-value 'identification)))
		  (send manager :add-graph-to-manager self)))
	  (send manager-proto :slot-value 'instances-manager))
  )

(defmeth association-diagram-proto :update-managers ()
  (dolist (x (send manager-proto :slot-value 'instances-manager) T)
	  (mapcar #'(lambda (graph vertex)
		      (send vertex :position (send graph :location))
		      ;; Type: Show, Hide or Close ?
		      (send vertex :label (send graph :title)))
		  (send x :return-graphs)
		  (send x :vertices))
	  (send x :redraw-graph))
  )

(defmeth manager-proto :return-model-number (&optional (model 'current modelset))
  )

(defmeth manager-proto :update-model
  (&key (model nil) (hierarchical T) (copy-vertices T) (title ""))
  )

(defmeth association-diagram-proto :add-manager-edge
  (from to &key (type 'directed))
  (dolist (x (send manager-proto :slot-value 'instances-manager) T)
	  (let* ((u (send x :vertex from))
		 (v (send x :vertex to)))
	    (if (and u v)
		(progn
		  (send x :edges
			(concatenate 'list (send x :edges)
				     (list (send edge-proto :new
						 (list u v) :type type))))
		  (send x :redraw-graph)))))
  )

(defmeth manager-proto :update-screen ()
  (mapcar #'(lambda (graph vertex)
	      (send graph :location
		    (round (car (send vertex :position)))
		    (round (cadr (send vertex :position))))
	      ;; Type: Show, Hide or Close ?
	      (send graph :title (send vertex :label)))
	  (send self :return-graphs)
	  (send self :vertices))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :drag-point (x y m1 m2 vertex)
  (call-method association-diagram-proto :drag-point x y m1 m2 vertex)
  (send (send vertex :graph) :location
	(round (send self :from-x-pixel x))
	(round (send self :from-y-pixel y)))
  )

(defmeth manager-proto :vertex (variable-name)
  (if (objectp variable-name)
      (if (kind-of-p variable-name drag-graph-proto)
	  (do ((n 0 (1+ n))
	       (i (send self :return-graphs) (cdr i)))
	      ((or (not i) (equalp variable-name (car i)))
	       (if (equalp variable-name (car i))
		   (nth n (send self :vertices)))))
	variable-name)
    (call-method association-diagram-proto :vertex variable-name))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :test-two-objects (a b edge
					    &key (decomposable-mode nil))
  (if (equalp (send a :slot-value 'identification)
	      (send b :slot-value 'identification))
      (if (send a :is-submodel-of (send b :slot-value 'model-number))
	  (if (or (not decomposable-mode)
		  (and (send b :is-decomposable) (send a :is-decomposable)))
	      (let ((test (send b :return-test-object a b)))
		(if (and test (< 0 (send test :df)))
		    (progn
		      (format t "Testing~%")
		      (send edge :width
			    (send self :p-to-width
				  (send self :select-p-value test
					:print-test T)))
		      (send edge :dashed nil)
		      test)
		  'not-submodel-of-base))
	    'non-decomposable)
	'not-submodel-of-base)
    'not-same-coco-object)
  )

(defmeth manager-proto :test-edge (edge &key (decomposable-mode nil))
  (send edge :test
	(let ((vertices (send edge :vertices)))
	  (if (or (send (send (car vertices) :graph) :blocks)
		  (send (send (cadr vertices) :graph) :blocks))
	      (progn
		(print "Not implemented for Block-recursive models")
		'error)
	    (send self :test-two-objects
		  (send (car vertices) :graph) (send (cadr vertices) :graph)
		  edge :decomposable-mode decomposable-mode))))
  )

(defmeth manager-proto :test-edge-nth (p &key (decomposable-mode nil))
  (send self :test-edge-nth (nth p (send self :edges)) 
	:decomposable-mode decomposable-mode)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :draw-edge (edge &optional (radius 4))
  (let ((vertices (send edge :vertices)))
    (if (and (not (equalp 6 (send (cadr vertices) :type)))
	     (not (equalp 6 (send (car vertices) :type))))
	(let ((width (send edge :width)))
	  (if (screen-has-color) (send self :set-edge-color edge))
	  (send self :line-width (if width   width 1))
	  (send self :line-type  (if (send edge :dashed) 'dashed 'solid))
	  (let
	      ((x1 (send self :project (send (car vertices) :position)))
	       (x2 (send self :project (send (cadr vertices) :position))))
	    (send self :draw-arrow
		  (car x1) (cadr x1) (caddr x1)
		  (car x2) (cadr x2) (caddr x2)
		  (if (and width (< 1 width)) (* 20 (/ width 16)) 2)
		  (and width (< 1 width)) radius)))))
  )

(defmeth manager-proto :graph-add-edge (p1 p2)
  (if (= p1 p2)
      nil
    (progn
      (if (send self :vertex-pair-in-edge-list p1 p2 (send self :edges))
	  (send self :set-edge-label (send self :edge (list p1 p2)) :center T
		:follow T :decomposable-mode nil)
	(progn
	  (send self :edges
		(concatenate
		 'list (list 
			(send edge-proto :new
			      (list (nth p1 (send self :vertices))
				    (nth p2 (send self :vertices)))
			      :type 'directed))
		 (send self :edges)))
	  (if (not (send self :set-edge-label-nth 0 :center T
			 :follow T :decomposable-mode nil))
	      (send self :edges (cdr (send self :edges))))
	  (send self :redraw-graph)))))
  )

(defmeth manager-proto :graph-add-vertex-pair (u v)
  (send self :graph-add-edge (send u :index) (send v :index))
  )

(defmeth manager-proto :graph-drop-edge
  (&optional edge &key (edges nil) (point nil) (x-move 0))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :make-graph-current-model (&key (redraw-plots nil)
						       (key-event nil))
  (let ((vertex (send self :return-closest-vertex-in-canvas
			     (send self :x) (send self :y))))
    (if vertex
	(send (send vertex :graph)
	      :make-graph-current-model :redraw-plots redraw-plots)))
  )

(defmeth manager-proto :make-graph-base-model (&key (redraw-plots nil)
						    (key-event nil))
  (let ((vertex (send self :return-closest-vertex-in-canvas
			     (send self :x) (send self :y))))
    (if vertex
	(send (send vertex :graph)
	      :make-graph-base-model :redraw-plots redraw-plots)))
  )

(defmeth manager-proto :select-p-value (test &key (print-test nil))
  (call-method coco-graph-window-proto
	       :select-p-value test :print-test print-test)
  )

(defmeth manager-proto :format-p-value (p)
  (call-method coco-graph-window-proto :format-p-value p)
  )

(defmeth manager-proto :p-to-width (p-value)
  (call-method coco-graph-window-proto :p-to-width p-value)
  )

(defmeth manager-proto :do-key (c m1 m2)
  (call-method coco-graph-window-proto :do-key c m1 m2)
  )

#|
(defmeth manager-proto :in-wastebasket ()
  (and (> (slot-value 'x) (send self :canvas-width))
       (> (slot-value 'y) (send self :canvas-height)))
  )
|#

(defmeth manager-proto :vertex-type
  (variable-name &optional (type nil set) &key (redraw nil) (set-type T))
  (if set
      (progn
	(if set-type
	    (let ((graph (send self :graph variable-name)))
	      (case type
		    (0 (send graph :show-window)) 
		    (1 (send graph :hide-window)) 
		    (2 (send graph :close))
		    (t (send graph :show-window)))))
	(call-method association-diagram-proto :vertex-type
		     variable-name type :redraw redraw))
    (call-method association-diagram-proto :vertex-type variable-name))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :click-vertex (point)
  (send self :show-vertex point :redraw T)
  )

(defmeth manager-proto :show-vertex (vertex-number &key (redraw nil))
  (send self :vertex-type vertex-number 0 :set-type nil :redraw redraw)
  (send (send self :graph vertex-number) :show-window :redraw redraw)
;;;  (send (send self :graph vertex-number) :redraw-graph)
  )

(defmeth association-diagram-proto :show-window (&key (redraw T))
  (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	  (send i :vertex-type self 0 :set-type nil :redraw redraw))
  (call-method graph-proto :show-window)
  (send self :redraw-graph)
  )

(defmeth manager-proto :show-window ()
  (call-method graph-proto :show-window)
  )

(defmeth manager-proto :show-family-2
  (vertex &key (offsprings nil) (ancestors nil) (siblings nil)
	  (redraw nil) (first nil))
  (let* ((old-type (send vertex :type))
	 (visit-vertex (not (equalp 'CONTINUOUS old-type))))
    (if (and (not first) visit-vertex) (send self :show-vertex vertex))
    (if (or first visit-vertex)
	(dolist (i (send self :edges) T)
		(let ((vertices (send i :vertices)))
		  (if (and siblings (not (equalp (send i :type) 'directed)))
		      (progn
			(if (equalp vertex (car vertices))
			    (send self :show-family-2 (cadr vertices)
				  :offsprings offsprings :ancestors ancestors
				  :siblings T))
			(if (equalp vertex (cadr vertices))
			    (send self :show-family-2 (car vertices)
				  :offsprings offsprings :ancestors ancestors
				  :siblings T))))
		  (if (and offsprings (equalp (send i :type) 'directed))
		      (if (equalp vertex (car vertices))
			  (send self :show-family-2 (cadr vertices)
				:offsprings T :ancestors ancestors
				:siblings siblings)))
		  (if (and ancestors (equalp (send i :type) 'directed))
		      (if (equalp vertex (cadr vertices))
			  (send self :show-family-2 (car vertices)
				:offsprings offsprings :ancestors T
				:siblings siblings))))))
    (if (and first visit-vertex offsprings ancestors)
	(send self :vertex-type vertex old-type)))
  )

(defmeth association-diagram-proto :show-family
  (&key (offsprings nil) (ancestors nil) (siblings nil) (redraw T))
  (if (kind-of-p self manager-proto)
      (let ((vertex (send self :return-closest-vertex-in-canvas
			  (send self :x) (send self :y))))
	(if vertex
	    (send self :show-family-2 vertex :redraw redraw
		  :offsprings offsprings :ancestors ancestors
		  :siblings siblings :first t)))
    (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	    (if (send i :vertex self)
		(send i :show-family-2 (send i :vertex self) :redraw redraw
		      :offsprings offsprings :ancestors ancestors
		      :siblings siblings :first T))))
  (if redraw
      (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	      (send i :redraw)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :hide-vertex (vertex-number &key (redraw nil))
  (send self :vertex-type vertex-number 1 :set-type nil :redraw redraw)
  (send (send self :graph vertex-number) :hide-window :redraw redraw)
  )

(defmeth association-diagram-proto :hide-window (&key (redraw T))
  (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	  (send i :vertex-type self 1 :set-type nil :redraw redraw))
  (call-method graph-proto :hide-window)
  )

(defmeth manager-proto :hide-window ()
  (call-method graph-proto :hide-window)
  )

(defmeth manager-proto :hide-family-2
  (vertex &key (offsprings nil) (ancestors nil) (siblings nil)
	  (redraw nil) (first nil))
  (let* ((old-type (send vertex :type))
	 (visit-vertex (equalp 'CONTINUOUS old-type)))
    (if (and (not first) visit-vertex) (send self :hide-vertex vertex))
    (if (or first visit-vertex)
	(dolist (i (send self :edges) T)
		(let ((vertices (send i :vertices)))
		  (if (and siblings (not (equalp (send i :type) 'directed)))
		      (progn
			(if (equalp vertex (car vertices))
			    (send self :hide-family-2 (cadr vertices)
				  :offsprings offsprings :ancestors ancestors
				  :siblings T))
			(if (equalp vertex (cadr vertices))
			    (send self :hide-family-2 (car vertices)
				  :offsprings offsprings :ancestors ancestors
				  :siblings T))))
		  (if (and offsprings (equalp (send i :type) 'directed))
		      (if (equalp vertex (car vertices))
			  (send self :hide-family-2 (cadr vertices)
				:offsprings T :ancestors ancestors
				:siblings siblings)))
		  (if (and ancestors (equalp (send i :type) 'directed))
		      (if (equalp vertex (cadr vertices))
			  (send self :hide-family-2 (car vertices)
				:offsprings offsprings :ancestors T
				:siblings siblings))))))
    (if (and first visit-vertex offsprings ancestors)
	(send self :vertex-type vertex old-type)))
  )

(defmeth association-diagram-proto :hide-family
  (&key (offsprings nil) (ancestors nil) (siblings nil) (redraw T))
  (if (kind-of-p self manager-proto)
      (let ((vertex
	     (send self :return-closest-vertex-in-canvas
		   (send self :x) (send self :y))))
	(if vertex
	    (send self :hide-family-2 vertex :redraw redraw
		  :offsprings offsprings :ancestors ancestors
				:siblings siblings :first t)))
    (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	    (if (send i :vertex self)
		(send i :hide-family-2 (send i :vertex self) :redraw redraw
		      :offsprings offsprings :ancestors ancestors
				:siblings siblings :first t))))
  (if redraw
      (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	      (send i :redraw)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :close-vertex (vertex-number &key (redraw nil))
  (send self :vertex-type vertex-number 6 :set-type nil :redraw redraw)
  (send (send self :graph vertex-number) :close :redraw redraw)
  )

(defmeth association-diagram-proto :close (&key (redraw T))
  (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	  (send i :vertex-type self 6 :set-type nil :redraw redraw))
  (call-method graph-proto :close)
  )

(defmeth manager-proto :close ()
  (call-method graph-proto :close)
  )

(defmeth manager-proto :close-family-2
  (vertex &key (offsprings nil) (ancestors nil) (siblings nil)
	  (redraw nil) (first nil))
  (let* ((old-type (send vertex :type))
	 (visit-vertex (or (equalp 'CONTINUOUS old-type)
			   (equalp 'DISCRETE old-type))))
    (if (and (not first) visit-vertex) (send self :close-vertex vertex))
    (if (or first visit-vertex)
	(dolist (i (send self :edges) T)
		(let ((vertices (send i :vertices)))
		  (if (and siblings (not (equalp (send i :type) 'directed)))
		      (progn
			(if (equalp vertex (car vertices))
			    (send self :close-family-2 (cadr vertices)
				  :offsprings offsprings :ancestors ancestors
				  :siblings T))
			(if (equalp vertex (cadr vertices))
			    (send self :close-family-2 (car vertices)
				  :offsprings offsprings :ancestors ancestors
				  :siblings T))))
		  (if (and offsprings (equalp (send i :type) 'directed))
		      (if (equalp vertex (car vertices))
			  (send self :close-family-2 (cadr vertices)
				:offsprings T :ancestors ancestors
				:siblings siblings)))
		  (if (and ancestors (equalp (send i :type) 'directed))
		      (if (equalp vertex (cadr vertices))
			  (send self :close-family-2 (car vertices)
				:offsprings offsprings :ancestors T
				:siblings siblings))))))
    (if (and first visit-vertex offsprings ancestors)
	(send self :vertex-type vertex old-type)))
  )

(defmeth association-diagram-proto :close-family
  (&key (offsprings nil) (ancestors nil) (siblings nil) (redraw T))
  (if (kind-of-p self manager-proto)
      (let ((vertex
	     (send self :return-closest-vertex-in-canvas
		   (send self :x) (send self :y))))
	(if vertex
	    (send self :close-family-2 vertex :redraw redraw
		  :offsprings offsprings :ancestors ancestors
				:siblings siblings :first t)))
    (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	    (if (send i :vertex self)
		(send i :close-family-2 (send i :vertex self) :redraw redraw
		      :offsprings offsprings :ancestors ancestors
				:siblings siblings :first t))))
  (if redraw
      (dolist (i (send manager-proto :slot-value 'instances-manager) T)
	      (send i :redraw)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth manager-proto :vertex-position
  (variable-name &optional (position nil)
		 &key (redraw nil) (set-location T))
  (if (and position (not (2-3-list position))) (error "Invalid argument"))
  (if position
      (progn
	(if set-location
	    (send (send self :graph variable-name)
		  :location (round (car position)) (round (cadr position))))
	(call-method association-diagram-proto :vertex-position
		     variable-name position :redraw redraw))
    (call-method association-diagram-proto :vertex-position variable-name))
  )

(defmeth association-diagram-proto :location (&optional (x nil) (y nil))
  (if x
      (progn
	(dolist (i (send manager-proto :slot-value 'instances-manager) T)
		(progn
		  (send i :vertex-position self (list x y 0) :set-location nil)
		  ;; To many redraw by update screen:
		  (send i :redraw-graph)))
	(call-method graph-proto :location x y))
    (call-method graph-proto :location))
  )

(defmeth manager-proto :location (&optional (x nil) (y nil))
  (if x
      (call-method graph-proto :location x y)
    (call-method graph-proto :location))
  )


(defmeth manager-proto :vertex-label
  (variable-name &optional (label nil set) &key (redraw nil) (set-title T))
  (if set
      (progn
	(if set-title
	    (send (send self :graph variable-name) :title label))
	(call-method association-diagram-proto :vertex-label
		     variable-name label :redraw redraw))
    (call-method association-diagram-proto :vertex-label variable-name))
  )

(defmeth association-diagram-proto :title (&optional string)
  (if string
      (progn
	(dolist (i (send manager-proto :slot-value 'instances-manager) T)
		(progn
		  (send i :vertex-label self string :set-title nil)
		  (send i :redraw-graph)))
	(call-method graph-proto :title string))
    (call-method graph-proto :title))
  )

(defmeth manager-proto :title (&optional (string nil))
  (if string
      (call-method graph-proto :title string)
    (call-method graph-proto :title))
  )

;;

(provide "cocomanager")
