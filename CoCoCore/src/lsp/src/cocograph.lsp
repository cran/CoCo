
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the coco-graph-window-proto
;;; and adds the basic methods to this proto


(setf *coco-graph-window-proto-version* "1.4")

(require "cocoapix")
(require "cocometh")
(require "association-diagram" "assocdiag")

(defproto coco-graph-window-proto
  '() '(instances-coco-graph)
  (list coco-model-proto association-diagram-proto))

(send coco-graph-window-proto :documentation
      'proto "CoCo Graph Window based on~
              Association diagram prototype and CoCo prototype")

(defmeth coco-graph-window-proto :isnew (identification &key location title)
  (let ((x (call-method association-diagram-proto :isnew
			:location location :title title)))
    (send coco-graph-window-proto :slot-value 'instances-coco-graph
	  (cons x (slot-value 'instances-coco-graph)))
    (send x :slot-value 'identification identification)
    x)
  )


;;
(defmeth association-diagram-proto :save ()
  `(let ((x (send COCO-OBJECT :make-graph :model ',(send self :return-model))))
     (send x :title              ',(send self :title             ))
     (send x :size               ',(car (send self :size         ))
	   ',(cadr (send self :size        )))
     (send x :location           ',(car (send self :location     ))
	   ',(cadr (send self :location    )))
     (send x :positions          ',(send self :positions         ))
     (send x :names              ',(send self :names             ))
     (send x :edges              ',(send self :edges             ))
     (send x :use-variables      ',(send self :use-variables     ))
     (send x :static             ',(send self :static            ))
     (send x :grid               ',(send self :grid              ))
     (send x :drag-graph         ',(send self :drag-graph        ))
     (send x :edit-graph         ',(send self :edit-graph        ))
     ;;    (send x :rejected-edges     ',(send self :rejected-edges    ))
     ;;    (send x :accepted-edges     ',(send self :accepted-edges    ))
     (send x :blocks             ',(send self :blocks))
     )
  )   




(defproto dynamic-coco-spin-proto
  '(identification model-number a b c) '(instances-dynamic-coco-spin)
  (list spin-proto coco-graph-window-proto))

(send dynamic-coco-spin-proto :documentation
      'proto "CoCo Spin based on spin proto type")

;;
(defun return-coco-graph-window
  (identification model-number vertices edges blocks use-variables
		  &key (location nil) (size nil) (title nil)
                  (proto-type coco-graph-window-proto))
  (let ((x (send proto-type :new identification :location location :title title)))
    (if size
	(send x :size (car size) (cadr size)))
    (send x :slot-value 'identification)
    (send x :slot-value 'model-number model-number)
    (send x :slot-value 'vertices vertices)
    (send x :slot-value 'use-variables use-variables)
    (send x :slot-value 'edges edges)
    (send x :slot-value 'undo-positions nil)
    (send x :slot-value 'redo-positions nil)
    (send x :slot-value 'rejected-edges nil)
    (send x :slot-value 'accepted-edges nil)
    (send x :slot-value 'drag-graph T)
    (send x :slot-value 'edit-graph nil)
    (send x :slot-value 'static nil)
    (send x :slot-value 'colors *default-colors*) ;copy?
    (send x :slot-value 'transformation nil)
    (send x :slot-value 'angle 0.01)
    (send x :slot-value 'overlays nil)
    (send x :slot-value 'blocks blocks)
    (send x :slot-value 'grid nil)
    (send x :use-color (screen-has-color))
    (add-coco-graph-menu x)
    (if blocks
	(mapcar #'(lambda (item)
		    (if (or (equalp (send item :title) "Tests ...")
			    (equalp (send item :title)
				    "Description of fitted values ...")
			    (equalp (send item :title) "The EH procedure ..."))
			(send item :enabled nil)))
		(send (send x :slot-value 'menu) :slot-value 'items)))
    (send x :update-arrows)
    (send x :redraw-graph)
    x)
  )

(defun add-coco-graph-menu (x)
  )

;;

;;; Method :return-child-coco-graph-window is updated in "cgblocks.lsp":

(defmeth coco-graph-window-proto :return-child-coco-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil)
	(permit-factor-graph T) (proto-type nil))
  (let* ((nr (send self :return-model-number-with-enter model)))
    (if (or (send self :is-graphical 'current) (not permit-factor-graph))
	(let* ((vertices (if copy-vertices
			     (slot-value 'vertices)
			   (return-default-vertices
			    (send self :return-names))))
	       (graph
		(return-coco-graph-window
		 (slot-value 'identification) nr vertices
		 (return-edge-list (send self :return-edge-list nr)
				   (car vertices))
		 (slot-value 'blocks) (send self :return-model-set nr)
		 :size size :location (if location location
					(+ offset (send self :location)))
		 :title (concatenate 'string (send self :title) title)
		 :proto-type (if proto-type proto-type
			       (eval (send self :slot-value 'proto-name))))))
	  ;;                    ^- The prototype of self!!!
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

(defmeth coco-graph-window-proto :return-block-trace-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling T) (size nil) (icon T)
	(location nil) (offset (list -5 -60)) (title "; Trace graph"))
  (let ((graph (send self :return-child-coco-graph-window :model model
		     :copy-slots copy-slots :copy-vertices copy-vertices
		     :location location :title title
		     :size (if icon (round (/ (send self :size) 3))
			     (send self :size))
		     :offset (if icon offset
			       (+ (list (* (if (> (car (send self :location))
						  400) -1 1)
					   (+ 20 (car (send self :size)))) 0)
				  offset))
		     :parant parant :child child :sibling sibling)))
    (if (boundp '*trace-graph*) (send *trace-graph* :close))
    (setf *trace-graph* graph)
    (defmeth graph :do-click (x y m1 m2))
    (defmeth graph :do-key (c m1 m2))
    (mapcar #'(lambda (edge) (send edge :dashed T)) (send graph :edges))
    (mapcar #'(lambda (item)
		(send item :enabled nil))
	    (send (send graph :slot-value 'menu) :slot-value 'items)))
  )

;;; Method :make-graph is updated in "cgblocks.lsp":

(defmeth coco-proto :make-graph (&key (model nil) (location nil) (size nil)
				      (title nil) (permit-factor-graph T))
  (let ((vertices (return-default-vertices (send self :return-names)))
	(nr (send self :return-model-number-with-enter model)))
    (if (or (send self :is-graphical nr) (not permit-factor-graph))
	(return-coco-graph-window
	 (slot-value 'identification)
	 nr
	 vertices
	 (return-edge-list (send self :return-edge-list nr) (car vertices))
	 nil
	 (send self :return-model-set nr)
	 :location location :size size :title title))
    (send self :make-factor-graph :model model :location location
	  :size size :title title))
  )

(defun make-coco-model-graph-window
  (coco-model-object &key (model nil) (location nil) (size nil) (title nil)
		     (permit-factor-graph T))
  (send coco-model-object :make-current
	(if model
	    (send coco-model-object :return-model-number-with-enter model)
	  (send coco-model-object :slot-value 'model-number)))
  (let ((vertices 
	 (return-default-vertices (send coco-model-object :return-names))))
    (if (or (send coco-model-object :is-graphical 'current)
	    (not permit-factor-graph))
	(return-coco-graph-window
	 (send coco-model-object :slot-value 'identification)
	 (send coco-model-object :return-model-number 'current)
	 vertices
	 (return-edge-list (send coco-model-object :return-edge-list 'current)
			   (car vertices))
	 nil
	 (send coco-model-object :return-model-set 'current)
	 :location location :size size :title title)
      (send coco-model-object :make-factor-graph :model
	    model :location location :size size :title title)))
  )

(defmeth coco-model-proto :make-graph
  (&key (model nil) (location nil) (size nil) (title nil))
  (make-coco-model-graph-window self :model model
				:location location :size size :title title)
  )

(defmeth coco-graph-window-proto :make-graph
  (&key (model nil) (location nil) (size nil) (title nil)
	(parant nil) (child nil) (sibling T))
  (let ((graph (call-next-method :model model :location location
				 :size size :title title)))
    (if parant
	(send self :add-manager-edge self graph)
      (if child
	  (send self :add-manager-edge graph self)
	(if sibling
	    (send self :add-manager-edge self graph :type 'sibling))))
    graph)
  )

(defmeth coco-proto :display-all-models
  (&optional (y-location -30) (title "") (location '(100 100)))
  (let ((current-number (send self :return-model-number 'current)))
    (send self :current)
    (let ((stop nil))
      (send self :current)			       
      (do ((i 20 (+ i 50))) (stop nil)
;	  (if (kind-of-p self manager-proto)
;	      (send self :make-graph
;		    :model 'current
;		    :location (+ (list i y-location)
;				 (send self :location))
;		    :title (concatenate
;			    'string
;			    (send self :title) title
;			    (send self :return-model 'current)))
	    (if (kind-of-p self coco-graph-window-proto)
		(send self :return-child-coco-graph-window
		      :model 'current
		      :location (+ (list i y-location) (send self :location))
		      :title (concatenate
			      'string
			      (format nil "~5d:"
				      (send self :return-model-number 'current))
			      title (send self :return-model 'current))
		      :sibling T)
	      (send self :make-graph
		    :model 'current
		    :location (+ (list i y-location) location)
		    :title (concatenate
			    'string
;			    (send self :title)
			    (format nil "~5d:"
				    (send self :return-model-number 'current))
			    title (send self :return-model 'current))))
;	    )
	  (if (> (send self :return-model-number 'current) 1)
	      (send self :make-current 'previous)
	    (setf stop T))))
    (send self :make-current current-number))
  )


(defun import-and-make-saturated (file)
  (let ((coco-cg-object (make-coco :title file)))
    (send coco-cg-object :import file)
    (let* ((model (send coco-cg-object :make-model "*"))
           (graph (send model :make-graph :title file)))
      (send graph :item-color 'vertex-label 'red)
      (send graph :add-controls)
      graph))
  )

(defun import-and-display-all (file)
  (let ((coco-cg-object (make-coco :title file)))
    (send coco-cg-object :import file)
    (send coco-cg-object :current)
    (send coco-cg-object :display-all-models)
    ; (send *current-manager* :display-all-models)
    coco-cg-object)
  )

;;
(defun coco-resume (&key (coco-id *current-coco*))
  (format t "~%")
  (format t "******************************************************* ~%")
  (format t "*                                                     * ~%")
  (format t "*                                                     * ~%")
  (format t "*   Do not use `resume-coco' when you have graphs.    * ~%")
  (format t "*                                                     * ~%")
  (format t "*   You Graph-Windows will be `dead' while in CoCo.   * ~%")
  (format t "*                                                     * ~%")
  (format t "*                                                     * ~%")
  (format t "******************************************************* ~%")
  (if (= *ended-coco* *current-coco*)
      (format t "This CoCo is ended ~%")
    (ok-coco (call-coco -1 nil :coco-id coco-id)))
  )

(defun coco-end (&key (coco-id *current-coco*))
  (if (/= *ended-coco* coco-id)
      (call-coco 0 (1- (length (which (if-else (= (cdr *coco-identifications*)
						  *ended-coco*) nil T))))
		 :coco-id coco-id) nil)
  (setf *coco-identifications*
	(combine nil (if-else
		      (= (cdr *coco-identifications*) coco-id)
		      *ended-coco* (cdr *coco-identifications*))))
  (if (= coco-id *current-coco*) (setf *current-coco* *ended-coco*))
  (let ((coco-graphs (send coco-graph-window-proto
			   :slot-value 'instances-coco-graph))
	(remaining-graphs nil))
    (mapcar #'(lambda (i)
		(if (eq (send i :slot-value 'identification) coco-id)
;;;		    (remove i (send coco-graph-window-proto
;;;				    :slot-value 'instances-coco-graph))
		    (send i :close)
		  (setf remaining-graphs (cons i remaining-graphs))))
	    coco-graphs)
    (send coco-graph-window-proto :slot-value
	  'instances-coco-graph remaining-graphs))

  (let ((managers (send manager-proto :slot-value 'instances-manager))
	(remaining-managers nil))
    (mapcar #'(lambda (i)
		(if (eq (send i :slot-value 'identification) coco-id)
;;;		    (remove i (send manager-proto
;;;				    :slot-value 'instances-manager))
		    (send i :close)
		  (setf remaining-managers (cons i remaining-managers))))
	    managers)
    (send manager-proto :slot-value 'instances-manager remaining-managers))

  (let ((coco-models (send coco-model-proto :slot-value 'instances-model))
	(remaining-models nil))
    (mapcar #'(lambda (i)
		(if (eq (send i :slot-value 'identification) coco-id)
		    (send i :slot-value 'identification *ended-coco*)
		  (setf remaining-models (cons i remaining-models))))
	    coco-models)
    (send coco-model-proto :slot-value 'instances-model remaining-models))
  T
  )

;;

(defun format-invalid (x)
  x
  )

(defmeth coco-graph-window-proto :format-p-value (p)
  (format nil "P = ~7,5f" (format-invalid p))
  )

(defmeth coco-graph-window-proto :p-to-width (p-value)
 (if (invalid-real p-value) 1
  (if (equalp p-value 0) 20
   (+ (+ 2 (floor (* (log (/ 1.00 0.20)) (/ 1 (log 2)))))
    (floor (* (log (/ 0.20 p-value)) (/ 1 (log 2)))))))
 )

#|

(defmeth coco-graph-window-proto :p-to-width (p)
  (length (which
	   (< p (list 2 0.40 0.20 0.10 0.05 0.025 0.01 0.005 0.0025 0.001
		      0.0005 0.00025 0.0001 0.00005 0.000025 0.00001
		      0.000005 0.0000025 0.000001 0.0000005 0.00000025 0.0000001))))
  )

(defmeth coco-graph-window-proto :compute-and-format-p-value (edge)
  (let ((p (send self :select-p-value (car (car (cddddr edge))))))
    (format nil "P = ~7,5f" (format-invalid p)))
  )

(defmeth coco-graph-window-proto :COMPUTE-EDGE-WIDTH
  (test &key (print-test nil))
  (send self :p-to-width
	(send self :select-p-value test :print-test print-test))
  )

|#

(defmeth coco-graph-window-proto :set-new-model-number (n)
  (if (not (send self :static))
      (dolist (i (send dynamic-coco-spin-proto :slot-value
		       'instances-dynamic-coco-spin))
	      (if (eq (send i :slot-value 'model-number)
		      (slot-value 'model-number))
		  (send i :slot-value 'model-number n))))
  (slot-value 'model-number n)
  )

(defmeth association-diagram-proto :add-manager-edge (from to)
  )

(defmeth coco-graph-window-proto :update-model
  (&key (model nil) (hierarchical T) (copy-vertices T) (title ""))
  (let ((x self))
    (slot-value 'edges (return-edge-list
			(send self :return-edge-list model)
			(send self :vertices)))
    (if hierarchical
	(send x :use-variables (send x :return-model-set model)))
    (if (not (equalp title "")) (send self :title title))
    (send self :set-new-model-number
	  (send self :return-model-number model))
    (slot-value
     'model-number (send self :return-model-number model)))
  )

;;


(defmeth coco-graph-window-proto :graph-drop-gc
  (gc hierarchical &key (create-graph T) (copy-vertices T) (x-move 0))
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base)))
    (if (send self :make-graph-current-model :redraw-plots nil)
	(let ((x self))
	  (if (or (not (send self :is-graphical 'current)) hierarchical) ;; !!
              (if (if hierarchical
		      (eq 'quadratic-generator (send hierarchical :type)))
               (send self :drop-interactions gc T 'quadratic)
               (send self :drop-interactions gc T))
	    (send self :drop-edges gc T))
	  (send self :current)
	  (if create-graph
	      (if (send self :static)
		  (progn
		    (setf x (send x :return-child-coco-graph-window
				  :copy-slots T
				  :copy-vertices copy-vertices
				  :offset (list (+ 20 x-move) -30)
				  :title (concatenate 'string " -" gc)
				  :parant T)))
		(send self :update-model :model 'current
		      :title (concatenate 'string (send self :title)
					  " -" gc))))
	  (send self :make-current current-number)
	  (send self :make-base base-number)
	  (if (not (send self :static))
	      (dolist (i (send dynamic-coco-spin-proto :slot-value
			       'instances-dynamic-coco-spin))
		      (send i :change-models)))
	  (send self :redraw-graph)
	  x)
      (progn (send self :make-current current-number)
	     (send self :make-base base-number)
	     nil)))
  )

(defmeth coco-graph-window-proto :graph-drop-edge
  (&optional edge &key (edges nil) (point nil) (x-move 0))
  (let ((gc (if point (send self :point-to-string point)
	      (if edges (send self :edge-list-to-string edges)
		(send edge :edge-to-string)))))
    (send self :graph-drop-gc gc point :xmove x-move))
  )

(defmeth coco-graph-window-proto :graph-drop-edge-nth
  (&optional p &key (edges nil) (point nil) (x-move 0))
  "Method args: (p &key (x-move 0)
Drops the p'te edge in the edge list for the object. If :static then a new
CoCo graph window is created, else the edge is removed from the current object.
Location is for the new object set to
:location (+ (list (+ 20 x-move) -30) (send self :location)).
The resulting model is added to the model list in the CoCo object.
The current model in the CoCo object remains the current model.
The resulting object is returned."
  (send self :graph-drop-edge (if p (nth p (send self :edges)))
	:edges edges :point point :x-move x-move)
  )

;;

(defmeth coco-graph-window-proto :graph-add-gc
  (gc &key (hierarchical nil) (create-graph T) (copy-vertices T) (x-move 0))
  (let ((current-number (send self :return-model-number 'current)))
    (if (send self :make-graph-current-model :redraw-plots nil)
	(let ((x self))
	  (if (or (not (send self :is-graphical 'current)) hierarchical) ;; !!
	      (send self :add-interactions gc T)
	    (send self :add-edges gc T))
	  (send self :current)
	  (if create-graph
	      (if (send self :static)
		  (progn
		    (setf x (send x :return-child-coco-graph-window
				  :copy-slots T
				  :copy-vertices copy-vertices
				  :offset (list (+ 20 x-move) -120)
				  :title (concatenate 'string " +" gc)
				  :child T)))
		(send self :update-model :model 'current
		      :title (concatenate 'string (send self :title)
					  " +" gc))))
	  (send self :make-current current-number)
	  (if (not (send self :static))
	      (dolist (i (send dynamic-coco-spin-proto :slot-value
			       'instances-dynamic-coco-spin))
		      (send i :change-models)))
	  (send self :redraw-graph)
	  x)
      (progn (send self :make-current current-number)
	     (send self :redraw-graph) nil)))
  )

(defmeth coco-graph-window-proto :graph-add-edge
  (&optional p1 p2 &key (edges nil) (x-move 0))
  (if (or edges
	  (and (not (equalp p1 p2))
	       (not (send self :vertex-pair-in-edge-list p1 p2
			  (send self :edges)))))
      (let ((gc (if edges (send self :vertex-lists-to-string edges)
		  (send self :position-to-name p1 p2))))
	(send self :graph-add-gc gc :x-move x-move)))
  )

(defmeth coco-graph-window-proto :graph-add-vertex-pair
  (&optional u v &key (edges nil) (x-move 0))
  (send self :graph-add-edge (send u :index) (send v :index) :edges edges
	:x-move x-move)
  )

(defmeth coco-graph-window-proto :add-fill-in
  (&key (create-graph T) (copy-vertices T))
  (let ((x self)
	(current-number (send self :return-model-number 'current)))
    (send self :make-graph-current-model :redraw-plots nil)
    (if (not (send self :is-decomposable 'current))
	(progn
	  (send self :generate-decomposable)
	  (send self :current)
	  (if create-graph
	      (if (send self :static)
		  (progn
		    (setf x (send x :return-child-coco-graph-window
				  :copy-slots T
				  :copy-vertices copy-vertices
				  :offset (list 20 -30)
				  :title (concatenate 'string " + FillIn")
				  :child T)))
		(progn
		  (slot-value 'edges
			      (return-edge-list
			       (send self :return-edge-list 'current)
			       (send self :vertices)))
		  (send self :remove-tests)
		  (send self :title (concatenate
				     'string (send self :title) " + FillIn "))
		  (slot-value 'model-number
			      (send self :return-model-number 'current))))))
      (format t "Graph is decomposable ~%"))
    (send self :make-current current-number)
    (send self :redraw-graph) x)
  )

;;

(defmeth coco-graph-window-proto :make-graph-current-model (&key)
  (send self :make-current (slot-value 'model-number))
  (if (boundp 'current-control-proto)
      (dolist (i (send current-control-proto :slot-value
		       'instances-current-control))
	      (send i :redraw-graph)))
  )

(defmeth coco-graph-window-proto :make-graph-base-model (&key)
  (send self :make-base (slot-value 'model-number))
  (if (boundp 'base-control-proto)
      (dolist (i (send base-control-proto :slot-value
		       'instances-base-control))
	      (send i :redraw-graph)))
  )

(defmeth association-diagram-proto :set-exact-test (&optional (val nil set))
  (if set 
      (progn
	(call-next-method val)
	(if (boundp 'linked-toggle-item-proto)
	    (send linked-toggle-item-proto :update
		  (send self :slot-value 'identification) 'exact-test val)))
    (send self :set-switch 'exact-test 'what))
  )


(defmeth coco-graph-window-proto :set-exact-test (&optional (val nil set))
  (if (and set (not (equalp val 'what)))
      (progn
	(call-next-method val)
	(if (boundp 'linked-toggle-item-proto)
	    (send linked-toggle-item-proto :update
		  (send self :slot-value 'identification) 'exact-test val)))
    (send self :set-switch 'exact-test 'what))
  )

(defmeth coco-graph-window-proto :set-switch (number &optional (hit 'flop set))
  (if (and set (not (equalp hit 'what)))
      (progn
	(call-next-method number hit)
	(if (boundp 'linked-toggle-item-proto)
	    (send linked-toggle-item-proto :update
		  (send self :slot-value 'identification) number
		  (if (equalp 'on hit) T
		    (if (equalp 'off hit) nil hit)))))
    (call-next-method number 'what))
  )

(defmeth coco-graph-window-proto :test ()
  (if (send self :blocks)
      (print "Not implemented for Block-recursive models")
      (call-next-method))
  )

(defmeth coco-graph-window-proto :factorize (&optional (code 'edges) (set ";"))
  (if (send self :blocks)
      (print "Not implemented for Block-recursive models")
      (call-next-method code set))
  )

(defmeth coco-graph-window-proto :find-log-l ()
  (if (send self :blocks)
      (print "Not implemented for Block-recursive models")
      (call-next-method))
  )

(defmeth coco-graph-window-proto :find-deviance ()
  (if (send self :blocks)
      (print "Not implemented for Block-recursive models")
      (call-next-method))
  )

(defmeth coco-graph-window-proto :compute-deviance ()
  (if (send self :blocks)
      (print "Not implemented for Block-recursive models")
      (call-next-method))
  )

(defmeth coco-graph-window-proto :compute-test
  (&optional (model-1 nil) (model-2 nil))
  (call-next-method model-1 model-2)
;;  (if (send self :blocks)
;;      (print "Not implemented for Block-recursive models")
;;      (call-next-method model-1 model-2))
  )

;;
(defmeth coco-proto :plot-EH-search-result (&optional (class nil))
  (let ((current-number (send self :return-model-number 'current)))
    (send self :current)
    (if (or (not class) (equalp 'all class) (equalp 'accepted class))
	(let ((stop (send self :return-model-number 'current)))
	  (send self :extract 'accepted)
	  (send self :current)			       
	  (do ((i 20 (+ i 50)))
	      ((= stop (send self :return-model-number 'current)) nil)
	      (send self :make-graph
		    :model 'current
		    :location (+ (list i -30) (list 100 100))
		    :title (concatenate 'string (send self :title)
					"  Accepted: "
					(send self :return-model 'current)))
	      (send self :make-current 'previous))))
    (send self :current)
    (if (or (not class) (equalp 'all class) (equalp 'rejected class))
	(let ((stop (send self :return-model-number 'current)))
	  (send self :extract 'rejected)
	  (send self :current)			       
	  (do ((i 20 (+ i 50)))
	      ((= stop (send self :return-model-number 'current)) nil)
	      (send self :make-graph
		    :model 'current
		    :location (+ (list i 300) (list 100 100))
		    :title (concatenate 'string (send self :title)
					"  Rejected: "
					(send self :return-model 'current)))
	      (send self :make-current 'previous))))
    (send self :make-current current-number))
  )

(defmeth coco-graph-window-proto :plot-EH-class
  (class &optional (y-location -30) (title ""))
  (let ((current-number (send self :return-model-number 'current)))
    (send self :current)
    (let ((stop (send self :return-model-number 'current)))
      (send self :extract class)
      (send self :current)			       
      (do ((i 20 (+ i 50)))
	  ((= stop (send self :return-model-number 'current)) nil)
	  (if (kind-of-p self manager-proto)
	      (send self :make-graph
		    :model 'current
		    :location (+ (list i y-location)
				 (send self :location))
		    :title (concatenate
			    'string
			    (send self :title) title
			    (send self :return-model 'current)))
	    (send self :return-child-coco-graph-window
		  :model 'current
		  :location (+ (list i y-location) (send self :location))
		  :title (concatenate
			  'string
			  (send self :title) title
			  (send self :return-model 'current))
		  :sibling T))
	  (send self :make-current 'previous)))
    (send self :make-current current-number))
  )

(defmeth coco-graph-window-proto :plot-EH-search-result (&optional (class nil))
  (let ((current-number (send self :return-model-number 'current)))
    (if (or (not class) (equalp 'all class)
	    (equalp 'classes class) (equalp 'accepted class))
	(send self  :plot-EH-class 'accepted -30 "  Acceptes: "))
    (if (or (not class) (equalp 'all class)
	    (equalp 'duals class) (equalp 'a-dual class))
	(send self  :plot-EH-class 'a-dual    80 "  A-dual: "))
    (if (or (not class) (equalp 'all class)
	    (equalp 'duals class) (equalp 'r-dual class))
	(send self  :plot-EH-class 'r-dual   190 "  R-dual: "))
    (if (or (not class) (equalp 'all class)
	    (equalp 'classes class) (equalp 'rejected class))
	(send self  :plot-EH-class 'rejected 300 "  Rejected: "))
    (send self :make-current current-number))
  )

;;

(defun default-coco-init ()
 (let ((default-file (concatenate 'string *xlisp-cocolib*
                      "/" "Examples/TestGraph.lsp")))
  (if (ok-or-cancel-dialog
       (concatenate 'string 
        "Welcome to Xlisp+CoCo. ~%"
        " ~%"
        "No cocoinit.lsp file found, using default example. ~%"
        "Copy the default example lsp-file ~%"
        default-file "~%"
        "to cocoinit.lsp (.cocoinit.lsp) in the current directory ~%"
        "or (cocoinit.lsp) .cocoinit.lsp in your home-directory, ~%"
        "edit the resulting file, and restart xlisp+coco. ~%"
        "In this ordered list of the four (.)-cocoinit.lsp files ~%"
        "the first found will be loaded when starting xlisp+coco. ~%"
        "Or 'touch cocoinit.lsp' (to make the empty file) and ~%"
        "avoid this dialog box and loading of the default example ~%"
        "when starting xlisp+coco. ~%"
        ))
   (load default-file))))

;;

(require "cocomenu")
(require "cocodialogs")
(require "cocooptions")
(require "cocokey")
(require "cocotest")
(require "cocomanager")
(require "cococontrols")
(require "graphtotex")
(require "cocodyns")

(provide "cocograph")
