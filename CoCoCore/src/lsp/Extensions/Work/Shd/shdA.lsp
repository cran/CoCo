
;;; Dear Søren,

;;; Use this as a ``patch'' to my code.  return-coco-graph-window and
;;; :return-child-coco-graph-window has been added the argument `proto-type'.
;;; If this argument is used (it should be a prototype made by extending the
;;; coco-graph-window-proto, then a graph-window of the `proto-type' is made.
;;; The slots inherited from coco-graph-window-proto are handled by my code,
;;; and all messages known to coco-graph-window-proto will of cause also be
;;; known to the extended prototype.

;;; / jhb, 20. january 2000


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
    (send x :slot-value 'static T)
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

(defmeth coco-graph-window-proto :return-child-coco-graph-window
  (&key (model nil) (copy-slots nil) (copy-vertices T)
	(parant nil) (child nil) (sibling nil)
	(location nil) (offset (list 50 -60)) (size nil) (title nil)
        (proto-type nil))
  (let* ((nr (if model
		 (if (stringp model)
		     (progn (send self :read-model model)
			    (send self :return-model-number 'last))
		   (if (numberp model) model
		     (send self :return-model-number model)))
	       (send self :return-model-number 'current)))
	 (vertices (if copy-vertices
		       (slot-value 'vertices)
		     (return-default-vertices (send self :return-names))))
	 (graph
	  (return-coco-graph-window
	   (slot-value 'identification) nr vertices
	   (return-edge-list (send self :return-edge-list nr) (car vertices))
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
  )

#|

;;; A later version for Xlisp+Mips:

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
|#
