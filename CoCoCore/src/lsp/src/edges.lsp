
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the edge-proto for the drag-graph-proto
;;; and adds the basic methods to this proto


;; Induce a circle:

;; (require "draggraph")


(defproto edge-proto
  '(vertices width test dashed type ;;; NAME STATUS INDEX LABEL
	     label-position fix-label-position label-arrow)
  '(instances-edges) (list *object*))

(send edge-proto :documentation 'proto "Edge prototype based on *object*")

(defmeth edge-proto :isnew (vertices &key (type nil))
  (send self :slot-value 'vertices              vertices )
  (send self :slot-value 'type                  type     )
  #|
  (send self :slot-value 'NAME                  name     )
  (send self :slot-value 'INDEX                 index    )
  (send self :slot-value 'STATUS                T        )
  (send self :slot-value 'LABEL                 nil      )
|#
  (send self :slot-value 'width                 nil      )
  (send self :slot-value 'dashed                (if type type nil))
  (send self :slot-value 'test                  nil      )
  (send self :slot-value 'label-position        nil      )
  (send self :slot-value 'fix-label-position    nil      )
  (send self :slot-value 'label-arrow           nil      )
  (send edge-proto :slot-value 'instances-edges
	(cons self (slot-value 'instances-edges)))
  )

(defun return-edge-list (edge-list vertices &key (type nil))
  (list
   (mapcar #'(lambda (i j)
	       (send edge-proto :new
		     (list (nth (car i) vertices)
			   (nth (cadr i) vertices)) :type type))
	   edge-list
	   (iseq (length edge-list))))
  )

(defmeth drag-graph-proto :edges (&optional (val nil set))
  (if set (setf (car (slot-value 'edges)) val))
  (car (slot-value 'edges))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth edge-proto :vertices (&optional (val nil set))
  (if set (setf (slot-value 'vertices) val)) (slot-value 'vertices)
  )

(defmeth drag-graph-proto :remove-tests ()
  (dolist (edge (send self :edges))
	  (send edge :slot-value 'width nil)
	  (send edge :test nil)
	  ;;; (send edge :label nil)
	  (send edge :slot-value 'label-position (list nil nil nil))
	  (send edge :fix-label-position nil)
	  (send edge :label-arrow nil)
	  )
  )

(defmeth drag-graph-proto :vertex-pair-in-edge-list (p1 p2 edges)
  (if (objectp (car edges))
      (let ((result nil)
	    (v1 (nth p1 (send self :vertices)))
	    (v2 (nth p2 (send self :vertices))))
	(do ((i edges (cdr i)))
	    ((or result (not i)) nil)
	    (let ((vertices (send (car i) :vertices)))
	      (setf
	       result
	       (or (and (eq v1 (car vertices)) (eq v2 (cadr vertices)))
		   (and (eq v2 (car vertices)) (eq v1 (cadr vertices)))))))
	result)
    (let ((result nil))
      (do ((i edges (cdr i)))
	  ((or result (not i)) nil)
	  (setf result (or (and (eq p1 (caar i)) (eq p2 (cadar i)))
			   (and (eq p2 (caar i)) (eq p1 (cadar i))))))
      result)
    )
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth edge-proto :edge-to-string ()
  (let* ((vertices (send self :vertices))
	 (u (cadr vertices)))
  (concatenate 'string
	       (if (send u :is-generator)
		   (send u :name)
		 (to-string (mapcar #'(lambda (vertex)
					(send vertex :name))
				    (send self :vertices)) ""))
	       (list #\;)))
  )

(defmeth drag-graph-proto :edge-to-string (edge)
  (concatenate 'string
	       (to-string (mapcar #'(lambda (i)
				      (send (send self :vertex i) :name))
				  edge) "")
	       (list #\;))
  )

(defmeth drag-graph-proto :vertex-lists-to-string (edges)
  (if edges
      (let ((result
	     (concatenate
	      'string
	      (to-string (mapcar #'(lambda (i)
				     (send (send self :vertex i) :name))
				 (car edges)) "")
	      (list #\;))))
	(dolist (edge (cdr edges) result)
		(setf result
		      (concatenate
		       'string
		       (to-string (mapcar #'(lambda (i)
					      (send (send self :vertex i)
						    :name))
					  edge) "")
		       (list #\,)
		       result)))))
  )

(defmeth drag-graph-proto :edge-list-to-string (edges)
  (if edges
      (let ((result
	     (concatenate
	      'string
	      (to-string (mapcar #'(lambda (i)
				     (send i :name))
				 (send (car edges) :vertices)) "")
	      (list #\;))))
	(dolist (edge (cdr edges) result)
		(setf result
		      (concatenate
		       'string
		       (to-string (mapcar #'(lambda (i) (send i :name))
					  (send edge :vertices)) "")
		       (list #\,)
		       result)))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#|
(defmeth edge-proto :NAME (&optional (val nil set))
  (if set (setf (slot-value 'name) val)) (slot-value 'name)
  )

(defmeth edge-proto :status (&optional (val nil set))
  (if set (setf (slot-value 'status) val)) (slot-value 'status)
  )

(defmeth drag-graph-proto :edge-status
  (edge-key &optional (val nil set) &key (redraw nil))
  (let ((edge (send self :edge edge-key)))
    (if (and edge set)
	(send edge :status val :redraw redraw)
      (send edge :status :redraw redraw)))
  )
|#

;;; (defmeth edge-proto :index (&optional (val nil set))
;;;   (if set (setf (slot-value 'index) val)) (slot-value 'index)
;;;   )

(defmeth edge-proto :type (&optional (val nil set))
  (if set (setf (slot-value 'type) val)) (slot-value 'type)
  )

(defmeth drag-graph-proto :edge-index (vertex-pair)
  (if (numberp vertex-pair)
      vertex-pair
    (let* ((vertex-pair (if (stringp vertex-pair)
			    (send self :string-to-vertices vertex-pair)
			  vertex-pair))
	   (u (if (numberp (car vertex-pair))
		  (nth (car vertex-pair) (send self :vertices))
		(car vertex-pair)))
	   (v (if (numberp (cadr vertex-pair))
		  (nth (cadr vertex-pair) (send self :vertices))
		(cadr vertex-pair))))
      (if (and u v)
	  (flet
	   ((ok (u v vertices)
		(or (and (eq u (car vertices)) (eq v (cadr vertices)))
		    (and (eq v (car vertices)) (eq u (cadr vertices))))))
	   (do ((n 0 (1+ n))
		(i (send self :edges) (cdr i)))
	       ((or (not i) (ok u v (send (car i) :vertices)))
		(if (if (car i) (ok u v (send (car i) :vertices))) n)))))))
  )

(defmeth drag-graph-proto :edge (edge-key)
  (if (objectp edge-key)
      edge-key
    (let ((edge-index (send self :edge-index edge-key)))
      (if edge-index (nth edge-index (send self :edges)))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth edge-proto :width (&optional (val nil set) &key (redraw nil))
 (if (and set val) (if (not (invalid-real val))
                    (setf (slot-value 'width) val)))
 (if (or (not (slot-value 'width)) (invalid-real (slot-value 'width)))
  nil
  (if (< (slot-value 'width) 10000) (slot-value 'width)))
 )

(defmeth drag-graph-proto :edge-width
  (edge-key &optional (val nil set) &key (redraw nil))
  (let ((edge (send self :edge edge-key)))
    (if (and edge set)
	(send edge :width val :redraw (if redraw self))
      (send edge :width)))
  )

(defmeth drag-graph-proto :set-edge-color (edge)
  (send self :draw-color
	(send self :item-color
	      (if (send edge :dashed) (send edge :dashed)
		(if (or (not (send edge :type))
		        (equalp 'directed (send edge :type)))
		    (if (send edge :width) 'fitted 'not-fitted)
		  'sibling))))
  )

(defmeth edge-proto :test (&optional (val nil set) &key (redraw nil))
  (if set (setf (slot-value 'test) val)) (slot-value 'test)
  )

(defmeth drag-graph-proto :edge-test
  (edge-key &optional (val nil set) &key (redraw nil))
  (let ((edge (send self :edge edge-key)))
    (if (and edge set)
	(send edge :test val  :redraw (if redraw self))
      (send edge :test)))
  )

(defmeth edge-proto :test-ok ()
  (if (slot-value 'test) (objectp (slot-value 'test)))
  )

(defmeth edge-proto :dashed (&optional (val nil set) &key (redraw nil))
  (if set (setf (slot-value 'dashed) val)) (slot-value 'dashed)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth edge-proto :label (&optional (val nil set) &key (redraw nil))
  (if set
      (if (not val)
	  (setf (slot-value 'label-position) (list nil nil)))
    (car (send self :slot-value 'label-position)))
  )

(defmeth drag-graph-proto :edge-label
  (edge-key &optional (val nil set) &key (redraw nil))
  (let ((edge (send self :edge edge-key)))
    (if (and edge set)
	(send edge :label val :redraw (if redraw self))
      (send edge :label)))
  )

(defmeth edge-proto :center ()
  (/ (+ (send (cadr (send self :vertices)) :position)
	(send (car (send self :vertices)) :position))
     2)
  )

(defmeth edge-proto :label-position (&optional (position nil set)
					       &key (redraw nil))
  (if (and set (not (2-3-list position))) (error "Invalid argument"))
  (let ((result
	 (let ((center (if (slot-value 'fix-label-position)
			   (list 0 0 0) (send self :center))))
	   (if set
	       (slot-value 'label-position (- (2-to-3-list position) center))
	     (+ (slot-value 'label-position) center)))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :edge-label-position
  (edge-key &optional (position nil) &key (redraw nil))
  (let ((edge (send self :edge edge-key)))
    (send edge :label-position position  :redraw (if redraw self)))
  )

(defmeth edge-proto :label-offset (&optional (position nil set)
					     &key (redraw nil))
  (if (and position (not (2-3-list position))) (error "Invalid argument"))
  (let ((result
	 (if set
	     (slot-value 'label-position (2-to-3-list position))
	   (slot-value 'label-position))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :edge-label-offset
  (edge-key &optional (position nil) &key (redraw nil))
  (let ((edge (send self :edge edge-key)))
    (if (and edge set)
	(send edge :label-offset val :redraw (if redraw self))
      (send edge :label-offset)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth edge-proto :fix-label-position (&optional (val nil set)
						   &key (redraw nil))
  (let ((label-position (slot-value 'label-position)))
    (if (and set label-position)
	(let ((old (slot-value 'fix-label-position))
	      (m (send self :center)))
	  (if (and old (not val))
	      (slot-value 'label-position (- label-position m))
	    (if (and val (not old))
		(slot-value 'label-position (+ label-position m))))
	  (slot-value 'fix-label-position val))
      (slot-value 'fix-label-position)))
  )

(defmeth drag-graph-proto :fix-edge-label-position
  (edge-key &optional (val nil set) &key (redraw nil))
  (let ((edge (send self :edge edge-key)))
    (if set
	(send edge :fix-label-position val :redraw (if redraw self))
      (send edge :fix-label-position)))
  )

(defmeth edge-proto :label-arrow (&optional (val nil set)
					    &key (redraw nil))
  (if set (setf (slot-value 'label-arrow) val)) (slot-value 'label-arrow)
  )

(defmeth drag-graph-proto :edge-label-arrow
  (edge-key &optional (val nil set) &key (redraw nil))
  (let* ((edge (send self :edge edge-key))
	 (label (send edge :label-position)))
    (if (and label set)
	(send edge :label-arrow val :redraw (if redraw self))
      (send edge :label-arrow)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :set-edge-label
  (edge &key (center nil) (follow T) (decomposable-mode T))
  (send self :push-edge-undo edge T)
  (if (or T (not (send edge :test-ok)))
      (send self :test-edge edge
	    :follow follow :decomposable-mode decomposable-mode))
  ;;   ^- Force recomputation for, e.g., change of ordinal model.
  (if (screen-has-color) (send self :set-edge-color edge))
  (if (send edge :test-ok)
      (let* ((vertex (cadr (send edge :vertices)))
	     (object (if (send vertex :is-generator) vertex edge))
	     (label (send self :format-p-value
			  (send (send edge :test) :select-p-value))))
	(send self :draw-string label (send self :x) (send self :y))
	(if (send vertex :is-generator)
	    (progn
	      (send edge :width 1)
	      (send vertex :label 
		    (concatenate 'string (send vertex :name) ": " label))))
	(send object :slot-value 'fix-label-position nil)
	(send object :slot-value 'label-arrow nil)
	(send object :slot-value 'label-position
	      (if center (list 0 0 0)
		(send self :find-move
		      (send self :from-x-pixel (send self :x))
		      (send self :from-y-pixel (send self :y))
		       (if (send vertex :is-generator)
			   (send vertex :position)
			 (send edge :center)))))
	label)
    (let ((label (send edge :test)))
      (if (screen-has-color)
	  (send self :draw-color (send self :item-color label)))
      (send edge :slot-value 'dashed label)
      (print label)
      (send self :draw-string
	    (case label
		  ('non-decomposable
		   "Non-decomposable")	; 'brown
		  ('no-degrees-of-freedom
		   "No degrees of freedom") ; 'dark-violet
		  ('not-submodel-of-base
		   "Not submodel of base") ; 'dark-violet
		  ('not-same-coco-object
		   "Not same CoCo object") ; 'wellow
		  (t
		   "Error")		; 'black
		  )
	    (send self :x) (send self :y))))
  )

(defmeth drag-graph-proto :set-edge-label-nth
  (p &key (center nil) (follow T) (decomposable-mode T))
  (send self :set-edge-label (nth p (send self :edges))
	:center center :follow follow :decomposable-mode decomposable-mode)
  )

(defmeth edge-proto :set-label
  (graph &key (center nil) (follow T) (decomposable-mode T))
  (send graph :set-edge-label self
	:center center :follow follow :decomposable-mode decomposable-mode)
  )

(defmeth drag-graph-proto :drop-edge-label (edge)
  (send self :push-edge-undo edge T)
  (send edge :test nil)
  (send edge :label nil)
  (send edge :width nil)
  (send edge :dashed nil)
  (send self :redraw-graph)
  )

(defmeth drag-graph-proto :drop-edge-label-nth ()
  (send self :drop-edge-label (nth p (send self :edges)))
  )

(defmeth edge-proto :drop-label (graph)
  (send graph :drop-edge-label self)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :draw-edge-label (edge)
  (if (and (send edge :test-ok) (send edge :label))
      (let ((vertices (send edge :vertices)))
	(if (and (not (equalp 6 (send (cadr vertices) :type)))
		 (not (equalp 6 (send (car vertices) :type))))
	    (let* ((m (send edge :center))
		   (x (send self :project
			    (+ (if (send edge :fix-label-position)
				   (list 0 0 0) m)
			       (send edge :slot-value 'label-position)))))
	      (if (send edge :label-arrow)
		  (let ((y (send self :project m)))
		    (send self :line-width 1)
		    (send self :line-type 'dashed)
		    (if (screen-has-color) (send self :set-edge-color edge))
		    (send self :draw-line
			  (send self :to-x-pixel (car x))
			  (send self :to-y-pixel (cadr x))
			  (send self :to-x-pixel (car y))
			  (send self :to-y-pixel (cadr y)))))
	      (if (screen-has-color) (send self :set-edge-color edge))
	      (send self :draw-string
		    (send self :format-p-value
			  (send (send edge :test) :select-p-value))
		    (send self :to-x-pixel (car x))
		    (send self :to-y-pixel (cadr x)))))))
  )

(defmeth drag-graph-proto :draw-edge-label-nth (p)
  (let ((edge (nth p (send self :edges))))
    (send self :draw-edge-label edge))
  )

(defmeth drag-graph-proto :draw-thick-edge
  (x y z a b c width radius &optional (2u 0) (2v 0))
  (if (> width 0)
      (let* ((length (sqrt (+ (^ (- x a) 2) (^ (- y b) 2))))
	     (dx (/ (- x a) length))
	     (dy (/ (- y b) length))
	     (tt (/ (* radius (if (send self :transformation) 
				  (/ (+ 100 c) 100) 1)) 4))
	     (u (* tt width dx))
	     (v (* tt width dy))
	     (p (- (+ a (* tt dx)) 2u))
	     (q (- (+ b (* tt dy)) 2v))
	     (tt (/ (* radius (if (send self :transformation) 
				  (/ (+ 100 z) 100) 1)) 4))
	     (m (- (* tt width dx)))
	     (n (- (* tt width dy)))
	     (r (- x (* tt dx)))
	     (s (- y (* tt dy))))
	(send self :paint-poly
	      (list (list (send self :to-x-pixel (+ p u v))
			  (send self :to-y-pixel (+ q v (- u))))
		    (list (send self :to-x-pixel p)
			  (send self :to-y-pixel q))
		    (list (send self :to-x-pixel (+ p u (- v)))
			  (send self :to-y-pixel (+ q v u)))
		    (list (send self :to-x-pixel (+ r m n))
			  (send self :to-y-pixel (+ s n (- m))))
		    (list (send self :to-x-pixel r)
			  (send self :to-y-pixel s))
		    (list (send self :to-x-pixel (+ r m (- n)))
			  (send self :to-y-pixel (+ s n m)))))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :draw-arrow
  (x y z a b c width fitted radius)
  (let ((length (sqrt (+ (^ (- x a) 2) (^ (- y b) 2)))))
    (if (< 0 length)
	(let* ((tt (/ (* radius (if (send self :transformation) 
				    (/ (+ 100 c) 100) 1)) 4))
	       (dx (/ (- a x) length))
	       (dy (/ (- b y) length))
	       (u (* tt (if fitted (* 4 width) 1) dx))
	       (v (* tt (if fitted (* 4 width) 1) dy))
	       (p (- a (* tt dx)))
	       (q (- b (* tt dy))))
	  (send self :paint-poly
		(list (list (send self :to-x-pixel (- p (* 2 u)))
			    (send self :to-y-pixel (- q (* 2 v))))
		      (list (send self :to-x-pixel (- p (* 4 u) (* 2 (- v))))
			    (send self :to-y-pixel (- q (* 4 v) (* 2 u))))
		      (list (send self :to-x-pixel p)
			    (send self :to-y-pixel q))
		      (list (send self :to-x-pixel (- p (* 4 u) (* 2 v)))
			    (send self :to-y-pixel (- q (* 4 v)
						      (* 2 (- u)))))))
	  (if fitted
	      (send self :draw-thick-edge x y z a b c
		    width radius (* 2 u) (* 2 v))
	    (send self :draw-line
		  (send self :to-x-pixel x)
		  (send self :to-y-pixel y)
		  (send self :to-x-pixel a)
		  (send self :to-y-pixel b))))))
  )

(defmeth drag-graph-proto :draw-edge (edge &optional (radius 4))
  (let ((vertices (send edge :vertices)))
    (if (and (not (equalp 6 (send (cadr vertices) :type)))
	     (not (equalp 6 (send (car vertices) :type))))
	(let ((width (send edge :width)))
	  (if (screen-has-color) (send self :set-edge-color edge))
	  (send self :line-width (if width width 1))
	  (send self :line-type (if (send edge :dashed) 'dashed 'solid))
	  (let ((x1 (send self :project (send (car vertices) :position)))
		(x2 (send self :project (send (cadr vertices) :position))))
	    (if (and (not (send (cadr vertices) :is-generator))
		     (< (send (car vertices) :stratum)
			(send (cadr vertices) :stratum)))
		(send self :draw-arrow
		      (car x1) (cadr x1) (caddr x1)
		      (car x2) (cadr x2) (caddr x2)
		      (if (and width (< 1 width)) (/ width 16) (/ 1 1))
		      (and width (< 1 width)) radius)
	      (if (and (not (send (cadr vertices) :is-generator))
		       (< (send (cadr vertices) :stratum)
			  (send (car vertices) :stratum)))
		  (send self :draw-arrow
			(car x2) (cadr x2) (caddr x2)
			(car x1) (cadr x1) (caddr x1)
			(if (and width (< 1 width)) (/ width 16) (/ 1 1))
			(and width (< 1 width)) radius)
		(if (and width (< 1 width))
		    (send self :draw-thick-edge
			  (car x1) (cadr x1) (caddr x1)
			  (car x2) (cadr x2) (caddr x2) (/ width 16) radius)
		  (if (or (not width) (< 0 width))
		      (send self :draw-line
			    (send self :to-x-pixel (car x1))
			    (send self :to-y-pixel (cadr x1))
			    (send self :to-x-pixel (car x2))
			    (send self :to-y-pixel (cadr x2)))))))))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :draw-edges (radius)
  (dolist (edge (send self :edges) NIL) (send self :draw-edge edge radius))
  (dolist (edge (send self :edges) NIL) (send self :draw-edge-label edge))
  )

(defmeth edge-proto :draw-label (graph)
  (send graph :draw-edge-label self)
  )

(defmeth edge-proto :draw (graph radius)
  (send graph :draw-edge-label self)
  (send graph :draw-edge self radius)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun edge-distance (x y a b c d)
  (let ((cross-product (+ (* (- a x) (- c x)) (* (- b y) (- d y))))
	(length (* (sqrt (+ (^ (- a x) 2) (^ (- b y) 2)))
		   (sqrt (+ (^ (- c x) 2) (^ (- d y) 2))))))
    (if (= 0 length)
	-99999999
      (if (< (/ cross-product length) (- 0 (/ 1 (sqrt 2))))
	  (let ((distance (+ (^ (- (/ (+ a c) 2) x) 2)
			     (^ (- (/ (+ b d) 2) y) 2)))
		(area (^ (- (* (- a x) (- d b)) (* (- b y) (- c a))) 2)))
	    (if (or (= 0 distance) (= 0 area))
		-99999999
	      (- 0 (/ 1 (* distance area)))))
	0)))
  )

(defmeth edge-proto :potential (x y graph)
  (let* ((pos1 (send graph :project
		     (send (car (send self :vertices)) :position)))
	 (pos2 (send graph :project
		     (send (cadr (send self :vertices)) :position))))
    (edge-distance x y (car pos1) (cadr pos1) (car pos2) (cadr pos2)))
  )

(defmeth drag-graph-proto :return-closest-edge (x y)
  (let ((edge nil)
	(potential 0))
    (mapcar #'(lambda (i)
		(let ((z (send i :potential x y self)))
		  (if (< z potential)
		      (progn (setf edge i) (setf potential z)))))
	    (send self :edges))
    edge)
  )

(defmeth drag-graph-proto :return-closest-edge-in-canvas (x y)
  (send self :return-closest-edge
	(send self :from-x-pixel x)
	(send self :from-y-pixel y))
  )

(defmeth drag-graph-proto :return-closest-edge-label-with-potential (x y)
  (let ((edge nil)
	(potential 0))
    (mapcar #'(lambda (i)
		(if (send i :label)
		    (let ((x (point-potential
			      x y
			      (send self :project (send i :label-position)))))
		      (if (< x potential)
			  (progn (setf edge i) (setf potential x))))))
	    (send self :edges))
    (list potential edge))
  )

;;

(provide "edges")
