
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the vertex-proto for the drag-graph-proto
;;; and adds the basic methods to this proto


;; Induce a circle:

;; (require "draggraph")


(defproto vertex-proto
  '(name label type stratum ;;; STATUS
	 index position label-position fix-label-position label-arrow)
  '(instances-vertices) (list *object*))

(send vertex-proto :documentation 'proto "Vertex prototype based on *object*")

(defmeth vertex-proto :isnew (name label type stratum index position)
  (send self :slot-value 'name            name         )
  (send self :slot-value 'label           label        )
  (send self :slot-value 'type            type         )
  (send self :slot-value 'stratum         stratum      )
;;;  (send self :slot-value 'status          nil          )
  (send self :slot-value 'index           index        )
  (send self :slot-value 'position        position     )
  (send self :slot-value 'label-position  (list nil nil 0))
  (send self :slot-value 'fix-label-position nil)
  (send self :slot-value 'label-arrow nil)
  (send vertex-proto :slot-value 'instances-vertices
	(cons self (slot-value 'instances-vertices)))
  self
  )

(defun return-default-vertices (names &optional (delta 0) (line nil))
  (list (let ((n (length names)))
	  (mapcar
	   #'(lambda (i name)
	       (send vertex-proto :new
		     ;; Name:
		     (car name)
		     ;; Vertex-label:
		     (cadr name)
		     ;; Vertex-type (continuous, discrete, ordinal, etc.):
		     (caddr name)
		     ;; Stratum:
		     (cadddr name)
		     ;; Index:
		     (+ i delta)
		     ;; Variable position (x,y,z):
		     (if line
			 (list (+ 0 -45)
			       (+ 0 (* 40 (- 1 (* 2 (/ (+ i .5) n))))) 0)
		       (list (+ 0 (* 40 (cos (* 2 (* pi (/ i n))))))
			     (+ 0 (* 40 (sin (* 2 (* pi (/ i n)))))) 0))
		     ))
	   (iseq (length names)) names)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth vertex-proto :index (&optional (val nil set))
  (if set (setf (slot-value 'index) val)) (slot-value 'index)
  )

(defmeth drag-graph-proto :vertex-index (variable-name)
  (if (numberp variable-name)
      variable-name
    (if (characterp variable-name)
	(send self :vertex-index (concatenate 'string (list variable-name)))
      (do ((n 0 (1+ n))
	   (i (send self :vertices) (cdr i)))
	  ((or (not i) (equal variable-name (send (car i) :name)))
	   (if (if (car i) (equal variable-name (send (car i) :name))) n)))))
  )

(defmeth drag-graph-proto :vertex (variable-name)
  (if (objectp variable-name)
      variable-name
    (if (numberp variable-name)
	(nth variable-name (send self :vertices))
      (if (characterp variable-name)
	  (send self :vertex (concatenate 'string (list variable-name)))
	(do ((i (send self :vertices) (cdr i)))
	    ((if (car i) (equal variable-name (send (car i) :name)) T)
	     (if (if (car i) (equal variable-name (send (car i) :name)))
		 (car i)))))))
  )

;; Updated in "factorgraph.lsp":

(defmeth drag-graph-proto :update-vertices-of-edges (&optional (vertices nil))
  (let ((all-vertices (send self :vertices)))
    (dolist (edge (send self :edges) NIL) 
      (if (not (send (cadr (send edge :vertices)) :is-generator))
	  (send edge :vertices
		(mapcar #'(lambda (vertex)
			    (nth (send vertex :index) all-vertices))
			(send edge :vertices))))))
  )

(defmeth drag-graph-proto :vertices (&optional (val nil))
  (if val (progn (setf (car (slot-value 'vertices)) val)
		 (send self :update-vertices-of-edges val)))
  (car (slot-value 'vertices))
  )

(defmeth drag-graph-proto :names (&optional (val nil))
  (if val
      (mapcar #'(lambda (i j) (send i :name j)) (send self :vertices) val)
    (mapcar #'(lambda (i) (send i :name)) (send self :vertices)))
  )

(defmeth drag-graph-proto :positions (&optional (val nil))
  (if val
      (mapcar #'(lambda (i j) (send i :position j))
	      (send self :vertices) val)
    (mapcar #'(lambda (i) (send i :position)) (send self :vertices)))
  )

(defmeth drag-graph-proto :label-positions (&optional (val nil))
  (if val
      (mapcar #'(lambda (i j) (send i :label-position j))
	      (send self :vertices) val)
    (mapcar #'(lambda (i) (send i :label-position)) (send self :vertices)))
  )

(defmeth drag-graph-proto :labels (&optional (val nil))
  (if val
      (mapcar #'(lambda (i j) (send i :label j))
	      (send self :vertices) val)
    (mapcar #'(lambda (i) (send i :label)) (send self :vertices)))
  )

(defmeth drag-graph-proto :xyz ()
  (transpose (send self :positions))
  )

(defmeth drag-graph-proto :point-to-string (point)
  (concatenate 'string
	       (send (if (objectp point) point
		       (nth point (send self :vertices))) :name) (list #\;))
  )

(defmeth drag-graph-proto :position-to-name (p1 p2)
  (concatenate 'string
	       (send (nth p1 (send self :vertices)) :name)
	       (send (nth p2 (send self :vertices)) :name) (list #\;))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth vertex-proto :name (&optional (name nil set) &key (redraw nil))
  (let ((result (if set (setf (slot-value 'name) name) (slot-value 'name))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :vertex-name
  (variable-name &optional (name nil set) &key (redraw nil))
  (let ((vertex (send self :vertex variable-name)))
    (if vertex
	(if set (send vertex :name name :redraw (if redraw self))
	  (send vertex :name))))
  )

(defmeth vertex-proto :label (&optional (label nil set) &key (redraw nil))
  (let ((result (if set (setf (slot-value 'label) label) (slot-value 'label))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :vertex-label
  (variable-name &optional (label nil set) &key (redraw nil))
  (let ((vertex (send self :vertex variable-name)))
    (if vertex
	(if set (send vertex :label label :redraw (if redraw self))
	  (send vertex :label))))
  )

(defmeth vertex-proto :type (&optional (type nil set) &key (redraw nil))
  (let ((result
	 (if set (setf (slot-value 'type) 
		       (case type ('continuous  0)  ('discrete    1) 
			     ('ordinal     2) (T type)))
	   (case (slot-value 'type) (0 'continuous) (1 'discrete) 
		 (2 'ordinal) (T (slot-value 'type))))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :vertex-type
  (variable-name &optional (type nil set) &key (redraw nil))
  (let ((vertex (send self :vertex variable-name)))
    (if vertex
	(if set (send vertex :type type :redraw (if redraw self))
	  (send vertex :type))))
  )

(defmeth vertex-proto :stratum (&optional (val nil set) &key (redraw nil))
  (if set (setf (slot-value 'stratum) val))
  (slot-value 'stratum)
  )

(defmeth drag-graph-proto :vertex-stratum
  (variable-name &optional (block-key nil set) &key (redraw nil))
  (let* ((vertex (send self :vertex variable-name))
	 (result
	  (if set
	      (let ((block (send self :block block-key)))
		(if (and (send block :visual)
			 (not (in-block (send vertex :position)
					(send block :position))))
		    (send vertex :position
			  (/ (apply #'+ (send block :block-position)) 2)))
		(send vertex :stratum (send block :stratum)))
	    (send vertex :stratum))))
    (if (and set redraw) (send self :redraw-graph))
    result)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth vertex-proto :update-stratum (&optional (val nil set))
  )

;;; (defmeth vertex-proto :status (&optional (val nil set))
;;;   ;; ``Use variable'' should not be shared among graphs.
;;;   (if set (setf (slot-value 'status) val)) (slot-value 'status)
;;;   )

(defmeth vertex-proto :position (&optional (position nil set)
					   &key (redraw nil) (graph nil))
  (if (and set (not (2-3-list position))) (error "Invalid argument"))
  (if (and set graph) (send graph :push-vertex-undo self))
  (let ((result (if set (setf (slot-value 'position) (2-to-3-list position))
		  (slot-value 'position))))
    (if position (send self :update-stratum))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :vertex-position
  (variable-name &optional (position nil set) &key (redraw nil))
  (let ((vertex (send self :vertex variable-name)))
    (if vertex
	(if set (send vertex :position position
		      :redraw (if redraw self) :graph self)
	  (send vertex :position))))
  )

(defmeth vertex-proto :label-position (&optional (position nil set)
						 &key (redraw nil) (graph nil))
  (if (and set (not (2-3-list position))) (error "Invalid argument"))
  (if (and set graph) (send graph :push-vertex-undo self))
  (let ((result
	 (if set (setf (slot-value 'label-position) (2-to-3-list position))
	   (slot-value 'label-position))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :vertex-label-position
  (variable-name &optional (label-position nil set) &key (redraw nil))
  (let ((vertex (send self :vertex variable-name)))
    (if vertex
	(if set (send vertex :label-position label-position
		      :redraw (if redraw self) :graph self)
	  (send vertex :label-position))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth vertex-proto :fix-label-position (&optional (val nil set)
						     &key (redraw nil))
  (if set
      (progn
	(if (and (send self :fix-label-position) (not val))
	    (send self :label-position
		  (- (send self :label-position) (send self :position)))
	  (if (and val (not (send self :fix-label-position)))
	      (send self :label-position
		    (if (car (send self :label-position))
			(+ (send self :label-position)
			   (send self :position))
		      (rescale-procent 1.10 (send self :position))))))
	(setf (slot-value 'fix-label-position) val))
    (slot-value 'fix-label-position))
  )

(defmeth drag-graph-proto :fix-vertex-label-position
  (variable-name &optional (val nil set) &key (redraw nil))
  (let ((vertex (send self :vertex variable-name)))
    (if vertex
	(if set (send vertex :fix-label-position val :redraw (if redraw self))
	  (send vertex :fix-label-position))))
  )

(defmeth vertex-proto :label-arrow (&optional (val nil set) &key (redraw nil))
  (let ((result (if set (setf (slot-value 'label-arrow) val)
		  (slot-value 'label-arrow))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(defmeth drag-graph-proto :vertex-label-arrow
  (variable-name &optional (val nil set) &key (redraw nil))
  (let ((vertex (send self :vertex variable-name)))
    (if vertex
	(if set (send vertex :label-arrow val :redraw (if redraw self))
	  (send vertex :label-arrow))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth vertex-proto :rescale-position
  (&optional (scale 1) &key (redraw nil) (graph nil))
  (send self :position (* scale (send self :position))
	:redraw redraw :graph graph)
  )

(defmeth drag-graph-proto :rescale-vertex-positions
  (&key (vertex-name nil) (scale 1) (redraw nil))
  (let ((result
	 (if (and vertex-name (numberp vertex-name))
	     (send (send self :vertex vertex-name) :rescale-position
		   scale :redraw (if redraw self) :graph self)
	   (progn
	     (send self :push-block-points-undo (if (send self :blocks) T) T)
	     (do ((vertex-list (send self :vertices) (cdr vertex-list)))
		 ((or (not vertex-list)) T)
		 (if (or (not vertex-name)
			 (equal vertex-name (send (car vertex-list) :name)))
		     (send (car vertex-list) :rescale-position
			   scale ;;; :redraw nil :graph self
			   )
		   ))))))
    (if redraw (send self :redraw-graph))
    result)
  )

(defmeth vertex-proto :adjust-to-grid (&key (delta 1)
					    (redraw nil) (graph nil))
  (send self :position (* (round (/ (send self :position) delta)) delta)
	:redraw redraw :graph graph)
  (if (car (send self :label-position))
      (send self :label-position
	    (* (round (/ (send self :label-position) delta)) delta)
	    :redraw redraw :graph nil))
  (list (send self :position) (send self :label-position))
  )

(defmeth drag-graph-proto :adjust-vertices-to-grid
  (&key (vertex-name nil) (delta 1) (redraw nil))
  (let ((result
	 (if (and vertex-name (numberp vertex-name))
	     (send (send self :vertex vertex-name) :adjust-to-grid
		   :delta delta :redraw redraw :graph self)
	   (progn
	     (send self :push-block-points-undo (if (send self :blocks) T) T)
	     (do ((vertex-list (send self :vertices) (cdr vertex-list)))
		 ((or (not vertex-list)) T)
		 (if (or (not vertex-name)
			 (equal vertex-name (send (car vertex-list) :name)))
		     (send (car vertex-list) :adjust-to-grid
			   :delta delta ;;; :redraw nil :graph self
			   )))))))
    (if redraw (send self :redraw-graph))
    result)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


(defmeth drag-graph-proto :draw-vertex (type x y z radius width)
  (let ((radius (if (send self :transformation)
		    (round (* radius (/ (+ 100 z) 100))) radius)))
    (if (or (eq 0 type) (eq 'continuous type)
	    (eq 1 type) (eq 'discrete type))
	(send self :paint-oval (- x radius) (- y radius)
	      (* 2 radius) (* 2 radius)))
    (if (or (eq 0 type) (eq 'continuous type))
	(send self :erase-oval (- x (- radius width)) (- y (- radius width))
	      (* 2 (- radius width)) (* 2 (- radius width))))
    (if (or (eq 2 type) (eq 'ordinal type)
	    (eq 3 type))
	(send self :paint-rect (- x radius) (- y radius)
	      (* 2 radius) (* 2 radius)))
    (if (eq 3 type)
	(send self :erase-rect (- x (- radius width)) (- y (- radius width))
	      (* 2 (- radius width)) (* 2 (- radius width))))
    (if (eq 4 type)
	(send self :frame-oval (- x radius) (- y radius)
	      (* 2 radius) (* 2 radius)))
    (if (or (eq 5 type) (eq 'generator type))
	(send self :frame-rect (- x radius) (- y radius)
	      (* 2 radius) (* 2 radius)))
    (if (or (eq 6 type) (eq 'gc type))
	(send self :paint-poly (list (list x (+ y radius))
				     (list (- x (round (* radius
							  (* (sqrt 3) 0.5))))
					   (- y (round (* radius 0.5))))
				     (list (+ x (round (* radius
							  (* (sqrt 3) 0.5))))
					   (- y (round (* radius 0.5))))
				     (list x (+ y radius)))))
    (if (or (eq 7 type) (eq 'linear-generator type))
	(send self :frame-poly (list (list x (- y radius))
				     (list (- x (round (* radius
							  (* (sqrt 3) 0.5))))
					   (+ y (round (* radius 0.5))))
				     (list (+ x (round (* radius
							  (* (sqrt 3) 0.5))))
					   (+ y (round (* radius 0.5))))
				     (list x (- y radius)))))
    (if (or (eq 8 type) (eq 'discrete-generator type))
	(send self :paint-poly (list (list (+ x radius) y)
				     (list (- x (round (* radius 0.5)))
					   (+ y (round (* radius
							  (* (sqrt 3) 0.5)))))
				     (list (- x (round (* radius 0.5)))
					   (- y (round (* radius
							  (* (sqrt 3) 0.5)))))
				     (list (+ x radius) y))))
    (if (or (eq 9 type) (eq 'quadratic-generator type))
	(send self :frame-poly (list (list (- x radius) y)
				     (list (+ x (round (* radius 0.5)))
					   (+ y (round (* radius
							  (* (sqrt 3) 0.5)))))
				     (list (+ x (round (* radius 0.5)))
					   (- y (round (* radius
							  (* (sqrt 3) 0.5)))))
				     (list (- x radius) y))))
    )
  )

(defmeth drag-graph-proto :draw-vertex-and-label
  (vertex use-variables radius)
  (if (and use-variables (> use-variables 0)
	   (not (equalp 6 (send vertex :type))))
      (let ((position (send vertex :position))
	    (label-position (send vertex :label-position))
	    (label (send vertex :label)))
	(let ((A (send self :project position))
	      (B (send self :project (if (send vertex :fix-label-position)
					 label-position
				       (if (car label-position)
					   (+ position label-position)
					 (rescale-procent 1.10 position))))))
	  (let ((A-x (send self :to-x-pixel (car A)))
		(A-y (send self :to-y-pixel (cadr A)))
		(B-x (send self :to-x-pixel (car B)))
		(B-y (send self :to-y-pixel (cadr B))))
	    (if (screen-has-color)
		(send self :draw-color
		      (send self :item-color 'vertex))) ; 'blue)
	    (send self :draw-vertex
		  (send vertex :type) A-x A-y (caddr A) radius 2)
	    (if (screen-has-color)
		(send self :draw-color
		      (send self :item-color 'vertex-label))) ; 'cyan)
	    (send self :draw-string
		  (string (if label label (send vertex :name))) B-x B-y)
	    (if (send vertex :label-arrow)
		(progn
		  (send self :line-width 1)
		  (send self :line-type 'dashed)
		  (send self :draw-line A-x A-y B-x B-y)))))))
  )

(defmeth vertex-proto :draw (graph radius)
  (send graph :draw-vertex-and-label self T radius)
  )

(defmeth drag-graph-proto :draw-vertices (radius)
  (mapcar #'(lambda (vertex use-variables)
	      (send self :draw-vertex-and-label vertex use-variables radius))
	  (send self :vertices)
	  (slot-value 'use-variables))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#|
(defmeth vertex-proto :potential (x y graph)
  (let* ((point (send graph :project (send self :position)))
	 (distance (+ (^ (- (car point) x) 2) (^ (- (cadr point) y) 2))))
    (if (= 0 distance) -99999999 (- 0 (/ 1 distance))))
  )
|#

(defmeth drag-graph-proto :return-closest-vertex-with-potential (x y)
  (let ((point nil)
	(potential 0))
    (mapcar #'(lambda (i)
		(let ((x (point-potential
			  x y (send self :project (send i :position)))))
		  (if (< x potential)
		      (progn (setf point i) (setf potential x)))))
	    (send self :vertices))
    (list potential point :index))
  )

(defmeth drag-graph-proto :return-closest-vertex-in-canvas (x y)
  (cadr (send self :return-closest-vertex-with-potential
	      (send self :from-x-pixel x)
	      (send self :from-y-pixel y)))
  )

;;

(provide "vertices")
