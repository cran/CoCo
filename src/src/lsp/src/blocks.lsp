
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the block-proto for the drag-graph-proto
;;; and adds the basic methods to this proto


;; Induce a circle:

;; (require "draggraph")


(defproto block-proto
  '(stratum positions visual ;;; NAME TYPE STATUS INDEX
	    label label-position fix-label-position label-arrow)
  '(instances-blocks) (list *object*))

(send block-proto :documentation 'proto "Block prototype based on *object*")

(defmeth block-proto :isnew (stratum index positions label visual)
  (send self :slot-value 'stratum               stratum     )
  (send self :slot-value 'positions             positions   )
;;;  (send self :slot-value 'NAME                  name        )
;;;  (send self :slot-value 'TYPE                  type        )
;;;  (send self :slot-value 'STATUS                T           )
;;;  (send self :slot-value 'INDEX                 index       )
  (send self :slot-value 'visual                visual      )
  (send self :slot-value 'label                 nil         )
  (send self :slot-value 'label-position        nil         )
  (send self :slot-value 'fix-label-position    nil         )
  (send self :slot-value 'label-arrow           nil         )
  (send block-proto :slot-value 'instances-blocks
	(cons self (slot-value 'instances-blocks)))
  )

(defun return-block-list (stratas labels n nanny visual)
  (list
   (mapcar #'(lambda (i label)
	       (send block-proto :new (if nanny (- n i) (1+ i)) i
		     (list (list (- (+ (* (/ 80 n) i)
				       (* (/ 20 (1+ n)) (1+ i))) 50) 45 -50)
			   (list (- (* (+ (/ 80 n) (/ 20 (1+ n)))
				       (1+ i)) 50) -45 50))
		     label visual))
	   stratas labels))
  )

;;; Method :blocks is updated in "cgblocks.lsp":

(defmeth drag-graph-proto :blocks (&optional (val nil set))
  (if set
      (progn 
	(setf (car (slot-value 'blocks)) val)
	(send self :update-arrows T)))
  (car (slot-value 'blocks))
  )

;;; Update of method :blocks of "blocks.lsp":

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

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; (defmeth block-proto :NAME (&optional (val nil set))
;;;   (if set (setf (slot-value 'name) val)) (slot-value 'name)
;;;   )

;;; (defmeth block-proto :TYPE (&optional (val nil set))
;;;   (if set (setf (slot-value 'type) val)) (slot-value 'type)
;;;   )

;;; (defmeth block-proto :STATUS (&optional (val nil set))
;;;   (if set (setf (slot-value 'status) val)) (slot-value 'status)
;;;   )

;;; (defmeth block-proto :index (&optional (val nil set))
;;;   (if set (setf (slot-value 'index) val)) (slot-value 'index)
;;;   )

(DEFMETH drag-graph-proto :block-index (block-key)
  (if (listp block-key)
      (car block-key)
    (do ((n 0 (1+ n))
	 (j (send self :blocks) (cdr j)))
	((or (not j)
	     (equal block-key
		    (if (objectp block-key) (car j)
		      (if (numberp block-key) (send (car j) :stratum)
			(send (car j) :label)))))
	 (if (equal block-key
		    (if (objectp block-key) (car j)
		      (if (numberp block-key) (send (car j) :stratum)
			(send (car j) :label)))) n))))
  )

(DEFMETH drag-graph-proto :block (block-key)
  (if (objectp block-key)
      block-key
    (let ((block-index (send self :block-index block-key)))
      (if block-index (nth block-index (send self :blocks)))))
  )

(defmeth block-proto :stratum (&optional (val nil set))
  (if set (setf (slot-value 'stratum) val)) (slot-value 'stratum)
  )

(defmeth block-proto :visual (&optional (val nil set))
  (if set (progn (setf (slot-value 'visual) val)))
  (slot-value 'visual)
  )

(defmeth drag-graph-proto :visual-blocks (&optional (val nil set))
  (if set
      (progn
	(if val (format T "Not implemented fully.~%~
                        Vertices may have positions outside blocks.~%"))
	(if (and  (send self :blocks) (boundp 'linked-toggle-item-proto))
	    (send linked-toggle-item-proto :update self 'visual-blocks val))
	(dolist (block (send self :blocks) nil) (send block :visual val))
	(send self :redraw-graph)))
  (let ((result nil))
    (mapcar #'(lambda (block)
		(if (send block :visual) (setf result T)))
	    (send self :blocks))
    result)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(DEFMETH block-proto :position (&optional (position nil set) &key (redraw nil))
  (if (and set position
	   (not (and (listp position) (= 2 (length position))
		     (2-3-list (car position)) (2-3-list (cadr position)))))
      (error "Invalid argument"))
  (let ((result
	 (if set (setf (slot-value 'positions) position)
	   (slot-value 'positions))))
;;;    (if set (send self :update-arrows))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(DEFMETH drag-graph-proto :block-position
  (block-key &optional (position nil) &key (redraw nil))
  (if set (send (send self :block block-key) :position val
		:redraw (if redraw self nil))
    (send (send self :block block-key) :position))
  )

(DEFMETH block-proto :adjust-to-grid (delta)
  (setf (slot-value 'positions) 
	(* (round (/ (slot-value 'positions) delta)) delta))
  )

(DEFMETH drag-graph-proto :adjust-blocks-to-grid
  (&key (delta 1) (redraw nil))
  ;; undo is pushed in :adjust-vertices-to-grid
  (mapcar #'(lambda (j) (send j :adjust-to-grid delta)) (send self :blocks))
  ;; NECESSARY, also when call to :adjust-vertices-to-grid:
  (send self :update-arrows)
  (send self :redraw-graph)
  )

(DEFMETH block-proto :rescale-positions (scale)
  (setf (slot-value 'positions) (* (slot-value 'positions) scale))
  )

(DEFMETH drag-graph-proto :rescale-block-positions
  (&key (scale 1) (redraw nil))
  ;; undo is pushed in :rescale-vertex-positions
  (mapcar #'(lambda (j) (send j :rescale-positions scale)) (send self :blocks))
  ;; un-necessary when also call to :rescale-vertex-positions:
  (send self :update-arrows)
  (send self :redraw-graph)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(DEFMETH block-proto :label (&optional (label nil set) &key (redraw nil))
  (let ((result
	 (if set (setf (slot-value 'label) label)
	   (slot-value 'label))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(DEFMETH drag-graph-proto :block-label
  (block-key &optional (label nil set) &key (redraw nil))
  (if set (send (send self :block block-key) :label label
		:redraw (if redraw self nil))
    (send (send self :block block-key) :label))
  )

(DEFMETH block-proto :label-position (&optional (position nil set)
						&key (redraw nil))
  (if (and position (not (2-3-list position))) (error "Invalid argument"))
  (let ((result
	 (if set (setf (slot-value 'label-position) position)
	   (slot-value 'label-position))))
    (if (and set redraw) (send redraw :redraw-graph))
    result)
  )

(DEFMETH drag-graph-proto :block-label-position
  (block-key &optional (label-position nil set) &key (redraw nil))
  (if set (send (send self :block block-key) :label-position label-position
		:redraw (if redraw self nil))
    (send (send self :block block-key) :label-position))
  )

(defmeth block-proto :fix-label-position (&optional (val nil set))
  (if set (setf (slot-value 'fix-label-position) val))
  (slot-value 'fix-label-position)
  )

(defmeth block-proto :label-arrow (&optional (val nil set))
  (if set (setf (slot-value 'label-arrow) val))
  (slot-value 'label-arrow)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; The method :update-arrows is updated in "cgblocks.lsp":

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
	(if remove (send self :remove-tests))
	(if redraw (send self :redraw-graph))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :position-vertices-in-default-blocks
  (block-list nanny n draw-blocks)
  (mapcar
   #'(lambda (name-list i)
       (let ((m (length name-list)))
	 (mapcar
	  #'(lambda (name j)
	      (let ((vertex (send self :vertex name)))
		(if draw-blocks
		    (if (< m 3)
			(send vertex :position
			      (list (- (+ (* (/ 80 n) (+ 0.5 i))
					  (* (/ 20 (1+ n)) (1+ i))) 50)
				    (- (+ (* (/ 80 m) (+ 0.5 j))
					  (* (/ 10 (1+ m)) (1+ j))) 45) 0))
		      (send vertex :position
			    (list (- (+ (* (/ 80 n) (+ 0.5 i))
					(* (/ 20 (1+ n)) (1+ i))
					(* 0.8 (/ 40 n)
					   (cos (* 2 (* pi (/ j m)))))) 50)
				  (+ 0 (* 40 (sin (* 2 (* pi (/ j m))))))
				  (+ 0 (* 40 (cos (* 2 (* pi (/ j m))))))))))
		(send vertex :stratum (1+ (if nanny (- n i) i)))))
	  name-list
	  (iseq (length name-list)))))
   block-list
   (if nanny
       (reverse (iseq (length block-list)))
     (iseq (length block-list))))
  )

(defmeth drag-graph-proto :set-blocks
  (block-list &optional (labels nil) (nanny nil) (draw-blocks T))
  (let* ((b (if (not (listp block-list))
		(string-to-block-list block-list)))
	 (nanny (if (listp block-list) nanny (cadr b)))
	 (block-list (if (listp block-list) block-list
		       (if nanny (reverse (car b)) (car b))))
	 (n (length block-list)))
    (slot-value 'blocks (return-block-list
			 (if nanny
			     (reverse (iseq (length block-list)))
			   (iseq (length block-list)))
			 (if labels labels
			   (= 0 (repeat 1 (length block-list))))
			 n nanny draw-blocks))
    (send self :position-vertices-in-default-blocks
	  block-list nanny n draw-blocks))
  (mapcar #'(lambda (item)
	      (if (or (equalp (send item :title) "Tests ...")
		      (equalp (send item :title)
			      "Description of fitted values ...")
		      (equalp (send item :title) "The EH procedure ..."))
		  (send item :enabled nil)))
	  (send (send self :slot-value 'menu) :slot-value 'items))
  (send self :redraw-graph)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;;; Method :define-blocks is updated in "cgblock.lsp":

(defmeth drag-graph-proto :define-blocks
  (block-list &optional (labels nil) (nanny nil) (draw-blocks T))
  (let ((new self)
	(current-number (send self :return-model-number 'current)))
    (send self :make-graph-current-model :redraw-plots nil)
    (if T
	(progn
	  (if (send self :static)
	      (if (or T draw-blocks)
		  (progn
		    (setf new
			  (send new :make-graph
				:location (+ (list 20 0)
					     (send self :location))
				:title (concatenate 'string " + Blocks")))
;;;		    (send self :add-manager-edge self new :type 'sibling)
		    )
		(setf new
		      (send new :return-child-coco-graph-window
			    :copy-slots nil
			    :offset (list 20 0)
			    :title (concatenate 'string " + Blocks")
			    :sibling T)))
	    (progn
	      (send self :remove-tests)
	      (send self :title (concatenate
				 'string (send self :title) " + Blocks"))))
	  (send new :set-blocks block-list labels nanny draw-blocks))
      (format t "Blocks already added. ~%"))
    (send self :make-current current-number)
    (send self :redraw-graph)
    new)
  )

(defmeth drag-graph-proto :add-block
  (n &key (a (list -45 -45 -50)) (b (list -40 -40  50)) (label nil))
  (if (not (and (2-3-list a) (2-3-list b))) (error "Invalid argument"))
  (send self :blocks
	(concatenate 'list
		     (list (send block-proto
				 :new n (length (send self :blocks))
				 (list a b) label (send self :visual-blocks)))
		     (send self :blocks)))
  (send self :update-arrows) ;;; ???
  (send self :redraw-graph)
  )

(defmeth drag-graph-proto :delete-block (n)
  (let ((blocks nil))
    (mapcar #'(lambda (i)
		(if (not (if (listp n)
			     (eq (car n) (send self :block-index i))
			   (eq (if (numberp n)
				   (send i :stratum) (send i :label))
			       n)))
		    (setf blocks (cons i blocks))))
	    (send self :blocks))
    (send self :blocks (reverse blocks)))
  (send self :update-arrows T) ;;; ???
  (send self :redraw-graph)
  )

(defmeth block-proto :delete ()
  (send graph :delete-block (send self :stratum))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth block-proto :format-label ()
  (if (send self :label) (send self :label)
    (format nil "~1d" (send self :stratum)))
  )

(defmeth drag-graph-proto :format-block-label (block)
  (send block :format-label)
  )

(DEFMETH drag-graph-proto :draw-block (block)
  (if (send block :visual)
      (let ((position (send block :position))
	    (label-position (send block :label-position)))
	(send self :draw-rectangle
	      (car (car position)) (cadr (car position)) (caddr (car position))
	      (car (cadr position)) (cadr (cadr position)) (caddr (cadr position)))
	(let ((x (send self :project
		       (+ (car position)
			  (if label-position label-position 0)))))
	  (send self :draw-string (send self :format-block-label block)
		(send self :to-x-pixel (car x))
		(send self :to-y-pixel (cadr x))))))
  )

(defmeth drag-graph-proto :draw-blocks ()
  (if (screen-has-color)
      (send self :draw-color (send self :item-color 'block))) ; 'wheat)
  (dolist (block (send self :blocks) NIL)
	  (send self :draw-block block))
  )

(defmeth block-proto :draw (graph)
  (send graph :draw-block self)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun in-block (position block)
  (flet ((between (x y a)  (or (< x a y) (< y a x))))
	(and (between (caar block) (car (cadr block)) (car position))
	     (between (cadar block) (cadr (cadr block)) (cadr position))
	     (between (caddar block) (caddr (cadr block)) (caddr position))))
  )

(defun return-block-point (n block)
  (case n 
	(0 (list (car (car  block)) (cadr (car  block)) (caddr (car  block)))) 
	(1 (list (car (cadr block)) (cadr (cadr block)) (caddr (cadr block))))
	(2 (list (car (car  block)) (cadr (cadr block)) (caddr (car  block))))
	(3 (list (car (cadr block)) (cadr (car  block)) (caddr (car  block))))
	(4 (list (car (car  block)) (cadr (car  block)) (caddr (cadr block))))
	(5 (list (car (car  block)) (cadr (cadr block)) (caddr (cadr block))))
	(6 (list (car (cadr block)) (cadr (car  block)) (caddr (cadr block))))
	(7 (list (car (cadr block)) (cadr (cadr block)) (caddr (car  block)))))
  )

(defun to-block-points (n p)
  (let ((a (car p)) (b (cadr p)) (c (caddr p)))
    (case n
	  (0 (list (list a b c) (list 0 0 0)))
	  (1 (list (list 0 0 0) (list a b c)))
	  (2 (list (list a 0 c) (list 0 b 0)))
	  (3 (list (list 0 b c) (list a 0 0)))
	  (4 (list (list a b 0) (list 0 0 c)))
	  (5 (list (list a 0 0) (list 0 b c)))
	  (6 (list (list 0 b 0) (list a 0 c)))
	  (7 (list (list 0 0 c) (list a b 0)))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(DEFMETH drag-graph-proto :visit-block-points (u v a b c x y z)
  (let ((point nil)
	(potential 0))
    (mapcar #'(lambda (i k)
		(let ((p (point-potential u v (send self :project i))))
		  (if (< p potential)
		      (progn (setf point k) (setf potential p)))))
	    (list (list a b c) (list x y z) (list a y c) (list x b c)
		  (list a b z) (list a y z) (list x b z) (list x y c))
	    (iseq 8))
    (list potential point))
  )

(DEFMETH drag-graph-proto :return-closest-block-point-with-potential
  (x y)
  (let ((point nil)
	(block nil)
	(potential 0))
    (mapcar
     #'(lambda (i)
	 (let* ((positions (send i :position))
		(x (send self :visit-block-points x y
			 (car (car positions)) (cadr (car positions))
			 (caddr (car positions))
			 (car (cadr positions)) (cadr (cadr positions))
			 (caddr (cadr positions)))))
	   (if (< (car x) potential)
	       (progn (setf block i)
		      (setf point (cadr x))
		      (setf potential (car x))))))
     (send self :blocks))
    (list potential (list block point)))
  )

(defmeth drag-graph-proto :return-closest-block-point-in-canvas
  (x y)
  (cdr (send self :return-closest-block-point-with-potential
	     (send self :from-x-pixel x) (send self :from-y-pixel y)))
  )

;;

(provide "vertices")
