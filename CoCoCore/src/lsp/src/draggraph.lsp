
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the drag-graph-proto
;;; and adds the basic methods to this proto


(defproto drag-graph-proto
  '(vertices edges blocks use-variables x y colors ;;; p
	     undo-positions redo-positions static grid drag-graph edit-graph
	     overlays transformation angle rotation-type)
  '(instances-graph) (list graph-window-proto))

(send drag-graph-proto :documentation
      'proto "Drag Graph graphics window prototype based on~
              basic graphics window prototype")

(defmeth drag-graph-proto :isnew (&key location title)
  (let ((x (apply #'call-next-method
		  :location location :title (list title))))
    (send drag-graph-proto :slot-value 'instances-graph
	  (cons x (slot-value 'instances-graph)))
    x)
  )

(defmeth drag-graph-proto :use-variables (&optional (val nil))
  (if val (setf (slot-value 'use-variables) val))
  (slot-value 'use-variables)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :location
  (&optional (left nil) (top nil))
  (if (and left top) (call-next-method (mod left 900) (mod top 900))
    (call-next-method)) ; Error when only one argument
  )

(defmeth drag-graph-proto :x (&optional (val nil set))
  (if set (setf (slot-value 'x) val)) (slot-value 'x)
  )

(defmeth drag-graph-proto :y (&optional (val nil set))
  (if set (setf (slot-value 'y) val)) (slot-value 'y)
  )

(defmeth drag-graph-proto :in-wastebasket ()
  (and (> (slot-value 'x) (send self :canvas-width))
       (> (slot-value 'y) (send self :canvas-height)))
  )

(defun rescale-procent (p x)  (* p x)  )

(defmeth drag-graph-proto :to-x-pixel (x)
  (floor (* (/ (send self :canvas-width) 100) (+ 50 x)))
  )

(defmeth drag-graph-proto :to-y-pixel (y)
  (floor (* (/ (send self :canvas-height) 100) (+ 50 y)))
  )

(defmeth drag-graph-proto :from-x-pixel (x)
  (- (* (/ x (send self :canvas-width)) 100) 50)
  )

(defmeth drag-graph-proto :from-y-pixel (y)
  (- (* (/ y (send self :canvas-height)) 100) 50)
  )

(defmeth drag-graph-proto :resize ()
  (send self :x (/ (send self :canvas-width) 2))
  (send self :y (/ (send self :canvas-height) 2))
  (if (and (send self :has-slot 'colors)
	   (send self :slot-value 'colors))
      (send self :redraw-graph))
  )

(defmeth drag-graph-proto :transformation
  (&optional (val nil set))
  (if set (setf (slot-value 'transformation) val)) (slot-value 'transformation)
  )

(defmeth drag-graph-proto :angle (&optional (val nil set))
  (if set (setf (slot-value 'angle) val)) (slot-value 'angle)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :static (&optional (val nil set))
  (if set
      (progn (setf (slot-value 'static) val)
	     (if (boundp 'linked-toggle-item-proto)
		 (send linked-toggle-item-proto :update self 'static val))))
  (slot-value 'static)
  )

(defmeth drag-graph-proto :grid (&optional (val nil set))
  (if set (progn (setf (slot-value 'grid) val)
		 (if (boundp 'linked-toggle-item-proto)
		     (send linked-toggle-item-proto :update self 'grid val))
		 (send self :redraw)))
  (slot-value 'grid)
  )

(defmeth drag-graph-proto :drag-graph (&optional (val nil set))
  (if set (setf (slot-value 'drag-graph) val))
  (slot-value 'drag-graph)
  )

(defmeth drag-graph-proto :edit-graph (&optional (val nil set))
  (if set (setf (slot-value 'edit-graph) val))
  (slot-value 'edit-graph)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :draw-color (color)
  (let ((current-color (call-method graph-window-proto :draw-color)))
    (if (or (equalp color 'black)
	    (not (or (equalp current-color 'white)
		     (equalp current-color 'beige))))
	(call-method graph-window-proto :draw-color color)))
  )

(setf *default-colors*
      (list
       'blue        ;	'vertex
       'cyan        ;	'vertex-label

       'yellow      ;	'new-edge
       'green       ;	'not-fitted
       'blue        ;	'fitted
       'magenta     ;	'non-decomposable
       'red         ;	'not-submodel-of-base
       'yellow      ;	'not-same-coco-object
       'cyan        ;	'coherence
       'black       ;	'error-edge
       'yellow      ;	'fix-edge

       'red         ;	'grid
       'red         ;   'rotate
       'green       ;   'block
       'blue        ;	'future-edge
       'green       ;   'controls
       'yellow      ;	'sibling
       'black       ;   'error
       
       'red))

(defmeth drag-graph-proto :item-color
  (item &optional (value nil set))
  (let ((number (case item
		      ('vertex                  0)	; 'blue
		      ('vertex-label	        1)	; 'cyan
		      ('new-edge	        2)	; 'yellow
		      ('not-fitted	        3)	; 'green
		      ('fitted                  4)	; 'dark-green
		      ('non-decomposable	5)	; 'brown
		      ('not-submodel-of-base    6)	; 'dark-violet
		      ('not-same-coco-object    7)	; 'Yellow
		      ('coherence	        8)	; 'light-red
		      ('error-edge	        9)	; 'pale-red 
		      ('fix-edge	       10)	; 'orange
		      ('grid	               11)	; 'magenta
		      ('rotate	               12)	; 'salmon
		      ('block	               13)	; 'wheat
		      ('future-edge            14)	; 'violet
		      ('controls               15)	; 'gold
		      ('sibling                16)	; 'gold
		      ('factor-edge             0)	; 'blue
		      (T                       17)      ; 'black
		      )))
    (if set (progn (setf (nth number (slot-value 'colors)) value)
		   (send self :redraw-graph))
      (nth number (slot-value 'colors))))
  )


;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun close-point (x y u v)
  (< (+ (^ (- u x) 2) (^ (- v y) 2)) 15)
  )

(defmeth drag-graph-proto :close-click (x y)
  (close-point x y (send self :x) (send self :y))
  )

(defun point-potential (x y point)
  (let ((distance (+ (^ (- (car point) x) 2) (^ (- (cadr point) y) 2))))
    (if (= 0 distance) -99999999 (- 0 (/ 1 distance))))
  )

(defmeth drag-graph-proto :find-move (x y p)
  (send self :inverse-project
	(concatenate 'list
		     (- (list x y) (select (send self :project p) '(0 1)))
		     '(0)))
  )

(defun 2-3-list (position)
  (and (listp position) (<= 2 (length position) 3)
       (numberp (car position)) (numberp (cadr position))
       (if (caddr position) (numberp (caddr position)) T))
  )

(defun 2-to-3-list (position)
  (if (caddr position) position (concatenate 'list position '(0)))
  )


;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :project (position)
  (if (slot-value 'transformation)
      (matmult (slot-value 'transformation) position)
    position)
  )

(defmeth drag-graph-proto :inverse-project (position)
  (if (slot-value 'transformation)
      (solve (slot-value 'transformation) position)
    position)
  )

(defmeth drag-graph-proto :apply-transformation
  (trans &optional (draw-box nil) (redraw nil))
  (if (not (slot-value 'transformation))
      (slot-value 'transformation (identity-matrix 3)))
  (slot-value 'transformation
	(matmult (slot-value 'transformation) trans))
  (send self :redraw-graph)
  (if (screen-has-color)
      (send self :draw-color (send self :item-color 'rotate))) ; 'salmon)
  (if draw-box (send self :draw-rectangle -25 -25 -25 25 25 25))
  )

(defmeth drag-graph-proto :canvas-to-sphere (x y rad)
  (let* ((x (- (send self :from-x-pixel x) 0))
	 (y (- (send self :from-y-pixel y) 0))
	 (norm-2 (+ (* x x) (* y y)))
	 (rad-2 (^ rad 2))
	 (z (sqrt (max (- rad-2 norm-2) 0))))
    (if (< norm-2 rad-2)
	(list x y z)
      (let ((r (sqrt (/ norm-2 rad-2))))
	(list (/ x r) (/ y r) (/ z r)))))
  )

(defmeth drag-graph-proto :do-hand-rotate (x y m1 m2)
  (send self :push-transformation)
  (send self :idle-on T)
  (let* ((oldp (send self :canvas-to-sphere x y 50))
	 (p oldp))
    (flet ((spin-sphere (x y)
	    (setf oldp p)
	    (setf p (send self :canvas-to-sphere x y 50))
	    (send self :idle-on nil)
	    (if (send self :has-slot 'tour-trans)
		(progn
		  (setf (slot-value 'tour-count) 50)
		  (setf (slot-value 'tour-trans)
			(make-rotation oldp p (send self :angle)))))
	    (send self :apply-transformation (make-rotation oldp p) T nil)))
	  (send self :while-button-down #'spin-sphere)))
  )

(defmeth drag-graph-proto :rock-plot (&optional (k 2))
  (let ((angle (send self :angle))
	(p1 (sphere-rand 3)) ;; (send self :canvas-to-sphere  50  50  50))
	(p2 (sphere-rand 3)) ;; (send self :canvas-to-sphere -50 -50 -50))
	)
    (dotimes (i k)
	     (send self :apply-transformation (make-rotation p1 p2 angle)))
    (dotimes (i (* 2 k))
	     (send self :apply-transformation (make-rotation p1 p2 (- angle))))
    (dotimes (i k)
	     (send self :apply-transformation (make-rotation p1 p2 angle)))))

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun sphere-rand (n)
  (loop (let* ((x (- (* 2 (uniform-rand n)) 1))
               (nx2 (sum (^ x 2))))
          (if (< nx2 1) (return (/ x (sqrt nx2)))))))

(defmeth drag-graph-proto :add-idle ()
  
  (send self :add-slot 'tour-count -1)
  
  (send self :add-slot 'tour-trans nil)
  
  (defmeth self :do-idle () (send self :tour-step))
  
  (defmeth self :tour-step ()
    (when (< (slot-value 'tour-count) 0)
	  (let ((angle (abs (send self :angle))))
	    (setf (slot-value 'tour-count) 
		  (random (floor (/ pi (* 2 angle)))))
	    (setf (slot-value 'tour-trans) 
		  (make-rotation (sphere-rand 3) (sphere-rand 3) angle))))
    (send self :apply-transformation (slot-value 'tour-trans) nil T)
    (setf (slot-value 'tour-count) (- (slot-value 'tour-count) 1)))
  
  (defmeth self :tour-on (&rest args) (apply #'send self :idle-on args))
  
  (let ((item (send graph-item-proto :new "Touring" self
		    :tour-on :tour-on :toggle T)))
    (send item :key #\T)
    (send (send self :menu) :append-items item)
    )
  
  self)


;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :draw-rectangle-line (a b)
  (let ((x1 (send self :project a))
	(x2 (send self :project b)))
    (send self :draw-line
	  (send self :to-x-pixel (car x1))
	  (send self :to-y-pixel (cadr x1))
	  (send self :to-x-pixel (car x2))
	  (send self :to-y-pixel (cadr x2))))
  )

(defmeth drag-graph-proto :draw-rectangle (a b c x y z)
  (send self :line-width 1)
  (send self :line-type 'dashed)
  (send self :draw-rectangle-line (list a b c) (list a y c))
  (send self :draw-rectangle-line (list x b c) (list x y c))
  (send self :draw-rectangle-line (list a b c) (list x b c))
  (send self :draw-rectangle-line (list a y c) (list x y c))
  (if (send self :transformation)
      (progn
	(send self :draw-rectangle-line (list a b z) (list a y z))
	(send self :draw-rectangle-line (list x b z) (list x y z))
	(send self :draw-rectangle-line (list a b z) (list x b z))
	(send self :draw-rectangle-line (list a y z) (list x y z))
	(send self :draw-rectangle-line (list a b c) (list a b z))
	(send self :draw-rectangle-line (list a y c) (list a y z))
	(send self :draw-rectangle-line (list x b c) (list x b z))
	(send self :draw-rectangle-line (list x y c) (list x y z))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :draw-shadows ()
  (if (screen-has-color) (send self :draw-color 'beige))
  (if (slot-value 'overlays)
      (defmeth self :to-x-pixel (x)
	(+ 15 (floor (* (/ (- (send self :canvas-width) 150) 100)
			(+ 50 x))) 150))
    (defmeth self :to-x-pixel (x)
      (+ 15 (floor (* (/ (send self :canvas-width) 100) (+ 50 x))))))
  (defmeth self :to-y-pixel (y)
    (+ 25 (floor (* (/ (send self :canvas-height) 100) (+ 50 y)))))
  (send self :draw-edges)
  (send self :draw-blocks)
  (send self :draw-vertices)
  (if (slot-value 'overlays)
      (defmeth self :to-x-pixel (x)
	(+ (floor (* (/ (- (send self :canvas-width) 150) 100) (+ 50 x))) 150))
    (defmeth self :to-x-pixel (x)
      (floor (* (/ (send self :canvas-width) 100) (+ 50 x)))))
  (defmeth self :to-y-pixel (y)
    (floor (* (/ (send self :canvas-height) 100) (+ 50 y))))
  (if (screen-has-color) (send self :draw-color 'black))
  )

(defmeth drag-graph-proto :draw-grid ()
  (if (screen-has-color)
      (send self :draw-color (send self :item-color 'grid))) ; 'magenta)
  (if (slot-value 'grid)
      (dotimes (i 19 nil)
	       (dotimes (j 19 nil)
			(send self :draw-symbol
			      (if (or (eq (mod i 2) 1)
				      (eq (mod j 2) 1)) 'dot1 'dot4) nil
			      (send self :to-x-pixel (* (+ -9 i) 5))
			      (send self :to-y-pixel (* (+ -9 j) 5))))))
  )

(defmeth drag-graph-proto :redraw-graph (&optional (radius 4))
  (send self :erase-window)
  (if (screen-has-color)
      (send self :draw-color (send self :item-color 'controls))) ; 'green)
  (send self :draw-string
	(string (send self :title))
	(send self :to-x-pixel 0) (send self :to-y-pixel -45))
  (if (if (boundp '*shadows*) *shadows* nil)
      (send self :draw-shadows))
  (send self :draw-edges radius)
  (if (send self :visual-blocks) (send self :draw-blocks))
  (send self :line-width 1)
  (send self :line-type 'solid)
  (send self :draw-vertices radius)
  (send self :draw-grid)
  (send self :line-width 1)
  (send self :line-type 'solid)
  (send self :redraw-overlays)
  T
  )  

(defmeth drag-graph-proto :redraw (&optional (radius 4))
; (format T "Redraw the graph by the key-event `r'~%")
  (send self :redraw-graph radius)
  )  

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :undo-redo (x)
  (case (car x) 
	('transformation
	 (let* ((transformation (cadr x))
		(result (send self :return-transformation)))
	   (send self :transformation transformation)
	   result))
	('edge-label-position
	 (let* ((edge (cadr x))
		(label-position (caddr x))
		(result (send self :return-edge-undo edge nil)))
	   (send edge :label-position label-position)
	   result))
	('edge-test
	 (let* ((edge (cadr x))
		(result (send self :return-edge-undo edge T)))
	   (send edge :slot-value 'width              (nth 2 x))
	   (send edge :slot-value 'test               (nth 3 x))
	   (send edge :slot-value 'dashed             (nth 4 x))
	   (send edge :slot-value 'label-position     (nth 5 x))
	   (send edge :slot-value 'fix-label-position (nth 6 x))
	   (send edge :slot-value 'label-arrow        (nth 7 x))
	   result))
	('vertex-position
	 (let* ((vertex (cadr x))
		(position (caddr x))
		(label-position (cadddr x))
		(result (send self :return-vertex-undo vertex)))
	   (send vertex :position position)
	   (send vertex :slot-value 'label-position label-position)
	   result))
	(t
	 (let* ((stratum  (nth 1 x))
		(block    (nth 2 x))
		(vertices (nth 3 x))
		(blocks   (nth 4 x))
		(result   (send self :return-block-points-undo block stratum)))
	   (mapcar
	    #'(lambda (x)
		(let ((vertex (cadr x))
		      (position (caddr x))
		      (label-position (cadddr x)))
		  (send vertex :position position)
		  (send vertex :slot-value 'label-position label-position)))
	    vertices)
	   (mapcar
	    #'(lambda (x)
		(let ((block (cadr x))
		      (positions (caddr x))
		      (label-position (cadddr x)))
		  (send block :position positions)
		  (send block :slot-value 'label-position label-position)))
	    blocks)
	   result)))
  )

(defmeth drag-graph-proto :return-transformation ()
  (list 'transformation (send self :transformation))
  )

(defmeth drag-graph-proto :return-edge-undo (edge test)
  (if test
      (list 'edge-test edge
	    (send edge :slot-value 'width)
	    (send edge :slot-value 'test)
	    (send edge :slot-value 'dashed)
	    (send edge :slot-value 'label-position)
	    (send edge :slot-value 'fix-label-position)
	    (send edge :slot-value 'label-arrow))
    (list 'edge-label-position edge
	  (copy-list (send edge :label-position))))
  )

(defmeth drag-graph-proto :return-vertex-undo (vertex)
  (list 'vertex-position vertex
        (copy-list (send vertex :position))
	(copy-list (send vertex :label-position)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :return-block-points-undo (block-id stratum)
  (let ((vertices nil)
	(blocks nil))
    (dolist (vertex (send self :vertices) vertices)
	    (if (or (equalp T stratum) (equalp (send vertex :stratum) stratum))
		(setf vertices
		      (concatenate
		       'list vertices
		       (list (list nil vertex
				   (copy-list (send vertex :position))
				   (copy-list (send vertex :label-position))
				   ))))))
    (dolist (block (send self :blocks) blocks)
	    (if (or (equalp T block-id) (equalp block block-id))
		(setf blocks
		      (concatenate
		       'list blocks
		       (list (list nil block
				   (copy-list (send block :position))
				   (copy-list (send block :label-position))
				   ))))))
    (list 'block stratum block-id vertices blocks))
  )

(defmeth drag-graph-proto :push-transformation ()
  (slot-value 'undo-positions (cons (send self :return-transformation)
				    (slot-value 'undo-positions)))
  )

(defmeth drag-graph-proto :push-edge-undo (edge test)
  (if (listp edge)
      nil
    (slot-value 'undo-positions (cons (send self :return-edge-undo edge test)
				      (slot-value 'undo-positions))))
  )

(defmeth drag-graph-proto :push-vertex-undo (vertex)
  (if (listp vertex)
      nil
    (slot-value 'undo-positions (cons (send self :return-vertex-undo vertex)
				      (slot-value 'undo-positions))))
  )

(defmeth drag-graph-proto :push-block-points-undo (block stratum)
  (slot-value 'undo-positions
	      (cons (send self :return-block-points-undo block stratum)
		    (slot-value 'undo-positions)))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :undo-move (&key (redraw T))
  (if (slot-value 'undo-positions)
      (progn
	(slot-value 'redo-positions
		    (cons (send self :undo-redo
				(car (slot-value 'undo-positions)))
			  (slot-value 'redo-positions)))
	(slot-value 'undo-positions (cdr (slot-value 'undo-positions)))
	(send self :update-arrows)
	(if redraw (send self :redraw-graph)))
    (format T "No more moves to undo~%"))
  )

(defmeth drag-graph-proto :skip-undo-move ()
  (if (slot-value 'undo-positions)
      (progn
	(slot-value 'redo-positions (cons (car (slot-value 'undo-positions))
					  (slot-value 'redo-positions)))
	(slot-value 'undo-positions (cdr (slot-value 'undo-positions)))))
  )

(defmeth drag-graph-proto :redo-move (&key (redraw T))
  (if (slot-value 'redo-positions)
      (progn
	(slot-value 'undo-positions
		    (cons (send self :undo-redo
				(car (slot-value 'redo-positions)))
			  (slot-value 'undo-positions)))
	(slot-value 'redo-positions (cdr (slot-value 'redo-positions)))
	(send self :update-arrows)
	(if redraw (send self :redraw-graph)))
    (format T "No more moves to redo~%"))
  )

(defmeth drag-graph-proto :skip-redo-move ()
  (if (slot-value 'redo-positions)
      (progn
	(slot-value 'undo-positions (cons (car (slot-value 'redo-positions))
					  (slot-value 'undo-positions)))
	(slot-value 'redo-positions (cdr (slot-value 'redo-positions)))))
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Updated for "factor-graph":

(defmeth drag-graph-proto :drag-point (x y m1 m2 vertex)
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
		(send vertex :position (+ position new-pos))
	      nil)))))			; (send self :drag-line-from-point x y)
  (send self :redraw-graph)
  (send self :buffer-to-screen)
  )

(defmeth drag-graph-proto :drag-edge-label (x y m1 m2 edge)
  (send self :start-buffering)
  (if edge
      (let ((label-position (send edge :slot-value 'label-position)))
	(let ((new-pos (send self :find-move
			     (send self :from-x-pixel x)
			     (send self :from-y-pixel y)
			     (send edge :label-position))))
	  (if (and m1 m2)
	      (send edge :slot-value 'label-position
		    (+ label-position new-pos))))))
  (send self :redraw-graph)
  (send self :buffer-to-screen)
  )

(defmeth drag-graph-proto :drag-block-point (x y m1 m2 point)
  (send self :start-buffering)
  (if point
      (let* ((block (car point))
	     (position (send block :position)))
	(let ((new-pos
	       (send self :find-move
		     (send self :from-x-pixel x)
		     (send self :from-y-pixel y) 
		     (return-block-point (cadr point) position))))
	  (if (and m1 m2)
	      (send block :label-position new-pos)
;	      (progn
;		(send self :delete-block (cadr p))
;		(send self :p nil))
	    (if m2
		nil            ; (send self :do-hand-rotate x y m1 m2)
	      (if m1
		  (progn
		    (send block :position (+ position (list new-pos new-pos)))
		    (send self :drag-points (send block :stratum) new-pos))
		(send block :position (+ position
					 (to-block-points
					  (cadr point) new-pos)))))))))
  (send self :redraw-graph)
  (send self :buffer-to-screen)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth drag-graph-proto :drag-line-from-point (x y vertex)
; (send self :erase-window)
  (if (eq 0 (random 2))
      (progn
	(send self :start-buffering)
	(send self :redraw-graph)
	(send self :buffer-to-screen)))
  (let ((pos (send self :project (send vertex :position))))
    (send self :line-width 2)
    (send self :line-type 'dashed)
    (if (screen-has-color)
	(send self :draw-color (send self :item-color 'new-edge))) ; 'yellow)
    (send self :draw-line
	  (send self :to-x-pixel (car pos))
	  (send self :to-y-pixel (cadr pos))
	  x y))
  )

(defmeth drag-graph-proto :drag-points (n new-pos)
  (mapcar #'(lambda (vertex)
	      (if (eq (send vertex :stratum) n)
		  (send vertex :position (+ (send vertex :position) new-pos))))
	  (send self :vertices))
  )

(defmeth drag-graph-proto :click-vertex (point)
  )

(defmeth drag-graph-proto :do-click (x y m1 m2)
  (if (not (send self :overlay-click x y m1 m2))
      (if (and m2 (not m1) (not (slot-value 'edit-graph)))
	  (send self :do-hand-rotate x y m1 m2)      
	(let ((drag nil)
	      (point nil)
	      (type 'vertex))
	  (flet ((set-symbol (x y)
		  (if (or drag (slot-value 'edit-graph)
			  (not (send self :close-click x y)))
		      (if (or m1 m2 (slot-value 'edit-graph) (eq 'block type))
			  (progn
			    (if (not drag)
				(if (eq 'block type)
				    (send self :push-block-points-undo
					  (car point)
					  (if (and m1 (not m2))
					      (send (car point) :stratum)))
				  (if (eq 'vertex type)
				      (send self :push-vertex-undo point)
				    (send self :push-edge-undo point nil))))
			    (setf drag T)
			    (if (eq 'vertex type)
				(send self :drag-point x y
				      (or m1 (slot-value 'edit-graph))
				      m2 point)
			      (if (eq 'block type)
				  (send self :drag-block-point x y m1 m2 point)
				(send self :drag-edge-label x y m1 m2 point))))
			(send self :drag-line-from-point x y point)))
		  (send self :x x)
		  (send self :y y)))
		(send self :x x)
		(send self :y y)
		(setf point (send self :return-closest-vertex-with-potential
				  (send self :from-x-pixel x)
				  (send self :from-y-pixel y)))
		(if (send self :blocks)
		    (let ((b (send self
				   :return-closest-block-point-with-potential
				   (send self :from-x-pixel x)
				   (send self :from-y-pixel y))))
		      (if (< (* 2 (car b)) (car point))
			  (progn
			    (setf point b)
			    (setf type 'block)))))
		(if (and m1 m2)
		    (let ((b (send self
				   :return-closest-edge-label-with-potential
				   (send self :from-x-pixel x)
				   (send self :from-y-pixel y))))
		      (if (and b (< (* 2 (car b)) (car point)))
			  (progn
			    (setf point b)
			    (setf type 'edge-label)))))
		(setf point (cadr point))
		(set-symbol x y)
		(if (and (not (slot-value 'drag-graph)) (or m1 m2))
		    (let ((cursor (send self :cursor)))
		      (send self :cursor 'finger)
		      (let ((xy (send self :drag-grey-rect x y 5 5)))
			(set-symbol (+ 3 (first xy)) (+ 3 (second xy))))
		      (send self :cursor 'cursor))
		  (send self :while-button-down #'set-symbol)))
	  (if (not drag)
	      (if (send self :close-click x y)
		  (let ((edge (send self :return-closest-edge-in-canvas
				    (send self :x) (send self :y))))
		    (if edge
			(if (and m1 m2)
			    (send self :drop-edge-label edge)
			  (if m1
			      (send self :set-edge-label edge :follow T
				    :decomposable-mode nil)
			    (if m2 (send self :drop-edge-label edge)
			      (send self :graph-drop-edge edge))))
			    (send self :click-vertex point)))
		(send self :graph-add-vertex-pair
		      point
		      (send self :return-closest-vertex-in-canvas
			    (send self :x) (send self :y))))
	    (if (and (or m1 (slot-value 'edit-graph))
		     (not m2) (send self :in-wastebasket) (eq 'vertex type))
		(progn
		  (send self :undo-move :redraw nil)
		  (send self :graph-drop-edge nil :point point))
	      (send self :update-arrows
		    (and m1 m2 (eq 'block type) (send self :visual-blocks))
		    :redraw T))))))
  )

(defmeth drag-graph-proto :do-motion (x y)
  (send self :x x)
  (send self :y y)
  )

(defmeth drag-graph-proto :do-key (c m1 m2)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;;
;;; Overlay Methods
;;;

(defmeth drag-graph-proto :redraw-overlays ()
  (if (screen-has-color)
      (send self :draw-color (send self :item-color 'controls))) ; 'gold)
  (dolist (ov (reverse (slot-value 'overlays)))
          (send ov :redraw)))
  
(defmeth drag-graph-proto :resize-overlays ()
  (dolist (ov (slot-value 'overlays))
          (send ov :resize)))
  
(defmeth drag-graph-proto :overlay-click (x y m1 m2)
  (dolist (ov (slot-value 'overlays))
    (if (send ov :do-click x y m1 m2) (return T))))

(defmeth drag-graph-proto :add-overlay (ov)
  (if (send ov :graph) (error "Already installed in a graph"))
  (send ov :slot-value 'graph self)
  (setf (slot-value 'overlays) (cons ov (slot-value 'overlays))))

(defmeth drag-graph-proto :delete-overlay (ov)
  (when (member ov (slot-value 'overlays))
        (send ov :slot-value 'graph nil)
        (setf (slot-value 'overlays) (remove ov (slot-value 'overlays)))))



;;;; Installation method

(defmeth drag-graph-proto :add-controls ()
  (apply #'send self :size (+ (send self :size) '(150 0)))
  (defmeth self :to-x-pixel (x)
    (+ (floor (* (/ (- (send self :canvas-width) 150) 100) (+ 50 x))) 150)
    )
  (defmeth self :from-x-pixel (x)
    (- (* (/ (- x 150) (- (send self :canvas-width) 150)) 100) 50)
    )
;  (defmeth self :to-y-pixel (y)
;    (+ (floor (* (/ (- (send self :canvas-height) 150) 100) (+ 50 y))) 150)
;    )
;  (defmeth self :from-y-pixel (y)
;    (- (* (/ (- y 150) (- (send self :canvas-height) 150)) 100) 50)
;    )
  (send self :resize)
  (send self :redraw-graph)
  )

;;

(require "vertices")
(require "edges")
(require "tests")
(require "blocks")

(provide "draggraph")
