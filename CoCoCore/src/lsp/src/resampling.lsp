
;;; Copyright 1993 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This ... is for resampling


(defmeth coco-proto :resampling-backward
  (case-list &optional (proportion 0.90) (times 100) &key (replacement nil)
	     (only nil) (reversed nil) (sorted nil) (short T)
	     (headlong T) (recursive T) (coherent T) (follow T)
	     (decomposable-mode T) (least-significant T) (separators nil)
	     (edges nil) (edge-lists nil))
  (let* ((case-list (if (listp case-list) case-list (coerce case-list 'list)))
         (number-of-cases (round (* proportion (length case-list)))))
    (send self :set-switch 'decomposable-mode (if decomposable-mode 'on 'off))
    (dotimes (i times edge-lists)
             (send self :enter-list
                   (element-seq
                    (sample case-list number-of-cases replacement)))
             (send self :backward
                   :only only :reversed reversed :sorted sorted :short short
                   :headlong headlong :recursive recursive :coherent coherent
                   :follow follow :least-significant least-significant
                   :separators separators :edges edges)
             (setf edge-lists
		   (concatenate 'list edge-lists
				(list (send self :return-edge-list-list
					    'last)))))
    (send self :enter-list (element-seq case-list))
    (if (not (send self :has-slot 'resampling-edges))
	(send self :add-slot 'resampling-edges edge-lists)
      (send self :slot-value 'resampling-edges edge-lists))
    edge-lists)
  )

(defun edge-lists-to-matrix (edge-lists &optional (n nil))
  (let* ((n (if n n (1- (max edge-lists))))
         (result (repeat 0 (/ (* n (1- n)) 2))))
    (dolist (i edge-lists result)
            (dolist (j (split-list i 2) result)
                    (let ((index (+ (/ (* (cadr j) (1- (cadr j))) 2) (car j))))
                      (setf (nth index result) (1+ (nth index result)))))))
  )

;;;(defmeth coco-graph-window-proto :set-edge-widths (edge-matrix)
;;;  (send self :add-slot 'resampling-backward edge-matrix)
;;;  (dolist (j (send self :edges) nil)
;;;          (let ((index (+ (/ (* (cadr j) (1- (cadr j))) 2) (car j))))
;;;            (setf (car (cdr (cdr j))) 
;;;                  (round (/ (* 20 (nth index edge-matrix)) (max edge-matrix)))
;;;                  )))
;;;  (send self :start-buffering)
;;;  (send self :redraw-graph)
;;;  (send self :buffer-to-screen)
;;;  )

(defmeth coco-graph-window-proto :set-edge-widths (edge-matrix)
  (send self :add-slot 'resampling-backward edge-matrix)
  (dolist (j (send self :edges) nil)
          (let* ((vertices (send j :vertices))
		 (a (send (car vertices) :index))
		 (b (send (cadr vertices) :index))
		 (index (+ (/ (* b (1- b)) 2) a)))
            (send self :edge-width j
                  (round (/ (* 40 (nth index edge-matrix))
                            (max edge-matrix))))))
  (send self :start-buffering)
  (send self :redraw-graph)
  (send self :buffer-to-screen)
  )

(defmeth coco-graph-window-proto :resampling-backward
  (case-list &optional (proportion 0.90) (times 100) &key (replacement nil)
	     (only nil) (reversed nil) (sorted nil) (short T)
	     (headlong T) (recursive T) (coherent T) (follow T)
	     (decomposable-mode T) (least-significant T) (separators nil)
	     (edges nil) (edge-lists nil))
  (let* ((case-list (if (listp case-list) case-list (coerce case-list 'list)))
         (n-of-variables (length (send self :return-names)))
         (number-of-cases (round (* proportion (length case-list)))))
    (send self :set-switch 'decomposable-mode (if decomposable-mode 'on 'off))
    (dotimes (i times edge-lists)
             (send self :enter-list
                   (element-seq
                    (sample case-list number-of-cases replacement)))
             (send self :backward
                   :only only :reversed reversed :sorted sorted :short short
                   :headlong headlong :recursive recursive :coherent coherent
                   :follow follow :least-significant least-significant
                   :separators separators :edges edges)
             (setf edge-lists
		   (concatenate 'list edge-lists
				(list (send self :return-edge-list-list
					    'last))))
             (send self :set-edge-widths (edge-lists-to-matrix
					  edge-lists n-of-variables)))
    (send self :enter-list (element-seq case-list))
    (if (not (send self :has-slot 'resampling-edges))
	(send self :add-slot 'resampling-edges edge-lists)
      (send self :slot-value 'resampling-edges edge-lists))
    edge-lists)
  )


;;; A function for returning a list of cases corresponding
;;; to one case in each cell:

(defun return-table-indices (x)
  (let ((pred 1)
        (post (prod x))
        (result nil))
    (dolist (i x result)
            (setf post (/ post i))
            (setf result (concatenate
                          'list result (list
                                        (repeat (repeat (iseq i) post)
                                                (repeat pred (* i post))))))
            (setf pred (* pred i)))
;;;    (split-list
;;;     (coerce (make-array (* (length x) pred) :displaced-to
;;;			 (transpose (make-array (list (length x) pred)
;;;						:initial-contents result)))
;;;	     'list) (length x)))
    (array-to-nested-list (transpose (make-array (list (length x) pred)
						 :initial-contents result))))
  )


;;; Return from the table of counts a list of cases:

(defmeth coco-proto :return-case-list ()
  (split-list
   (1+ (repeat
	(return-table-indices (send self :return-level-list))
	(round (send self :return-vector 'observed "*"))))
   (length (send self :return-level-list)))
  )


;;; (For small tables with many cases it is probably more efficient to
;;; do the simulation on the table of counts. But this is not implemented.)


(provide "resampling")
