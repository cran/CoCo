
(defun mymember (a b)
  (do ((i b (cdr i))) ((or (not i) (equalp a (car i))) i)))

(defmeth coco-graph-window-proto :try-edge (edge result &optional (alpha 0.05))
  (send self :drop-edges edge T)
  (send self :current)
  (let ((test (if (boundp 'test-proto)
		  (send self :return-test-object nil nil)
		(send self :compute-test nil nil))))
    (send self :make-current 'base)
    (if (and test (< 0 (if (objectp test)
			   (send test :df) (nth 1 (car test)))))
	(progn
	  (format t "Testing edge: ~4a~%" edge)
	  (if (> (send self :select-p-value test :print-test T) alpha)
	      (if (mymember edge result) result
		(concatenate 'list result (list edge)))
	    result))
      result))
  )

(defmeth coco-graph-window-proto :kreinersearch (&optional (alpha 0.05))
  (let* ((names (send self :names))
	 (names (if (listp (car names)) (mapcar #'(lambda (i) (car i)) names)
		  names))
	 (result nil)
	 (current-number (send self :return-model-number 'current))
	 (base-number (send self :return-model-number 'base)))
    (do ((i names (cdr i))) ((< (length i) 3) result)
	(do ((j (cdr i) (cdr j))) ((< (length j) 2) result)
	    (do ((k (cdr j) (cdr k))) ((< (length k) 1) result)
		(send self :read-model
		      (to-string (list (car i) (car j) (car k)) ""))
		(send self :base)
		(setf result (send self :try-edge
				   (to-string (list (car i) (car j)) "")
				   result alpha))
		(setf result (send self :try-edge
				   (to-string (list (car i) (car k)) "")
				   result alpha))
		(setf result (send self :try-edge
				   (to-string (list (car j) (car k)) "")
				   result alpha)))))
    (send self :read-model "*")
    (send self :current)
    (send self :base)
    (send self :drop-edges (to-string result ",") T)
    (if (kind-of-p self drag-graph-proto)
	(send self :return-child-coco-graph-window
	      :model (send self :return-model-number 'last)))
    (send self :make-current current-number)
    (send self :make-base base-number)
    result)
  )