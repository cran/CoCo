
(defun format-invalid (x)
  (if (eq x #.NOT-A-NUMBER) nil x)
  )

(defun invalid-real (x)
  (or (eq x #.NOT-A-NUMBER)
   (not (< x #.POSITIVE-INFINITY))
;;   (< (abs (/ (- x *my-not-a-number*) *my-not-a-number*)) 0.0001)
   )
  )

(defun replace-my-not-a-number (real-list)
  (setf last-real-list real-list)
  (mapcar #'(lambda (i) (if (invalid-real i) 9999999 i))
   ;; #.NOT-A-NUMBER
   ;; #.POSITIVE-INFINITY
   real-list)
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
			(z (if (eq 'GENERATOR type) 0
			     (if (or (eq 6 type) (eq 'GC type)) 20
			       (if (or (eq 7 type) (eq 'LINEAR type)
				       (eq 'LINEAR-GENERATOR type)) 40
				 (if (or (eq 8 type) (eq 'DISCRETE type)
					 (eq 'DISCRETE-GENERATOR type)) -20
				   (if (or (eq 9 type) (eq 'QUADRATIC type)
					   (eq 'QUADRATIC-GENERATOR type)) -40 0))))))
			(xyz (if (eq 0 (caddr position))
				 (list (car position) (cadr position) z) position)))
;		   (print type)
		   (send generator :position xyz))))
	   )) a))
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

; (trace :update-factor-positions)

; (trace :return-gcs)

; (trace :type)

;(trace :POSITION)
;(trace :LABEL-POSITION)
;(trace :PROJECT)
;(trace :FIX-LABEL-POSITION)
;(trace :DRAW-VERTEX-AND-LABEL)
