
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This adds methods for tests and model selection to
;;; the coco-graph-window-proto

(defmeth coco-graph-window-proto :maximal-stratum-of-edge (edge)
  (if (objectp edge)
      (max (list (mapcar
		  #'(lambda (vertex)
		      (if (send vertex :is-generator)
			  (max (mapcar
				#'(lambda (v) (send v :stratum))
				(send self :generator-to-vertices vertex)))
			(send vertex :stratum)))
		  (send edge :vertices))))
    (max (mapcar #'(lambda (vertex)
		     (send (if (objectp vertex) vertex
			     (nth vertex (send self :vertices))) :stratum))
		 edge)))
  )

;; Updated in "factor-graph":

(defmeth coco-graph-window-proto :string-to-vertices (string)
  (mapcar #'(lambda (char) (send self :vertex char))
	  (split-name-string string))
  )

(defun list-to-comma-string (list)
  (let ((result (car list)))
    (dolist (c (cdr list) result)
	    (setf result (concatenate 'list result (list #\,) c)))
    (concatenate 'string result))
  )

;;

;;; The method  :return-history-and-future is updated in "cgblocks.lsp":

(defmeth coco-graph-window-proto :return-history-and-future
  (block &key (redraw nil))
  (if (and block (send self :blocks))
      (let ((history nil)
	    (current nil)
	    (future nil))
	(mapcar #'(lambda (vertex use)
		    (if (< 0 use)
			(if (< (send vertex :stratum) block)
			    (setf history (concatenate
					   'list history
					   (list (send vertex :name))))
			  (if (> (send vertex :stratum) block)
			      (setf future (concatenate
					    'list future
					    (list (send vertex :name))))
			    (setf current (concatenate
					   'list current
					   (list (send vertex :name))))))))
		(send self :vertices)
		(slot-value 'use-variables))
	(list history current future)))
  )

(defmeth coco-graph-window-proto :return-history-model-nr
  (block &key (redraw nil) (current-model nil) (fix-history nil))
  (if (and block (send self :blocks))
      (let ((block-model nil)
	    (current-number (send self :return-model-number 'current)))
	(if (or current-model (send self :make-graph-current-model
				    :redraw-plots nil))
	    (let ((x (send self :return-history-and-future block)))
	      (if x
		  (let ((history (car x))
			(current (cadr x))
			(future (caddr x)))
		    (if (and fix-history history)
			(send self :and-fix-edges (to-string history "")))
		    (if history
			(progn
			  (if (send self :is-graphical 'current)
			      (send self :add-edges (to-string history "") T)
			    (send self :add-interactions
				  (to-string history "") T))
			  (send self :current)))
		    (if future
			(progn
			  (send self :drop-interactions
				(list-to-comma-string future) T)
			  (send self :current)
			  (if history
			      (progn
				(send self :make-current 'previous)
				(send self :dispose-of-model 'current)))
			  (send self :current))
		      (send self :current))
		    (send self :read-model;; ???
			  (send self :return-model 'current));; ???
		    (if (or future history)
			(progn
			  (send self :make-current 'previous)
			  (send self :dispose-of-model 'current)))
		    (send self :current)
		    (setf block-model
			  (send self :return-model-number 'current))
		    (if (if (boundp '*my-trace*) (>= *my-trace* 1) nil)
			(send self :return-block-trace-graph-window))))))
	(send self :make-current current-number)
	block-model))
  )

;;

;;; Updated in "factor-graph" /
;;; Method :test-remove-edge-from-current is updated in "cgblocks.lsp":

(defmeth coco-graph-window-proto :test-remove-edge-from-current
  (string edge &key (decomposable-mode nil))
  (if (send self :is-graphical 'current)
      (send self :drop-edges string T)
    (send self :drop-interactions string T))
  (send self :current)
  (let ((result
	 (if (send self :is-submodel-of)
	     (if (or (not decomposable-mode)
		     (send self :is-decomposable 'current))
		 (let ((test (send self :return-test-object nil nil)))
		   (if (and test (< 0 (send test :df)))
		       (progn
			 (format t "Testing edge: ~4a~%" string)
			 (send edge :width
			       (send self :p-to-width
				     (send self :select-p-value
					   test :print-test T)))
			 (send edge :dashed nil)
			 test)
		     'not-submodel-of-base))
	       'non-decomposable)
	   'not-submodel-of-base)))
    (send self :dispose-of-model 'current)
    result)
  )

;;; Update of method :test-remove-edge-from-current of "cocotest.lsp":

(defmeth coco-graph-window-proto :test-remove-edge-from-current
  (string edge &key (decomposable-mode nil))
  (if (send self :is-graphical 'current)
      (send self :drop-edges string T)
    (send self :drop-interactions string T))
  (send self :current)
  (let ((result
	 (if (send self :is-submodel-of)
	     (if (or (not decomposable-mode)
		     (send self :is-decomposable 'current))
		 (let ((test (send self :return-test-object nil nil)))
		   (if (and test (< 0 (send test :df)))
		       (progn
			 (format t "Testing edge: ~4a~%" string)
			 (send edge :width
			       (send self :p-to-width
				     (send self :select-p-value
					   test :print-test T)))
			 (send edge :dashed nil)
			 test)
		     'no-degrees-of-freedom))
	       'non-decomposable)
	   'not-submodel-of-base)))
    (send self :dispose-of-model 'current)
    result)
  )

(defmeth coco-graph-window-proto :test-edge
  (edge &key (block-model nil) (follow T) (decomposable-mode nil))
  (send edge :test
	(let ((current-number (send self :return-model-number 'current))
	      (base-number (send self :return-model-number 'base)))
	  (if (and (send self :make-graph-current-model :redraw-plots nil)
		   (if follow (send self :base) T))
	      (let ((result
		     (let* ((gc (send edge :edge-to-string)))
		       (if (send self :blocks)
			   (progn
			     (if block-model
				 (send self :make-current block-model)
			       (progn
				 (send self :return-history-model-nr
				       (send self :maximal-stratum-of-edge
					     edge))
				 (send self :current)))
			     (if follow (send self :base) T)))
		       (if (or (not decomposable-mode)
			       (or (not follow)
				   (and (send self :is-decomposable 'current)
					(send self :is-in-one-clique gc
					      'current))))
			   (send self :test-remove-edge-from-current gc edge
				 :decomposable-mode decomposable-mode)
			 'non-decomposable))))
		(if (and (send self :blocks) (not block-model))
		    (progn
		      (send self :current)
		      (send self :dispose-of-model 'current)))
		(send self :make-base base-number)
		(send self :make-current current-number)
		result)
	    (progn (send self :make-current current-number)
		   (send self :make-base base-number)
		   'no-current-model))))
  )

(defmeth coco-graph-window-proto :test-edge-nth
  (p &key (block-model nil) (follow T) (decomposable-mode nil))
  (send self :test-edge (nth p (send self :edges)) :block-model block-model
	:follow follow :decomposable-mode decomposable-mode)
  )

;;
(defmeth coco-graph-window-proto :test-add-to-current
  (string block-model &key (follow T) (decomposable-mode nil))
  (let ((n (if block-model block-model
	     (send self :return-model-number 'last))))
    (if (send self :is-graphical 'current)
	(send self :add-edges string T)
      (send self :add-interactions string T))
    (send self :current)
    (let ((result
	   (if (or (not (send self :blocks))
		   (equal (send self :return-model-set 'current)
			  (send self :return-model-set n)))
	       (if (or follow (send self :is-submodel-of))
		   (if (or (not decomposable-mode)
			   (send self :is-decomposable 'current))
		       (progn
			 (if follow
			     (progn
			       (send self :base)
			       (if (send self :blocks)
				   (if block-model
				       (send self :make-current block-model)
				     (send self :make-current 'previous))
				 (send self :make-graph-current-model
				       :redraw-plots nil))))
			 (let ((test (send self :return-test-object nil nil)))
			   (if (and test (< 0 (send test :df)))
			       (progn
				 (format t "Testing edge: ~4a~%" string)
				 test)
			     'not-submodel-of-base)))
		     'non-decomposable)
		 'not-submodel-of-base)
	     'future-edge)))
      (send self :current)
      (send self :dispose-of-model 'current)
      result))
  )

(defmeth coco-graph-window-proto :test-add-gc
  (gc vertex-list &key (block-model nil) (follow T) (decomposable-mode nil))
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base)))
    (if (send self :make-graph-current-model :redraw-plots nil)
	(let ((result
	       (progn
		 (if (send self :blocks)
		     (if block-model (send self :make-current block-model)
		       (progn
			 (send self :return-history-model-nr
			       (send self :maximal-stratum-of-edge vertex-list)
			       :current-model T)
			 (send self :current))))
		 (if (or (not decomposable-mode) (not follow)
			 (send self :is-decomposable 'current))
		     (send self :test-add-to-current gc block-model
			   :follow follow :decomposable-mode decomposable-mode)
		   'non-decomposable))))
	  (if (and (send self :blocks) (not block-model))
	      (progn (send self :current)
		     (send self :dispose-of-model 'current)))
	  (send self :make-base base-number)
	  (send self :make-current current-number)
	  result)
      (progn (send self :make-current current-number)
	     (send self :make-base base-number)
	     'no-current-model)))
  )

(defmeth coco-graph-window-proto :test-add-edge
  (vertex-pair &key (block-model nil) (follow T) (decomposable-mode nil))
  (let* ((vertex-list
	  (if (stringp vertex-pair)
	      (send self :string-to-vertices vertex-pair)
	    vertex-pair))
	 (gc (if (stringp vertex-pair)
		 vertex-pair
	       (send self :position-to-name
		     (car vertex-pair) (cadr vertex-pair)))))
    (send self :test-add-gc gc vertex-list :block-model block-model
	  :follow follow :decomposable-mode decomposable-mode))
  )

;;
(defmeth coco-graph-window-proto :draw-add-edge (i test p vertices)
  (let ((edge (send edge-proto :new  (list (nth (car i) vertices)
					   (nth (cadr i) vertices)))))
    (send edge :width (if p (send self :p-to-width p) 1))
    (send edge :dashed (if (not (objectp test)) test))
    (send edge :test test)
    (send edge :slot-value 'label-position '(0 0 0))
    (send edge :fix-label-position nil)
    (send self :draw-edge edge)
    (if (objectp test) (send self :draw-edge-label edge)))
  (send edge-proto :slot-value 'instances-edges
	(cdr (send edge-proto :slot-value 'instances-edges)))
  )

(defmeth coco-graph-window-proto :label-all-other-edges
  (&rest keyword-pairs
   &key (block-model nil) (print-test-for-edge nil)
	(p-accepted 0.10) (p-rejected 0.05)
	(coherent nil) (headlong nil) (random-order nil)
	(follow T) (most-significant T) (make-graph T)
;        (decomposable-mode nil)
   &allow-other-keys)
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base)))
    (if (and (send dynamic-coco-spin-proto :slot-value
		   'instances-dynamic-coco-spin)
	     follow)
	(progn 
	  (send self :make-graph-base-model :redraw-plots nil)
	  (if (not (send self :make-base 'previous))
	      (progn
		(format t
			"Please ignore above message:  ~
                         No model with that number~%")
		(send self :make-graph-base-model :redraw-plots nil)))))
    (send self :make-graph-current-model :redraw-plots T)
    (if block-model
	(send self :make-current block-model))
    (let ((rejected-edges nil)
	  (return-vertex-pair nil)
	  (p-value 1)
	  (non-fix-edges (send self :return-edge-list 'current
			       :edges 'not-in-model :fix 'non-fix-edges))
	  (accepted-edges (send self :accepted-edges)))
      (do ((non-graph-edges (if random-order
				(sample non-fix-edges (length non-fix-edges))
			      non-fix-edges)
			    (cdr non-graph-edges)))
	  ((or (not non-graph-edges) 
	       (and headlong (if (< p-value p-rejected) return-vertex-pair))))
	  (let ((i (car non-graph-edges)))
	    (if (or (not coherent)
		    (not (send self :vertex-pair-in-edge-list (car i) (cadr i)
			       accepted-edges)))
		(let* ((x (apply #'send self :test-add-edge
                                 (list (car i) (cadr i))
				:block-model block-model :follow follow
;				:decomposable-mode decomposable-mode
				:allow-other-keys t keyword-pairs))
		       (p (if (objectp x)
			      (send self :select-p-value x :print-test T))))
		  (send self :draw-add-edge i x p (send self :vertices))
		  (if (objectp x)
		      (progn
			(if (and coherent (< p-accepted p))
			    (send self :accepted-edges
				  (cons i (send self :accepted-edges))))
			(if (< p p-rejected)
			    (setf rejected-edges (cons i rejected-edges)))
			(if (< p p-value)
			    (progn (setf return-vertex-pair i)
				   (setf p-value p)))))))))
      (send self :make-current current-number)
      (send self :make-base base-number)
      (pause 120)
      (if make-graph
	  (if most-significant
	      (if return-vertex-pair
		  (send self :graph-add-edge
			(car return-vertex-pair) (cadr return-vertex-pair)))
	    (if rejected-edges
		(send self :graph-add-edge nil nil :edges rejected-edges)))
	(if most-significant
	    (if (< p-value p-rejected) return-vertex-pair)
	  rejected-edges))))
  )

;;
(defmeth coco-graph-window-proto :label-all-edges
  (&rest keyword-pairs
   &key (block-model nil) (print-test-for-edge nil)
	(p-accepted 0.10) (p-rejected 0.05)
	(coherent nil) (headlong nil) (random-order nil)
	(follow T) (least-significant T) (make-graph T)
;        (decomposable-mode nil)
   &allow-other-keys)
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base)))
    (if (and (send dynamic-coco-spin-proto :slot-value
		   'instances-dynamic-coco-spin)
	     follow)
	(progn 
	  (send self :make-graph-base-model :redraw-plots nil)
	  (if (not (send self :make-base 'previous))
	      (progn
		(format t "Please ignore above message:  ~
                         No model with that number~%")
		(send self :make-graph-base-model :redraw-plots nil)))))
    (send self :make-graph-current-model :redraw-plots T)
    (if block-model
	(send self :make-current block-model))
    (if follow (send self :base))
    (let ((accepted-edges nil)
	  (return-edge nil)
	  (p-value 0)
	  (non-fix-edges
	   (send self :return-edge-list 'current :fix 'non-fix-edges))
	  (all-edges (if block-model (send self :return-edge-list 'current)))
	  (rejected-edges (send self :rejected-edges)))
      (do ((graph-edges (if random-order
			    (sample (send self :edges)
				    (length (send self :edges)))
			  (send self :edges))
			(cdr graph-edges)))
	  ((or (not graph-edges)
	       (and headlong (if (> p-value p-accepted) return-edge))))
	  (let* ((edge (car graph-edges))
		 (u (send (car (send edge :vertices)) :index))
		 (v (send (cadr (send edge :vertices)) :index)))
	    (if (and (or (not coherent)
			 (not (send self :vertex-pair-in-edge-list u v
				    rejected-edges)))
		     (send self :vertex-pair-in-edge-list u v non-fix-edges))
		(let* ((x (apply #'send self :test-edge edge
				:block-model block-model :follow follow
;				:decomposable-mode decomposable-mode
                                :allow-other-keys t keyword-pairs))
		       (p (if (objectp x)
			      (send self :select-p-value x :print-test nil))))
		  (if (objectp x)
		      (progn
			(send self :start-buffering)
			(send self :redraw-graph)
			(send self :buffer-to-screen)
			(if (and coherent (< p p-rejected))
			    (send self :rejected-edges
				  (cons edge (send self :rejected-edges))))
			(if (< p-accepted p)
			    (setf accepted-edges (cons edge accepted-edges)))
			(if (< p-value p)
			    (progn (setf return-edge edge) (setf p-value p))))
		    (send edge :dashed x)))
	      (send edge :dashed
		    (if (send self :vertex-pair-in-edge-list u v non-fix-edges)
			'coherence
		      (if (and block-model
			       (not (send self :vertex-pair-in-edge-list u v
					  all-edges)))
			  'future-edge
			'fix-edge))))))
      (send self :make-current current-number)
      (send self :make-base base-number)
      (if make-graph
	  (if least-significant
	      (if return-edge
		  (send self :graph-drop-edge return-edge))
	    (if accepted-edges
		(send self :graph-drop-edge nil :edges accepted-edges)))
	(if least-significant
	    (if (> p-value p-accepted) return-edge)
	  accepted-edges))))
  )

;;
(defmeth coco-graph-window-proto :drop-least-significant-edge
  (&rest keyword-pairs
   &key (block-model nil) (recursive nil) (x-move 0)
;        (p-accepted 0.10) (p-rejected 0.05) (coherent nil) (headlong nil) 
;        (random-order nil) (follow T) (decomposable-mode nil)
;        (least-significant T)
        &allow-other-keys)
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base))
	(y self) (edge T))
    (loop
     (setf edge (apply #'send y :label-all-edges :block-model block-model
;		      :p-accepted p-accepted
;		      :p-rejected p-rejected
;		      :coherent coherent
;		      :headlong headlong
;		      :random-order random-order
;		      :follow follow
;		      :decomposable-mode decomposable-mode
;		      :least-significant least-significant
		      :make-graph nil :allow-other-keys t keyword-pairs))
     (if (send y :static)
	 (send y :location (- (- (car (send y :location)) x-move) 1)
	       (- (car (cdr (send y :location))) 1)))
     (if edge
	 (progn
	   (if (and recursive block-model)
	       (progn
		 (send y :make-current block-model)
		 (let ((arg
			(if (listp edge)
			    (send self :edge-list-to-string edge)
			  (send edge :edge-to-string))))
		   (if (send y :is-graphical 'current)
		       (send y :drop-edges arg T)
		     (send y :drop-interactions arg T)))
		 (send self :current)
		 (if (if (boundp '*my-trace*) (>= *my-trace* 2) nil)
		     (send self :return-block-trace-graph-window))
		 (send y :dispose-of-model 'number block-model)
		 (setf block-model (send y :return-model-number 'current))))
	   (setf y (if (listp edge)
		       (send y :graph-drop-edge nil
			     :edges edge :x-move x-move)
		     (send y :graph-drop-edge edge :x-move x-move))))
       (if block-model
	   (progn
	     ;; (format t "Please ignore: No model with that number \n")
	     (send y :dispose-of-model 'number block-model))))
     (if (not (and edge recursive)) (return y)))
    (send self :make-base base-number)
    (send self :make-current current-number) y)
  )

(defmeth coco-graph-window-proto :block-backward
  (&rest keyword-pairs
   &key (block nil) (recursive nil) (x-move 0) 
;       (p-accepted 0.10) (p-rejected 0.05) (coherent nil) (headlong nil)
;	(random-order nil) (follow T) (decomposable-mode nil)
;	(least-significant T)
        &allow-other-keys)
  (if (send self :static)
      (send self :location (- (- (car (send self :location)) x-move) 1)
	    (- (car (cdr (send self :location))) 1)))
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base))
	(y self)
	(blocks (if block (if (listp block) block (list (list nil block)))
		  (send self :blocks))))
    (do ((i blocks (cdr i))) ((not i) nil)
	(let ((fix (send y :return-fix 'edges))
	      (block-model (send y :return-history-model-nr
				 (if (objectp (car i))
				     (send (car i) :stratum)
				   (cadr (car i)))
				 :fix-history T)))
	  (if (send y :static)
	      (send y :location (- (+ (car (send y :location)) x-move) 1)
		    (- (car (cdr (send y :location))) 1)))
	  (setf y (apply #'send y :drop-least-significant-edge
			:block-model block-model
;			:recursive recursive
;			:p-accepted p-accepted
;			:p-rejected p-rejected
;			:coherent coherent
;			:random-order random-order
;			:headlong headlong
;			:follow follow
;			:decomposable-mode decomposable-mode
;			:least-significant least-significant
;			:x-move x-move
                        :allow-other-keys t keyword-pairs))
	  (send y :fix-edges fix)))
    (send self :make-base base-number)
    (send self :make-current current-number) y)
  )

;;
(defmeth coco-graph-window-proto :add-most-significant-edge
  (&rest keyword-pairs
   &key (block-model nil) (recursive nil) (x-move 0) 
;	(p-accepted 0.10) (p-rejected 0.05) (coherent nil) (headlong nil)
;	(random-order nil) (follow T) (decomposable-mode nil)
;	(most-significant T)
        &allow-other-keys)
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base))
	(y self) (edge T))
    (loop
     (setf edge (apply #'send y :label-all-other-edges
		      :block-model block-model
;		      :p-accepted p-accepted
;		      :p-rejected p-rejected
;		      :coherent coherent
;		      :headlong headlong
;		      :random-order random-order
;		      :follow follow
;		      :decomposable-mode decomposable-mode
;		      :most-significant most-significant
		      :make-graph nil :allow-other-keys t keyword-pairs))
     (if (send y :static)
	 (send y :location (- (- (car (send y :location)) x-move) 1)
	       (- (car (cdr (send y :location))) 1)))
     (if edge
	 (progn
	   (if (and recursive block-model)
	       (progn
		 (send y :make-current block-model)
		 (let ((arg (if (listp (car edge))
				(send self :vertex-lists-to-string edge)
			      (if (listp  edge)
				  (send self :edge-to-string edge)
				(send edge :edge-to-string)))))
		   (if (send y :is-graphical 'current)
		       (send y :add-edges arg T)
		     (send y :add-interactions arg T)))
		 (send self :current)
		 (if (if (boundp '*my-trace*) (>= *my-trace* 2) nil)
		     (send self :return-block-trace-graph-window))
		 (send y :dispose-of-model 'number block-model)
		 (setf block-model (send y :return-model-number 'current))))
	   (setf y (if (listp (car edge))
		       (send y :graph-add-edge
			     nil nil :edges edge :x-move x-move)
		     (send y :graph-add-edge
			   (car edge) (cadr edge) :x-move x-move))))
       (if block-model
	   (progn
	     ;; (format t "Please ignore: No model with that number \n")
	     (send y :dispose-of-model 'number block-model))))
     (if (not (and edge recursive)) (return y)))
    (send self :make-base base-number)
    (send self :make-current current-number)
    y)
  )

(defmeth coco-graph-window-proto :block-forward
  (&rest keyword-pairs
   &key (block nil) (recursive nil) (x-move 0)
;       (p-accepted 0.10) (p-rejected 0.05) (coherent nil) (headlong nil)
;       (random-order nil) (follow T) (decomposable-mode nil)
;	(most-significant T)
        &allow-other-keys)
  (if (send self :static)
      (send self :location (- (- (car (send self :location)) x-move) 1)
	    (- (car (cdr (send self :location))) 1)))
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base))
	(y self)
	(blocks (if block (if (listp block) block (list (list nil block)))
		  (send self :blocks))))
    (do ((i blocks (cdr i))) ((not i) nil)
	(let ((fix (send y :return-fix 'edges))
	      (block-model (send y :return-history-model-nr
				 (if (objectp (car i))
				     (send (car i) :stratum)
				   (cadr (car i))) :fix-history T)))
	  (if (send y :static)
	      (send y :location (- (+ (car (send y :location)) x-move) 1)
		    (- (car (cdr (send y :location))) 1)))
	  (setf y (apply #'send y :add-most-significant-edge
			:block-model block-model
;			:recursive recursive
;			:p-accepted p-accepted
;			:p-rejected p-rejected
;			:coherent coherent
;			:headlong headlong
;			:random-order random-order
;			:follow follow
;			:decomposable-mode decomposable-mode
;			:most-significant most-significant
;			:x-move x-move
                        :allow-other-keys t keyword-pairs))
	  (send y :fix-edges fix)))
    (send self :make-base base-number)
    (send self :make-current current-number) y)
  )


(provide "cocotest")
