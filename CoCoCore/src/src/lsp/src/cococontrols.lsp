
;;; Copyright 1992, 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This is a version of :add-controls for the coco-graph-window-proto


(require "plotcontrols")
(require "toggle")

(defmeth spin-rock-control-proto :isnew (&key (location (list 10 70)))
  (call-next-method :location location :title "Rock Plot"))

(defmeth spin-speed-control-proto :isnew (&key (points 21)
					       (location (list 10 90)))
  (call-next-method (rseq 0 .2 points)
		    :index 1 :location location :title "Speed"))


;;; Rotation graph around axes

(defproto rotate-graph-control-proto '(v) () rocker-control-proto)

(defmeth rotate-graph-control-proto :isnew
  (v &key (location (list 10 (case v (0 10) (1 30) (2 50)))))
  (call-next-method :v v :location location))

(defmeth rotate-graph-control-proto :variable-label (c)
  (case c (0 "Pitch") (2 "Roll") (1 "Yaw")))

(defmeth rotate-graph-control-proto :title ()
  (send self :variable-label (slot-value 'v)))

(defmeth rotate-graph-control-proto :do-action (first sign)
  (let ((graph (send self :graph)))
    (if first
        (let* ((v (slot-value 'v))
               (v1 (if (= v 0) 1 0))
               (v2 (if (= v 2) 1 2))
               (cols (column-list (identity-matrix 3)))
               (angle (send graph :angle)))
          (send graph :idle-on (car first))
          (send graph :slot-value 'rotation-type
                (make-rotation (nth v1 cols) (nth v2 cols) 
                               (case sign (+ angle) (- (- angle)))))))
    (send graph :apply-transformation (send graph :slot-value 'rotation-type)))
  )


;;; Rescale graph

(defproto rescale-graph-control-proto '() () rocker-control-proto)

(defmeth rescale-graph-control-proto :isnew (&key (location (list 10 240)))
  (call-next-method :location location))

(defmeth rescale-graph-control-proto :title ()
  "Rescale"
  )

(defmeth rescale-graph-control-proto :do-action (first sign)
  (let ((graph (send self :graph))
	(angle (send (send self :graph) :angle)))
    (case sign
	  (+ (send graph :rescale-vertex-positions :scale (+ 1 angle)))
	  (- (send graph :rescale-vertex-positions :scale (/ 1 (+ 1 angle)))))
    (case sign
	  (+ (send graph :rescale-block-positions :scale (+ 1 angle)))
	  (- (send graph :rescale-block-positions :scale (/ 1 (+ 1 angle)))))
    (send graph :redraw-graph)
    )
  )


;;; Undo graph

(defproto undo-graph-control-proto '() () rocker-control-proto)

(defmeth undo-graph-control-proto :isnew (&key (location (list 10 260)))
  (call-next-method :location location))

(defmeth undo-graph-control-proto :title ()
  "Un/Redo"
  )

(defmeth undo-graph-control-proto :do-action (first sign)
  (let ((graph (send self :graph)))
    (case sign
	  (+ (send graph :redo-move))
	  (- (send graph :undo-move)))
    (send graph :redraw-graph)
    )
  )




;;; Current Control

(defproto current-control-proto '() '(instances-current-control)
  switch-control-proto)

(defmeth current-control-proto :rect-on ()
  (equal (send (send self :graph) :return-model-number 'current)
	 (send (send self :graph) :slot-value 'model-number))
  )

(defmeth current-control-proto :isnew (&key (location (list 10 120)))
  (call-next-method :location location :title "Current")
  (send current-control-proto :slot-value 'instances-current-control
	(cons self (slot-value 'instances-current-control)))
  )

(defmeth current-control-proto :do-action (first) 
  (send (send self :graph) :make-graph-current-model :redraw-plots T)
  (send self :redraw)
  )


;;; Base Control

(defproto base-control-proto '()  '(instances-base-control)
  switch-control-proto)

(defmeth base-control-proto :rect-on ()
  (equal (send (send self :graph) :return-model-number 'base)
	 (send (send self :graph) :slot-value 'model-number))
  )

(defmeth base-control-proto :isnew (&key (location (list 10 140)))
  (call-next-method :location location :title "Base")
  (send base-control-proto :slot-value 'instances-base-control
	(cons self (slot-value 'instances-base-control)))
  )

(defmeth base-control-proto :do-action (first) 
  (send (send self :graph) :make-graph-base-model :redraw-plots T)
  (send self :redraw)
  )


;;;; Installation method

(defmeth coco-graph-window-proto :add-controls ()
  (apply #'send self :size (+ (send self :size) '(150 0)))
  (defmeth self :to-x-pixel (x)
    (+ (floor (* (/ (- (send self :canvas-width) 150) 100) (+ 50 x))) 150)
    )
  (defmeth self :from-x-pixel (x)
    (- (* (/ (- x 150) (- (send self :canvas-width) 150)) 100) 50)
    )

;;;  (defmeth self :to-y-pixel (y)
;;;    (+ (floor (* (/ (- (send self :canvas-height) 150) 100) (+ 50 y))) 150)
;;;    )
;;;  (defmeth self :from-y-pixel (y)
;;;    (- (* (/ (- y 150) (- (send self :canvas-height) 150)) 100) 50)
;;;    )

  (send self :resize)

  (send self :add-overlay (send current-control-proto
				:new :location (list  10 10)))
  (send self :add-overlay (send base-control-proto
				:new :location (list 120 10)))
  
  (send self :add-overlay
	(send linked-switch-control-proto :new
	      :key 'drag-graph :location (list 10 40) :title "Drag-Graph"
	      :action #'(lambda (graph &optional (val nil set))
			  (if set
			      (send graph :drag-graph val)
			    (send graph :drag-graph)))))

  (send self :add-overlay
	(send linked-switch-control-proto :new
	      :key 'edit-graph :location (list 10 60) :title "Edit-Graph"
	      :action #'(lambda (graph &optional (val nil set))
			  (if set
			      (send graph :edit-graph val)
			    (send graph :edit-graph)))))

  (send self :add-overlay
	(send linked-switch-control-proto :new
	      :key 'static :location (list 10 80) :title "Static"
	      :action #'(lambda (graph &optional (val nil set))
			  (if set
			      (send graph :static val)
			    (send graph :static)))))

  (send self :add-overlay
	(send linked-switch-control-proto :new
	      :key 'grid :location (list 10 100) :title "Grid"
	      :action #'(lambda (graph &optional (val nil set))
			  (if set
			      (send graph :grid val)
			    (send graph :grid)))))
  (send self :add-overlay
	(send linked-switch-control-proto :new
	      :key 'exact-test :location (list 10 280) :title "Exact tests"
	      :action #'(lambda (graph &optional (val nil set))
			  (if set
			      (send graph :set-exact-test val)
			    (send graph :set-exact-test)))))

  (send self :add-overlay (send rotate-graph-control-proto
				:new 0 :location (list  10 125)))
  (send self :add-overlay (send rotate-graph-control-proto
				:new 1 :location (list  10 145)))
  (send self :add-overlay (send rotate-graph-control-proto
				:new 2 :location (list  10 165)))
  (send self :add-overlay (send spin-rock-control-proto
				:new :location (list  10 185)))
  (send self :add-overlay (send spin-speed-control-proto
				:new :location (list  10 205)))
  (send self :add-overlay (send rescale-graph-control-proto
				:new :location (list  10 235)))
  (send self :add-overlay (send undo-graph-control-proto
				:new :location (list  10 255)))

  (send self :redraw-graph)
  )


(provide "cococontrols")
