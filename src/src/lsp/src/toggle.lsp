
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This creates linked toggle items and switch controls:


;;; Toggle item

(defproto linked-toggle-item-proto
  '(graph key) '(instances-toggle-items) (list toggle-item-proto))

(send linked-toggle-item-proto :documentation 'proto
      "linked-toggle-item-proto prototype based on toggle-item-proto")

(defmeth linked-toggle-item-proto :isnew (text graph key &key (value nil)
					   (action nil))
  (call-method toggle-item-proto :isnew text :value value :action action)
  (send self :slot-value 'graph graph)
  (send self :slot-value 'key key)
  (send linked-toggle-item-proto :slot-value 'instances-toggle-items
	(cons self (slot-value 'instances-toggle-items)))
  self
  )

(defmeth linked-toggle-item-proto :graph (&optional (val nil set))
  (if set (slot-value 'graph set))
  (slot-value 'graph)
  )

(defmeth linked-toggle-item-proto :key (&optional (val nil set))
  (if set (slot-value 'key set))
  (slot-value 'key)
  )

(defmeth linked-toggle-item-proto :equal (graph key)
  (and (or (not (send self :graph))
	   (if (objectp (send self :graph))
	       (equalp graph (send self :graph))
	     (equalp graph (send graph :slot-value 'identification))))
       (equalp key (send self :key)))
  )

(defmeth linked-toggle-item-proto :update (graph key &optional (val nil set))
  (if (boundp 'linked-toggle-item-proto)
      (dolist (i (send linked-toggle-item-proto :slot-value
		       'instances-toggle-items))
	      (if (and (equalp graph (send i :graph))
		       (equalp key (send i :key)))
		  (send i :value val))))
  (if (boundp 'linked-switch-control-proto)
      (dolist (i (send linked-switch-control-proto :slot-value
		       'instances-switch-controls))
	      (if (send i :equal graph key)
		  (send i :redraw))))
  )

;;; Switch control

(defproto switch-control-proto '(action) () graph-control-proto)

(defmeth switch-control-proto :isnew (&key (location (list 10 10))
					   (title nil) (key nil)
					   (action nil))
  (call-next-method :location location :title title)
  (send self :slot-value 'action action)
  )

(defmeth switch-control-proto :redraw ()
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (second loc))
         (title (send self :title)))
    (send self :erase)
    (send graph :frame-rect loc-x (+ 5 loc-y) 10 10)
    (if (send self :rect-on)
	(send graph :paint-rect (+ 1 loc-x) (+ 6 loc-y) 8 8)
      (send graph :erase-rect (+ 1 loc-x) (+ 6 loc-y) 8 8))
    (send graph :draw-text title (+ 15 loc-x) (+ 15 loc-y) 0 0)))

(defmeth switch-control-proto :do-click (x y a b)
  (let* ((graph (send self :graph))
         (loc (send self :location))
         (loc-x (first loc))
         (loc-y (+ 5 (second loc))))
    (when (and (< loc-x x (+ loc-x 10)) (< loc-y y (+ loc-y 10)))
          (send self :do-action (list a b))
          (send graph :while-button-down
                #'(lambda (x y) (send self :do-action nil)) nil)
          t)))

(defmeth switch-control-proto :do-action (first) 
  (if first (send self :user-action))
  (send self :redraw)
  )

(defmeth switch-control-proto :rect-on (&optional (val nil set))
  (if (slot-value 'action)
      (funcall (slot-value 'action) (send self :graph)))
  )

(defmeth switch-control-proto :user-action ()
  (if (slot-value 'action)
      (funcall (slot-value 'action) (send self :graph)
	       (not (funcall (slot-value 'action) (send self :graph))))
;;;    (send self :rect-on (not (send self :rect-on)))
    )
  )


(defproto linked-switch-control-proto
  '(key) '(instances-switch-controls) switch-control-proto)

(defmeth linked-switch-control-proto :isnew (&key (location (list 10 280))
						  (title nil) (key nil)
						  (action nil))
  (call-next-method :location location :title title :action action)
  (send self :slot-value 'key key)
  (send linked-switch-control-proto :slot-value 'instances-switch-controls
	(cons self (slot-value 'instances-switch-controls)))
  )

(defmeth linked-switch-control-proto :key (&optional (val nil set))
  (if set  (slot-value 'key set))
  (slot-value 'key)
  )

(defmeth linked-switch-control-proto :do-action (first) 
  (if first
      (send self :update (not (send self :rect-on))))
  (call-next-method first)
  )

(defmeth linked-switch-control-proto :equal (graph key)
  (and (or (not graph)
	   (if (objectp graph)
	       (equalp graph (send self :graph))
	     (equalp graph (send (send self :graph)
				 :slot-value 'identification))))
       (equalp key (send self :key)))
  )

(defmeth linked-switch-control-proto :update (&optional (val nil set))
#|
  (if (boundp 'linked-toggle-item-proto)
      (dolist (i (send linked-toggle-item-proto :slot-value
		       'instances-toggle-items))
	      (if (send i :equal (send self :graph) (send self :key))
		  (send i :value val))))
|#
  )
