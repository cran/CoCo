
;;; Dear Søren,

;;; You can then create your own new proto-types, specializations of
;;; coco-graph-window-proto be the following few lines (can be reduced
;;; to approx. 7 lines including the :return-... method).
;;; The new prototypes will:
;;;
;;;  1) Have all slots of coco-graph-window-proto,
;;;  2) These slots will be initiated correctly, also in (most)
;;;     future versions of my code,
;;;  3) The new prototype will know and handle correctly all methods of
;;;      coco-graph-window-proto.
;;;
;;; 

;;; / jhb, 20. January 2000


;;; The split-root-proto:

(defproto split-root-proto
  '(complete-set) '(
;;                    instances-split-root
                    ) (list coco-graph-window-proto))

(send split-root-proto :documentation
      'proto "A prototype for a CoCo Graph Window specialized to the root of a split.")

(defmeth split-root-proto :isnew (identification &key location title)
 (let ((new-object (call-method coco-graph-window-proto :isnew identification
           :location location :title title)))
;;  (send split-root-proto :slot-value 'instances-split-root
;;   (cons new-object (slot-value 'instances-split-root)))
  (send new-object :slot-value 'identification identification)
  new-object)
 )

(defmeth coco-graph-window-proto :return-split-root (&rest keyword-pairs)
 (apply #'send self :return-child-coco-graph-window
        :proto-type split-root-proto :child T :allow-other-keys t keyword-pairs))


;;; The split-leaf-proto:

(defproto split-leaf-proto
  '(some-other-slots) '(
;;                        instances-split-leaf
                        ) (list split-root-proto))

(send split-leaf-proto :documentation
      'proto "A prototype for a CoCo Graph Window specialized to the leaf of a split.")

(defmeth split-leaf-proto :isnew (identification &key location title)
 (let ((new-object (call-method split-root-proto :isnew identification
           :location location :title title)))
;;  (send split-leaf-proto :slot-value 'instances-split-leaf
;;   (cons new-object (slot-value 'instances-split-leaf)))
  (send new-object :slot-value 'identification identification)
  new-object)
 )

(defmeth split-root-proto :return-split-leaf (&rest keyword-pairs)
 ;; When doing a split, you will of cource not do this
 (apply #'send self :return-child-coco-graph-window
        :proto-type split-leaf-proto :child T :allow-other-keys t keyword-pairs)
 ;; but create new coco-objects, and return graphs for these.
 )
