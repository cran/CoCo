
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the association-diagram-proto
;;; and adds the basic methods to this proto


(require "independence")
(require "draggraph")
(require "vertices")
(require "edges")
(require "tests")
(require "blocks")

(defproto association-diagram-proto
  '(rejected-edges accepted-edges) '(instances-graph)
  (list drag-graph-proto independence-proto))

(send association-diagram-proto :documentation
      'proto "Independence Graph graphics window prototype based on~
              basic graphics window prototype")

(defmeth association-diagram-proto :isnew (&key location title)
  (let ((x (apply #'call-next-method
		  :location location :title (list title))))
    (send association-diagram-proto :slot-value 'instances-graph
	  (cons x (slot-value 'instances-graph)))
    x)
  )



;
(defmeth association-diagram-proto :rejected-edges (&optional (val nil set))
  (if set (setf (slot-value 'rejected-edges) val))
  (slot-value 'rejected-edges)
  )

(defmeth association-diagram-proto :accepted-edges (&optional (val nil set))
  (if set (setf (slot-value 'accepted-edges) val))
  (slot-value 'accepted-edges)
  )

(provide "association-diagram")
