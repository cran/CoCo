
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the independence-proto


(defproto independence-proto '(title) '(instances-independence))

(send independence-proto  :documentation
      'proto "Independence data (model) prototype based on the basic prototype")

(defmeth independence-proto :isnew (&key title)
  (if title (send self :title title))
  (send independence-proto :slot-value 'instances-independence
	(cons self (slot-value 'instances-independence)))
  )

(provide "independence")
