
(load "tstinit")

;;; (print (send x :read-q-table "..." (list ...) ))
;;; (print (send x :read-q-list "..." (list ...) ))

;;; (print (send x :enter-q-table "AB." (list 1 0 0 0) ))
(print (send x :enter-q-table "AB." (list 0 1 2 0) ))
(send x :return-vector 'zero "AB")
(send x :print-table 'zero "AB")

(print (send x :enter-q-table "EF." (list 1 0 0 0) ))
(send x :return-vector 'zero "EF")
(send x :print-table 'zero "EF")

(print (send x :enter-q-list "CD." (list 1 1) ))
(send x :return-vector 'zero "CD")
(send x :print-table 'zero "CD")

(print (send x :enter-q-list "CD." (list 2 2) ))
(send x :return-vector 'zero "CD")
(send x :print-table 'zero "CD")

(send x :print-table 'zero "AB")
(send x :print-table 'zero "EF")
(send x :print-table 'zero "CD")

(send x :status 'specification)
(send x :status 'data)

(send x :read-model "*")
(send x :read-model "ACE,ADE,BC,F")

(send x :return-vector 'observed )
(send x :return-vector 'observed "*" :complete T)

(send x :return-vector 'zero)

(send x :print-table  'observed "*" :complete T)
(send x :print-table  'zero "*")

(print (send x :clean-data ))

(print (send x :dispose-of-q-table "AB" ))
(print (send x :dispose-of-all-q-tables ))
