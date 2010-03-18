
(def x (make-coco))

(print (send x :set-specification-file "/user/jhb/CoCo/source/COCO.DAT" )) 
(print (send x :read-specification ))
;;; (print (send x :read-factors ))
;;; (print (send x :read-names ))

(print (send x :skip-missing))

(print (send x :set-observations-file  "/user/jhb/CoCo/source/COCO.DAT" )) 
(print (send x :read-observations ))

;;; (print (send x :read-table ))
;;; (print (send x :read-list ))

;;; (print (send x :enter-names-and-list ... ))
;;; (print (send x :enter-names-and-list-as-table ... ))
;;; (print (send x :enter-list ... ))
;;; (print (send x :enter-list-as-table ... ))

;;; (print (send x :enter-table (list ...) ))

(print (send x :read-model "ACE,ADE,BC,F" ))

(print (send x :return-vector 'observed))
(print "Substitute")
(print (send x :substitute ))
(print "Substitute")
(print (send x :return-vector 'observed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (send x :set-data-file          "/user/jhb/CoCo/source/COCO.DAT" )) 
(print (send x :read-data ))

(print (send x :exclude-missing 'on ))
(print (send x :exclude-missing 'what ))
(print (send x :exclude-missing 'off ))
(print (send x :exclude-missing 'what ))
(print (send x :exclude-missing 'flop ))
(print (send x :exclude-missing 'what ))
(print (send x :exclude-missing 'in "*"))
(print (send x :exclude-missing 'what ))

(print (send x :em-on))
(print (send x :em-on 'what))
