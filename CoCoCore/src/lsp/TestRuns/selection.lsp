
(def x (make-coco))

(send x :enter-names "ABCDEF" '(2 2 2 2 2 2) '(0 0 0 0 0 0))

(print (send x :select-cases     "AB" (list 1 1)))
(print (send x :or-select-cases  "CD" (list 1 1)))
(print (send x :reject-cases     "EF" (list 0 0)))
(print (send x :or-reject-cases  "AB" (list 0 0)))
(send x :status 'specification)

;;; (print (send x :select-cases     'what))
;;; (print (send x :or-select-cases  'what))
;;; (print (send x :reject-cases     'what))
;;; (print (send x :or-reject-cases  'what))

(print (send x :redefine-factor "A" 1 1))
(print (send x :cutpoints "A" (list .5)))

(print (send x :redefine-factor "A" 2 0))
(print (send x :cutpoints "A" (list .5)))

(print (send x :redefine-factor "A" 2))
(print (send x :cutpoints "A" (list .5)))
(print (send x :cutpoints "A" 'what))
(print (send x :cutpoints "B" 'what))
(print (send x :cutpoints "W" 'what))
