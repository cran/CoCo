
(load "tstinit.lsp")

(print (send x :read-model "*"))
(print (send x :read-n-interactions 1))
(print (send x :read-n-interactions 2 "ABCD"))
(print (send x :read-n-interactions 3 "ABCDE"))
(print (send x :read-n-interactions 4 "*"))

(print (send x :read-model "ACDE,ABCF"))
(print (send x :base))
(print (send x :read-model "ACE,ADE,BC,F"))

(print (send x :print-common-decompositions))
(print (send x :decompose-models "BC"))

(print (send x :test))
(print (send x :find-log-l))
(print (send x :find-deviance))

(print (send x :compute-test))
(print-test (send x :compute-test))
(print (send x :compute-deviance))
(print-deviance (send x :compute-deviance))

(print (send x :exact-test))
(print (send x :partitioning-test))
(print (send x :test-one-edge))
(print (send x :factorize 'edges))
(print (send x :factorize 'edges "ABCDEF"))
(print (send x :factorize 'edges "FEDCBA"))
(print (send x :factorize 'interactions))

(print "Generate")

(print (send x :generate-decomposable nil))
(print (send x :generate-graphical nil))

(print "Drop")

(print (send x :drop-factor "A"))
(print (send x :drop-edges "BC"))
(print (send x :add-edges "CF"))
(print (send x :drop-interactions "ACE"))
(print (send x :add-interactions "CF"))
(print (send x :reduce-generator "ACE"))
(print (send x :remove-generator "ACE"))
(print (send x :remove-total-interaction "ACE"))

(print "Meet/Join")

(print (send x :meet-of-models nil))
(print (send x :join-of-models nil))

(print "Generate")

(print (send x :generate-decomposable T))
(print (send x :generate-graphical T))

(print "Drop")

(print (send x :drop-factor "A" T))
(print (send x :drop-edges "BC" T))
(print (send x :add-edges "CF" T))
(print (send x :drop-interactions "ACE" T))
(print (send x :add-interactions "CF" T))
(print (send x :reduce-generator "ACE" T))
(print (send x :remove-generator "ACE" T))
(print (send x :remove-total-interaction "ACE" T))

(print "Generate")

(print (send x :meet-of-models T))
(print (send x :join-of-models T))

(print (send x :collaps-model "ABCD"))

(print "Dual-Normal")

(print (send x :normal-to-dual nil))
(print (send x :dual-to-normal nil))
(print (send x :normal-to-dual T))
(print (send x :dual-to-normal T))

(print "Return")

(print (send x :return-model))
(print (send x :return-model-set))
(print (send x :return-model-set-string))

(print (send x :return-model-number 'current))
(print (send x :return-edge-list-list 'current))
(print (send x :return-edge-list 'current))

(print "Base/Current")

(print (send x :print-model 'all))

(print (send x :base))
(print (send x :print-model 'all))

(print (send x :current))
(print (send x :print-model 'all))

(print (send x :make-base 'base))
(print (send x :print-model 'base))
(print (send x :make-base 'current))
(print (send x :print-model 'base))
(print (send x :make-base 'last))
(print (send x :print-model 'base))
(print (send x :make-base 'previous))
(print (send x :print-model 'base))
(print (send x :make-base 'previous))
(print (send x :print-model 'base))
(print (send x :make-base 'previous))
(print (send x :print-model 'base))
(print (send x :make-base 'next))
(print (send x :print-model 'base))
(print (send x :make-base 'next))
(print (send x :print-model 'base))
(print (send x :make-base 'next))
(print (send x :print-model 'base))
(print (send x :make-base '1))
(print (send x :print-model 'base))

(print (send x :make-current 'base))
(print (send x :print-model 'current))
(print (send x :make-current 'current))
(print (send x :print-model 'current))
(print (send x :make-current 'last))
(print (send x :print-model 'current))
(print (send x :make-current 'previous))
(print (send x :print-model 'current))
(print (send x :make-current 'previous))
(print (send x :print-model 'current))
(print (send x :make-current 'previous))
(print (send x :print-model 'current))
(print (send x :make-current 'next))
(print (send x :print-model 'current))
(print (send x :make-current 'next))
(print (send x :print-model 'current))
(print (send x :make-current 'next))
(print (send x :print-model 'current))
(print (send x :make-current 1))
(print (send x :print-model 'current))

(print "Formula")

(print (send x :read-model "*"))
(print (send x :print-formula))
(print (send x :print-vertex-order))
(print (send x :dispose-of-formula))
(print (send x :read-model "*"))

(print "Is-graphical")

(print (send x :is-graphical ))
(print (send x :is-graphical 'current))
(print (send x :is-graphical 'base))
(print (send x :is-graphical 'last))
(print (send x :is-graphical '1))

(print "Is-decomposable")

(print (send x :is-decomposable ))
(print (send x :is-decomposable 'current))
(print (send x :is-decomposable 'base))
(print (send x :is-decomposable 'last))
(print (send x :is-decomposable '1))


(print "Is-submodel")

(print (send x :is-submodel-of ))
(print (send x :is-submodel-of 'current))
(print (send x :is-submodel-of 'base))
(print (send x :is-submodel-of 'last))
(print (send x :is-submodel-of '1))
(print (send x :is-submodel-of "*"))
(print (send x :is-submodel-of "."))

(print (send x :is-submodel-of 'current 'base))
(print (send x :is-submodel-of 'base 'base))
(print (send x :is-submodel-of 'last 'base))
(print (send x :is-submodel-of '1 'base))
(print (send x :is-submodel-of "*" 'base))
(print (send x :is-submodel-of "." 'base))

(print (send x :is-submodel-of 'current 'current))
(print (send x :is-submodel-of 'base 'current))
(print (send x :is-submodel-of 'last 'current))
(print (send x :is-submodel-of '1 'current))
(print (send x :is-submodel-of "*" 'current))
(print (send x :is-submodel-of "." 'current))

(print (send x :is-submodel-of 'current 'last))
(print (send x :is-submodel-of 'base 'last))
(print (send x :is-submodel-of 'last 'last))
(print (send x :is-submodel-of '1 'last))
(print (send x :is-submodel-of "*" 'last))
(print (send x :is-submodel-of "." 'last))

(print (send x :is-submodel-of 'current 2))
(print (send x :is-submodel-of 'base 2))
(print (send x :is-submodel-of 'last 2))
(print (send x :is-submodel-of '1 2))
(print (send x :is-submodel-of "*" 2))
(print (send x :is-submodel-of "." 2))

(print (send x :is-submodel-of 'current "ABC,DEF"))
(print (send x :is-submodel-of 'base "ABC,DEF"))
(print (send x :is-submodel-of 'last "ABC,DEF"))
(print (send x :is-submodel-of '1 "ABC,DEF"))
(print (send x :is-submodel-of "*" "ABC,DEF"))
(print (send x :is-submodel-of "." "ABC,DEF"))

(print "Is-in-one-clique")

(print (send x :is-in-one-clique "AB" ))
(print (send x :is-in-one-clique "AB" 'current))
(print (send x :is-in-one-clique "AB" 'base))
(print (send x :is-in-one-clique "AB" 'last))
(print (send x :is-in-one-clique "AB" '1))
(print (send x :is-in-one-clique "AB" "*"))
(print (send x :is-in-one-clique "AB" "."))

;(print (send x :make-model 'current "Gryf"))

(print (send x :print-model 'current))
(print (send x :print-model 'base))
(print (send x :print-model 'last))
(print (send x :print-model 'all))
(print (send x :print-model '1))
(print (send x :print-model 'number 1))
(print (send x :print-model 'interval 1 3))
(print (send x :print-model 'list '(1 3 5)))

(print (send x :describe-model 'current))
(print (send x :describe-model 'base))
(print (send x :describe-model 'last))
(print (send x :describe-model 'number 1))
(print (send x :describe-model 'interval 1 3))
(print (send x :describe-model 'list '(1 3 5)))

(print (send x :dispose-of-model 'current))
(print (send x :dispose-of-model 'base))
(print (send x :dispose-of-model 'last))
(print (send x :dispose-of-model 'number 1))
(print (send x :print-model 'all))
(print (send x :dispose-of-model 'interval 1 3))
(print (send x :dispose-of-model 'list '(1 3 5)))
(print (send x :dispose-of-model 'all))
(print (send x :print-model 'all))

(print (send x :show-tests ))

(print (send x :dispose-of-tests ))

(print (send x :show-tests ))

(print (send x :dispose-of-tables ))
(print (send x :dispose-of-probabilities ))
