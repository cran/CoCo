(def svin (make-coco))

(send svin :enter-names-and-list
      '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      '(":race" ":sex" ":rdn1" ":rdn2a" ":meana1" ":meana2a"
        ":meana2b" ":capdens" ":capfib" ":cs" ":had" ":ldh"
        ":glykogen" ":kodpct" ":fe" ":muscg" ":fatimf"
        ":pigment" ":biteforce" ":sarc" ":phu" ":whc")
                      '(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
      :missing-levels '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
       :columns '(  0 1 2 3 )  
      )

(def m (send svin :make-model "*"))
(def g (send m :make-graph :title "PIGS in space"))

(trace :return-history-and-future)
(trace :return-history-model-nr)
;(trace :names)
;(trace :positions)
(trace split-string)
(trace split-block-string)
(trace string-to-block-list)

(send g :define-blocks  ":race:sex < :rdn1 :rdn2a")

(send g :define-blocks  ":race:sex<:rdn1:rdn2a")
