(require "Testgraph")

(require "TestDynamic")

; Do a `Headlong Backward' at `graph-1':


(send graph-1 :drop-least-significant-edge :p-accepted 0.10 :p-rejected 0.05
      :random-order T :coherent T :headlong T :recursive T :x-move 0)

#|

The methods graph-... does not exist any longer:

(send graph-1 :drop-least-significant-edge
      :p-accepted (send graph-1 :graph-p-accepted)
      :p-rejected (send graph-1 :graph-p-rejected)
      :random-order (send graph-1 :graph-random-order)
      :coherent (send graph-1 :graph-coherent)
      :headlong T :recursive T :x-move 0)

|#
