(require "Testgraph")

(def graph-1-block (send graph-1 :define-blocks "F,BC,AE,D"))

; Do a `Headlong Backward' at `graph-1': 

(send graph-1-block
      :block-backward :block nil :p-accepted 0.10 :p-rejected 0.05
      :random-order T :coherent T :headlong T :decomposable-mode T
      :headlong T :recursive T :x-move 0)

#|

The methods graph-... does not exist any longer:

(send graph-1 :block-backward
      :block nil
      :p-accepted (send graph-1 :graph-p-accepted)
      :p-rejected (send graph-1 :graph-p-rejected)
      :random-order (send graph-1 :graph-random-order)
      :coherent (send graph-1 :graph-coherent)
      :follow (send graph-1 :graph-follow)
      :decomposable-mode (send graph-1 :graph-decomposable-mode)
      :headlong T :recursive T :x-move 0)

|#