(require "Testgraph")

; Create a "Dynamic-spin-plot" for `graph-1':
; The values in the plot is reevaluated when the graph for the
; spin-plot receives the message :make-graph-current-model or 
; :make-graph-current-model

(let ((a '(send *this-graph* :return-vector 'adjusted "*" :model 'current))
      (b '(send *this-graph* :return-vector 'adjusted "*" :model 'base))
      (c '(send *this-graph* :return-vector 'observed "*" :model nil)))
  (def graph-1-spin (send graph-1 :return-dynamic-coco-spin-plot (list a b c)))
  (send graph-1-spin :location 700 500)
  )

(let ((c '(send *this-graph* :return-vector 'adjusted "*" :model 'current))
      (b '(send *this-graph* :return-vector 'adjusted "*" :model 'base))
      (a '(send *this-graph* :return-vector 'unadjusted "*" :model nil))
      (d '(send *this-graph* :return-vector 'observed "*" :model nil)))
  (def graph-2-spin (send graph-1 :return-dynamic-coco-spin-plot
			  (list a b c d)))
  (send graph-2-spin :location 500 500)
  )


(provide "TestDynamic")
