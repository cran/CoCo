(setf a (make-coco :title "Without structural zero"))
(send a :enter-names "smv" '(2 2 2))
(send a :enter-table '(11 2209 0 111 48 239 72 2074))
(setf Model-a-1 (send a :make-model "*" :title "Saturated model"))
(setf Model-a-2 (send a :make-model "mv,vs"
                      :title "Sentence cond. indp. of Murderer"))
(print-test (send Model-a-2 :compute-test-against-model-object Model-a-1))
(send a :return-vector 'observed "*")

(setf b (make-coco :title "With structural zero"))
(send b :enter-names "smv" '(2 2 2))
(send b :enter-table '(11 2209 -1 111 48 239 72 2074))
(setf Model-b-1 (send b :make-model "*"
                      :title "Saturated model with struct. zero"))
(setf Model-b-2 (send b :make-model "mv,vs"
                      :title "Sentence cond. quasi-indp. of Murderer"))
(print-test (send Model-b-2 :compute-test-against-model-object Model-b-1))

(- (send Model-a-2 :return-vector 'expected "*")
   (send Model-b-2 :return-vector 'expected "*"))

(setf b-graph (send Model-b-1 :make-graph))
