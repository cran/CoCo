
;;; Tutorial (test) example on using CoCo-graphs
;;; ============================================

;;; Source: Morrison (1976). Multivariate Statistical Methods, McGraw-Hill.
;;;     from Edwards (1995). Introduction to Graphical Modelling.


;;; Create a CoCo-object:

;;; (def Rats (make-coco :n 65536 :p 65536 :q 65535
;;;           :r 65534 :s 65533 :ss 65532 :t 65531))

(def Rats (make-coco :title "Rats"))

;;; (load "fix")

; Send the specification of the table to the CoCo-object:

;;; (send Rats :enter-names "abxy" '(2 3 0 0) '(0 0 0 0))

(send Rats :enter-names "abxy" (list 2 3 'continuous 'continuous) '(0 0 0 0))

(send Rats :set-switch 100 'off)

;;; (send Rats :set-switch 227 'on)
;;; (send Rats :set-switch 127 'on)
;;; (send Rats :set-switch 106 'on)

;;; Define `n' to hold the observations:

(def abxy '(1 1  5  6   1 1  5  4   1 1  9  9   1 1  7  6   
            1 2  7  6   1 2  7  7   1 2  9 12   1 2  6  8   
            1 3 21 15   1 3 14 11   1 3 17 12   1 3 12 10   
            2 1  7 10   2 1  6  6   2 1  9  7   2 1  8 10   
            2 2 10 13   2 2  8  7   2 2  7  6   2 2  6  9   
            2 3 16 12   2 3 14  9   2 3 14  8   2 3 10  5   ))

;;; Send the table `n' to the CoCo-object:

;;; (send Rats :enter-list abxy)

(def ab '(1 1 1 1 1 1 1 1  1 2 1 2 1 2 1 2 
          1 3 1 3 1 3 1 3  2 1 2 1 2 1 2 1 
          2 2 2 2 2 2 2 2  2 3 2 3 2 3 2 3))

(def xy '( 5  6     5  4     9  9     7  6     7  6     7  7     9 12     6  8   
          21 15    14 11    17 12    12 10     7 10     6  6     9  7     8 10   
          10 13     8  7     7  6     6  9    16 12    14  9    14  8    10  5   ))

;;; Send the table `ab' and `xy` to the CoCo-object:

(send Rats :enter-list (list ab xy))

;;; Just to see all tree types of vertices:

(send Rats :set-ordinal "a")

;;; Create some model-objects, including causal models:

(def model-1 (send Rats :make-model "*;"))
(def model-2 (send Rats :make-model "ab,by,yx,xa;"))
(def model-3 (send Rats :make-model "ab / ax, by / x,y / ; "))
;; (def model-4 (send Rats :make-model "ab / ax, by / x,y | ab < y < x ;"))
;; (def model-5 (send Rats :make-model "*|xy|a|b;"))

;;; Create graphs for some of the models:

(def graph-1 (send model-1 :make-graph :title "Rats CoCo Graph"))
(def graph-3 (send Model-3 :make-graph :location (list 700  50) :title "Rats"))
;;(def graph-5 (send Model-5 :make-graph :location (list 700 150)
;;                   :title "Rats: Causal"))

;; (def model-1-causal (send graph-1 :define-blocks "ab < xy"))

;;; You can now move vertices, drop and add edges etc. with the mouse
;;; and perform, e.g., a model search by selecting from the menu.

;;; Set label on the vertices in the graph:

(send graph-1 :vertex-label #\a "a: Sex"       :redraw nil)
(send graph-1 :vertex-label #\b "b: Treatment" :redraw nil)
(send graph-1 :vertex-label #\x "x: Wt loss 1" :redraw nil)
(send graph-1 :vertex-label #\y "y: Wt loss 2" :redraw nil)

;;; Change the color on the vertex-labels for all graphs:

(send graph-1 :item-color 'vertex-label 'red)

;;; Move the vertices in the graph:

(send graph-1 :vertex-position #\a (- (list 25 25) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\b (- (list 25 75) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\x (- (list 75 25) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\y (- (list 75 75) (list 50 50)) :redraw T)

;;; Move point-labels for vertex `a' and `x':

;;; (send graph-1 :vertex-label-position #\a (list -3 -4)) 
;;; (send graph-1 :vertex-label-position #\x (list -11 -2 0) :redraw T)


;;; Add controll buttuns to graph-1:

(send graph-1 :add-controls)


;;; Let `graph-3' have same vertex-positions as `graph-1':

;;; (send graph-3 :slot-value 'vertices (send graph-1 :slot-value 'vertices))


;;; Return some vectors:

(def canonical (send rats :return-continuous 'canonical "abxy;"))
(def moments (send rats :return-continuous 'moment "abxy;"))

(def observed (send rats :return-vector 'observed "ab;"))
(def expected (send rats :return-vector 'expected "ab;"))

(provide "Testgraph")
