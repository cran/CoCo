
;;; Tutorial example on using CoCo-graphs
;;; =====================================

; Create a CoCo-object:

;(def reinis-coco-object (make-coco :n 65536 :p 65536))
(def reinis-coco-object (make-coco :title "Reinis"))


; Send the specification of the table to the CoCo-object:

(send reinis-coco-object :enter-names "ABCDEF" '(2 2 2 2 2 2) '(0 0 0 0 0 0))


; Define `n' to hold the table of observations:

(def n '(
 44  40  112  67  129 145   12  23     35  12   80  33  109  67    7   9
 23  32   70  66   50  80    7  13     24  25   73  57   51  63    7  16
  5   7   21   9    9  17    1   4      4   3   11   8   14  17    5   2
  7   3   14  14    9  16    2   3      4   0   13  11    5  14    4   4))


; Send the table `n' to the CoCo-object:

(send reinis-coco-object :enter-table n)

; Create 3 model-objects:


(def model-1 (send reinis-coco-object :make-model "*"))
(def model-2 (send reinis-coco-object :make-model "ABCF,ACDE"))
(def model-3 (send reinis-coco-object :make-model "ACE,ADE,BC,F"))


; Create graphs for the 1th and 3th model:

(def graph-1 (send model-1 :make-graph :title "Reinis CoCo Graph"))

(def graph-3 (send graph-1 :return-child-coco-graph-window
                   :model "ACE,ADE,BC,F"
                   :location (list 700 50)
		   :title "Reinis: ACE,ADE,BC,F"))

; You can now move vertices, drop and add edges etc. with the mouse
; and perform, e.g., a model search by selecting from the menu.

; Set label on the vertices in the graph:

(send graph-1 :vertex-label #\A "A: Smoking"               :redraw nil)
(send graph-1 :vertex-label #\B "B: Mental"                :redraw nil)
(send graph-1 :vertex-label #\C "C: Physical"              :redraw nil)
(send graph-1 :vertex-label #\D "D: Blood pressure"        :redraw nil)
(send graph-1 :vertex-label #\E "E: Ratio of lipoproteins" :redraw nil)
(send graph-1 :vertex-label #\F "F: Family anamnesi")


; Change the color on the vertex-labels for all graphs:

(send graph-1 :item-color 'not-fitted 'red)
(send graph-1 :item-color 'vertex-label 'red)
(send graph-1 :item-color 'controls 'black)


; Move the vertices in the graph:

(send graph-1 :vertex-position #\A (- (list 50 30) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\B (- (list 50 70) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\C (- (list 85 50) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\D (- (list 20 10) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\E (- (list 10 50) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\F (- (list 20 90) (list 50 50)) :redraw T)


; Move point-labels for vertex `C' and `E':

(send graph-1 :vertex-label-position #\E (list -3 -4)) 
(send graph-1 :vertex-label-position #\C (list -11 -2 0) :redraw T)


; Add controll buttuns to graph-1:

(send graph-1 :add-controls)



(provide "Testgraph")
