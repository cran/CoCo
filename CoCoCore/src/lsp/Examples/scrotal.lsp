
;;; Tutorial example on using CoCo-graphs
;;; =====================================

; Create a CoCo-object:

(def scrotal-coco-object (make-coco :title "Scrotal Swelling Data"))


; Send the specification of the table to the CoCo-object:

(send scrotal-coco-object :enter-names
      "HABCDEFG" '(2 2 2 2 2 2 2 2) '(0 0 0 0 0 0 0 0))


; Define `n' to hold the table of observations:

(def scrotal-data '(
 1 1 1 1 1 1 2 2 1
16 1 2 1 1 1 1 1 1
 3 1 2 1 1 1 1 2 1
51 1 2 1 1 1 2 2 1
17 1 2 1 1 1 2 2 2
30 1 2 1 2 1 1 1 1
 1 1 2 1 2 1 1 1 2
 3 1 2 1 2 1 1 2 1
 1 1 2 1 2 1 2 1 1
20 1 2 1 2 1 2 2 1
 4 1 2 1 2 1 2 2 2
36 1 2 1 2 2 1 1 1
 3 1 2 1 2 2 1 2 1
38 1 2 2 1 1 1 1 1
 1 1 2 2 1 1 1 1 2
 3 1 2 2 1 1 1 2 1
 3 1 2 2 1 1 2 2 1
21 1 2 2 2 1 1 1 1
 2 1 2 2 2 1 2 2 1
39 2 1 1 2 2 1 1 1
 5 2 1 1 2 2 1 2 1
 1 2 2 1 2 2 1 1 1))


; Send the table `n' to the CoCo-object:

(send scrotal-coco-object :enter-list scrotal-data :accumulated T)

; Create a saturated model-object:

(def model-1 (send scrotal-coco-object :make-model "*"))


; Create graphs for the 1th model:

(def graph-1 (send model-1 :make-graph :title "Scrotal CoCo Graph"))


; You can now move vertices, drop and add edges etc. with the mouse
; and perform, e.g., a model search by selecting from the menu.

; Set label on the vertices in the graph:

(send graph-1 :vertex-label #\H "H: Hernia" :redraw nil)
(send graph-1 :vertex-label #\A "A: Possible to get above the swelling"
      :redraw nil)
(send graph-1 :vertex-label #\A "A: Get above" :redraw nil)
(send graph-1 :vertex-label #\B "B: Swelling transilluminates" :redraw nil)
(send graph-1 :vertex-label #\B "B: Transilluminates" :redraw nil)
(send graph-1 :vertex-label #\C "C: Swelling separated from testes"
      :redraw nil)
(send graph-1 :vertex-label #\C "C: Separated from testes" :redraw nil)
(send graph-1 :vertex-label #\D "D: Positive Valsalva" :redraw nil)
(send graph-1 :vertex-label #\D "D: Valsalva" :redraw nil)
(send graph-1 :vertex-label #\E "E: Tenderness" :redraw nil)
(send graph-1 :vertex-label #\F "F: Pain" :redraw nil)
(send graph-1 :vertex-label #\G "G: Urinal tract infection")


; Change the color on the vertex-labels for all graphs:

(send graph-1 :item-color 'vertex-label 'red)


; Move the vertices in the graph:

(send graph-1 :vertex-position #\H (- (list 20 20) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\A (- (list 20 80) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\B (- (list 40 20) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\C (- (list 60 80) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\D (- (list 40 80) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\E (- (list 60 20) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\F (- (list 80 20) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\G (- (list 80 80) (list 50 50)) :redraw nil)

(send graph-1 :vertex-position #\H (- (list 20 20) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\A (- (list 80 20) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\B (- (list 10 40) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\C (- (list 90 60) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\D (- (list 90 40) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\E (- (list 10 60) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\F (- (list 20 80) (list 50 50)) :redraw nil)
(send graph-1 :vertex-position #\G (- (list 80 80) (list 50 50)) :redraw nil)

