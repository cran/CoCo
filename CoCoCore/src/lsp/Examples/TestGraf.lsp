
; Lav et coco-objekt:

(setf reinis-coco-object (make-coco))


; Indlaes specifikationen af tabellen i objektet:

(send reinis-coco-object :enter-names "ABCDEF" '(2 2 2 2 2 2) '(0 0 0 0 0 0))


; Indlaes tabellen i objektet:

(def n (list
 44  40  112  67  129 145   12  23     35  12   80  33  109  67    7   9
 23  32   70  66   50  80    7  13     24  25   73  57   51  63    7  16
  5   7   21   9    9  17    1   4      4   3   11   8   14  17    5   2
  7   3   14  14    9  16    2   3      4   0   13  11    5  14    4   4))
(send reinis-coco-object :enter-table n)

(set-ordinal "AB.")

; Lav nogle model-objekter under coco-objektet:

(setf model-1 (make-model "*" reinis-coco-object))
(setf model-2 (make-model "ABCF,ACDE" reinis-coco-object))
(setf model-3 (make-model "ACE,ADE,BC,F" reinis-coco-object))
(setf model-4 (make-model "[[ABC][ABE][ADE][DF]]" reinis-coco-object))

 

; Plot to af model-objekterne:

(setf graph-1 (send model-1 :make-graph :title "Reinis CoCo Graph"))

(setf graph-3 (send Model-3 :make-graph :location (list 700 50)
		    :title "Reinis: ACE,ADE,BC,F"))

(setf graph-4 (send Model-4 :make-graph :location (list 700 450)
		    :title "Reinis: [[ABC][ABE][ADE][DF]]"))

; Saet variabel-labels:

(send graph-1 :vertex-label #\A "A: Smoking"               :redraw nil)
(send graph-1 :vertex-label #\B "B: Mental"                :redraw nil)
(send graph-1 :vertex-label #\C "C: Physical"              :redraw nil)
(send graph-1 :vertex-label #\D "D: Blood pressure"        :redraw nil)
(send graph-1 :vertex-label #\E "E: Ratio of lipoproteins" :redraw nil)
(send graph-1 :vertex-label #\F "F: Family anamnesi")


; Saet positioner for punkter:

(send graph-1 :vertex-position #\A (- (list 50 30) 50) :redraw nil)
(send graph-1 :vertex-position #\B (- (list 50 70) 50) :redraw nil)
(send graph-1 :vertex-position #\C (- (list 85 50) 50) :redraw nil)
(send graph-1 :vertex-position #\D (- (list 20 10) 50) :redraw nil)
(send graph-1 :vertex-position #\E (- (list 10 50) 50) :redraw nil)
(send graph-1 :vertex-position #\F (- (list 20 90) 50) :redraw T)

;(send graph-3 :vertex-label #\A "smoking")
;(send graph-3 :vertex-label #\B "strenuous mental work")
;(send graph-3 :vertex-label #\C "strenuous physical work")
;(send graph-3 :vertex-label #\D "systolic blood pressure")
;(send graph-3 :vertex-label #\E "ratio of `alpha' to `beta' lipoproteins")
;(send graph-3 :vertex-label #\F "family anamnesis of coronary heart disease")
