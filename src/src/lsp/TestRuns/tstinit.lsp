
;;; Tutorial example on using CoCo-graphs
;;; =====================================

; Create a CoCo-object:

;(def reinis-coco-object (make-coco :n 65536 :p 65536))
(def reinis-coco-object (make-coco))
(def x reinis-coco-object)


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

; Create 1 model-objects:

(def model-1 (send reinis-coco-object :make-model "*"))

