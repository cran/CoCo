
;;; Tutorial example on using CoCo-graphs
;;; =====================================

; Create a CoCo-object:

(def byssinosis-coco-object (make-coco))


; Send the specification of the table to the CoCo-object:

(send byssinosis-coco-object :enter-names
      '(":byssinosis" ":dust" ":race" ":sex" ":smoking" ":employment")
      '(2 3 2 2 2 3) '(0 0 0 0 0 0))

; Declare ``Dust'' and ``Employment'' to ordinal:

(send byssinosis-coco-object :set-ordinal ":dust:employment")
(send byssinosis-coco-object :set-ordinal "*")


; Set adjuste Degrees of fredom on:

(send byssinosis-coco-object :set-switch 'adjusted-df 'on)


; Define `byssinosis-data' to hold the table of observations:

(def byssinosis-data '(
  3    37    0    74    2   258     25   139    0    88    3   242  
  0     5    1    93    3   180      2    22    2   145    3   260  
  0    16    0    35    0   134      6    75    1    47    1   122  
  0     4    1    54    2   169      1    24    3   142    4   301  
  8    21    1    50    1   187      8    30    0     5    0    33  
  0     0    1    33    2    94      0     0    0     4    0     3  
  2     8    1    16    0    58      1     9    0     0    0     7  
  0     0    0    30    1    90      0     0    0     4    0     4  
 31    77    1   141   12   495     10    31    0     1    0    45  
  0     1    3    91    3   176      0     1    0     0    0     2  
  5    47    0    39    3   182      3    15    0     1    0    23  
  0     2    3   187    2   340      0     0    0     2    0     3))


; Send the table `n' to the CoCo-object:

(send byssinosis-coco-object :enter-table byssinosis-data)

; Create a saturated model-object:

(def model-1 (send byssinosis-coco-object :make-model "*"))


; Create graphs for the 1th model:

(def graph-1 (send model-1 :make-graph :title "Byssinosis CoCo Graph"))


; You can now move vertices, drop and add edges etc. with the mouse
; and perform, e.g., a model search by selecting from the menu.

; Set label on the vertices in the graph:

; Change the color on the vertex-labels for all graphs:

(send graph-1 :item-color 'vertex-label 'red)

; Change the color on dragged edges in all graphs:

(send graph-1 :item-color 'new-edge 'black)


; Move the vertices in the graph (flip two vertices):

 (def a (send graph-1 :vertex-position ":byssinosis"))

 (send graph-1 :vertex-position ":byssinosis"
       (send graph-1 :vertex-position ":race") :redraw nil)

 (send graph-1 :vertex-position ":race" a :redraw nil)

(provide "Lung")
