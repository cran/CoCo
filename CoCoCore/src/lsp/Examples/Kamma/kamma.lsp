
;;; Tutorial example on using CoCo-graphs
;;; =====================================

; Create a CoCo-object:

(load "Windows-load-mips")
(load "replace")

(def coco-cg-object (make-coco :title "CoCoCg for Kamma"))

; Import:

(send coco-cg-object :import "kamma.xpt" )

; Create 2 model-objects:

(def model-1 (send coco-cg-object :make-model "*"))

(def model-2
     (send coco-cg-object :make-model
	   "[[:AGE2:INDFLYD:MIDLER][:AGE2:INDFLYD:SEX][:ORG2:AGE2:MIDLER][:PH:ORG2:AGE2][:PH:AGE2:SEX]]"))

(def model-4
     (send coco-cg-object :make-model
	   "[[:ORG2:MIDLER][:AGE2:INDFLYD:MIDLER][:PH:ORG2:AGE2:SEX][:AGE2:INDFLYD:SEX]]"))

(def model-3-a (send coco-cg-object :make-model "."))

(def model-3-b (send coco-cg-object :make-model ".;"))

(def model-3
     (send coco-cg-object :make-model
	   "[[:SEX][:PH][:ORG2][:AGE2][:INDFLYD][:MIDLER]]"))

; Create graphs for the 1th and 3th model:

(def graph-1 (send model-1 :make-graph :title "Full"))
(def graph-2 (send Model-2 :make-graph :location (list 300 150)
		   :title "Backward"))
(def graph-3 (send Model-3 :make-graph :location (list 700 50)
		   :title "Maineffects"))

; Change the color on the vertex-labels for all graphs:

(send graph-1 :item-color 'vertex-label 'red)

; Add controll buttuns to graph-1:

(send graph-1 :add-controls)

(send graph-2 :positions '((25 25 0) (20 -35 0) (5 -5 0) (-25 5 0)
			   (-35 -35 0) (-35 35 0)))

; Let `graph-1' have same vertex-positions as `graph-2':

(send graph-1  :vertices (send graph-2 :vertices))
(send graph-3  :vertices (send graph-2 :vertices))

(def a (send model-1 :return-vector 'observed "."))

(provide "Kamma")
