
;;; Tutorial example on using CoCo-graphs
;;; =====================================

(load "Windows-load-mips")
(load "replace")

; Create a CoCo-object:

(def coco-cg-object (make-coco :title "CoCoCg for Kamma"))

; Import:

(send coco-cg-object :import "kammaX.xpt" )

; Create 2 model-objects:

(def model-1 (send coco-cg-object :make-model "*"))
(def model-2
     (send coco-cg-object :make-model
	   "[[:SEX:PH:ORG2:AGE2:INDFLYD:MIDLER]]"))
(def model-3x (send coco-cg-object :make-model "."))
(def model-3
     (send coco-cg-object :make-model
	   "[[:SEX][:PH][:ORG2][:AGE2][:INDFLYD][:MIDLER]]"))


; Create graphs for the 1th and 3th model:

(def graph-1 (send model-1 :make-graph :title "Full"))
(def graph-2 (send Model-2 :make-graph :location (list 300 150)
		   :title "Forward"))
(def graph-3 (send Model-3 :make-graph :location (list 700 50)
		   :title "Maineffects"))

; Change the color on the vertex-labels for all graphs:

(send graph-1 :item-color 'vertex-label 'red)

; Add controll buttuns to graph-1:

(send graph-1 :add-controls)

(send graph-2 :positions '((40 0 0) (-5 -45 0) (5 -25 0)
			   (-15 -25 0) (-45 -25 0) (-35 -15 0)))

; Let `graph-1' have same vertex-positions as `graph-2':

(send graph-1  :vertices (send graph-2 :vertices))
(send graph-3  :vertices (send graph-2 :vertices))

(provide "Kamma")
