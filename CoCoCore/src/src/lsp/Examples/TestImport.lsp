
;;; Tutorial example on using CoCo-graphs
;;; =====================================

; Create a CoCo-object:

(def coco-cg-object (make-coco :title "Mips"))

; Import:

(send coco-cg-object :import
"/home/jhb/CoCo/NoName/Examples/OzDASL/CloudSeeding/cloudtas.xpt" )

; Create 2 model-objects:

(def model-1 (send coco-cg-object :make-model "*"))
(def model-3 (send coco-cg-object :make-model "."))


; Create graphs for the 1th and 3th model:

(def graph-1 (send model-1 :make-graph :title "Full"))
(def graph-3 (send Model-3 :make-graph :location (list 700 50)
		   :title "Maineffects"))

; Change the color on the vertex-labels for all graphs:

(send graph-1 :item-color 'vertex-label 'red)

; Add controll buttuns to graph-1:

(send graph-1 :add-controls)


; Let `graph-3' have same vertex-positions as `graph-1':

;; (send graph-3 :slot-value 'vertices (send graph-1 :slot-value 'vertices))


(provide "TestMips")
