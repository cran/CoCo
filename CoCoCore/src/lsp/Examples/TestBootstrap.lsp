
; Load first ``Testgraph'' then `TestBootstrap''
; or the plots will not be updated (redrawn)
; between steps in the resampling.

(require "Testgraph")
(require "resampling")

(def case-list (send reinis-coco-object :return-case-list))

; Set some options

(send reinis-coco-object :set-switch 'decomposable-mode 'on)
(send reinis-coco-object :set-rejection 0.001)
(send reinis-coco-object :set-acceptance 0.01)


; Do 100 ``Headlong backward eliminations'' on 50 percent of the cases:

(def graph-1b (send model-1 :make-graph :location (list 400 350)
		    :title "Reinis CoCo Graph"))

;(send graph-1b :slot-value 'positions (send graph-1 :slot-value 'positions))
(send graph-1b :positions (send graph-1 :positions))

(setf edges
      (send graph-1b :resampling-backward case-list 0.50 100
	    :replacement nil
            :only nil :reversed nil :sorted nil :short T
	    :headlong T :recursive T :coherent T :follow T
	    :least-significant T :separators nil :edges T))


;(16 73 100 22 0 1 56 29 34 36 0 2 1 0 2)
;(12 86 100 21 1 1 59 19 41 37 1 5 2 0 1)

;(0 12 0 86 100 0 21 1 1 0 59 19 41 37 0 1 5 2 0 1 0)
