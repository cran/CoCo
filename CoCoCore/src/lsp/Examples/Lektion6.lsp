(def model-4 (send reinis-coco-object :make-model "*"))
(def graph-4a (send model-4 :make-graph :location (list 100 50)
		    :title "Reinis: "))

; Set label on the vertices in the graph:

(send graph-4a :vertex-label #\A "A: Smoking"               :redraw nil)
(send graph-4a :vertex-label #\B "B: Mental"                :redraw nil)
(send graph-4a :vertex-label #\C "C: Physical"              :redraw nil)
(send graph-4a :vertex-label #\D "D: Blood pressure"        :redraw nil)
(send graph-4a :vertex-label #\E "E: Ratio of lipoproteins" :redraw nil)
(send graph-4a :vertex-label #\F "F: Family anamnesi")

; Move the vertices in the graph:

(send graph-4a :vertex-position #\A (list   0 -20) :redraw nil)
(send graph-4a :vertex-position #\B (list  40  35) :redraw nil)
(send graph-4a :vertex-position #\C (list   0  20) :redraw nil)
(send graph-4a :vertex-position #\D (list -40 -20) :redraw nil)
(send graph-4a :vertex-position #\E (list -40  20) :redraw nil)
(send graph-4a :vertex-position #\F (list  40 -20) :redraw T)

(def graph-4b (send graph-4a :return-child-coco-graph-window
		   :model "[[ABC][ACE][ADE][EF]];" :copy-vertices T
		   :title "[[ABC][ACE][ADE][EF]];"
		   :offset (list 10 0) :parant T))

(def graph-5a (send graph-4a :return-child-coco-graph-window
		   :model "[[AF][BC][BF][ACDE]];" :copy-vertices T
		   :title "[[AF][BC][BF][ACDE]];"
		   :offset (list 300 0) :parant T))

(def graph-5b (send graph-5a :return-child-coco-graph-window
		   :model "[[ACE][ADE][BC][F]];" :copy-vertices T
		   :title "[[ACE][ADE][BC][F]];"
		   :offset (list 10 10) :parant T))

(def graph-6a (send graph-4a :return-child-coco-graph-window
		   :model "[[ACE][BCE][ADE][BF]];" :copy-vertices T
		   :title "[[ACE][BCE][ADE][BF]];"
		   :offset (list 600 0) :parant T))

(def graph-6b (send graph-6a :return-child-coco-graph-window
		   :model "[[AC][BC][BE][ADE][BF]];" :copy-vertices T
		   :title "[[AC][BC][BE][ADE][BF]];"
		   :offset (list 10 10) :parant T))
