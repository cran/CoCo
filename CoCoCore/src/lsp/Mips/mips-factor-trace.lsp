
(trace make-coco-model-graph-window)
(trace :make-graph)
(trace :make-factor-graph)
(trace :return-child-coco-graph-window)
(trace :return-child-coco-factor-graph-window)

;; (trace split-name-string)
;; (trace split-string)
;; (trace split-string-chr)

;; (trace :string-to-vertices)

(trace inner)
(trace :return-model)
(trace :make-graph-current-model)
(trace :return-generators)
(trace :return-gcs)
(trace return-default-vertices)
(trace split-block-string)
(trace string-to-block-list)
(trace :set-blocks)

;; (trace :return-names)
;; (trace :return-name-list)
;; (trace :return-name-list-string)

;; (send graph-3 :return-names)

;; (def string (send graph-3 :return-model))
;; (def x (split-string-chr string #\[ #\] nil))

;;(map-elements #'length x (iseq (length x)))

;; (mapcar #'(lambda (i) (length i)) x)

;; (nth (car (which (/= 0 (mapcar #'(lambda (i) (length i)) x)))) x)
;; (nth (car (which (< 1 (mapcar #'(lambda (i) (length i)) x)))) x)
