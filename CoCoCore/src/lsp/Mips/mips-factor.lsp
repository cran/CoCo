
;; Update of "Mips" to "Factor-graph":
;; Could be put in, e.g., "mipsblocks.lsp", but not "mips.lsp".

(defmeth coco-proto :return-gcs (model &key (full nil) (simple nil)) ;; !!!!!
  (mapcar #'(lambda (i)
	      (list
	       ;; Name:
	       (car i)
	       ;; Variable-label:
	       nil
	       ;; (concatenate 'string "Variable " (list (int-char i)))
	       ;; Variable-type (discrete, continuous, ordinal, etc.):
	       (case (cadr i)
		 ('discrete  'discrete-generator)
		 ('linear    'linear-generator)
		 ('quadratic 'quadratic-generator)) ;; 'generator
	       ;; Stratum:
	       0))
	  (send self :return-generators
		model :full full :simple simple :noted T))
  )

(defmeth coco-proto :return-model-vertices 
  (&optional (vertices nil) (model 'current modelset) number &key (simple nil)) ;; !!!!!
  (let ((vertices (if vertices vertices (send self :vertices)))
	(generators (send self :return-generators model :simple simple)))
    (mapcar #'(lambda (char) (send self :string-to-vertices char vertices))
	    generators))
  )
