
(load "tstinit.lsp")

(print (send x :print-table 'observed))
(print (send x :print-table 'observed "ABCDEF"))
(print (send x :print-table 'observed "*"
	     :model 'current :random nil :log-transformed nil
	     :complete nil :permuted T))

(print (send x :describe-table 'observed))
(print (send x :describe-table 'observed "ABCDEF"))
(print (send x :describe-table 'observed "*"
	     :model 'current :random nil :log-transformed nil
	     :probit nil :rankit nil :uniform nil
	     :complete nil :permuted T))

(print (send x :return-vector 'observed))
(print (send x :return-vector 'observed "ABCDEF"))
(print (send x :return-vector 'observed "*"
	     :model 'current :random nil :log-transformed nil
	     :complete nil :permuted T :dump nil))

(print (send x :return-matrix (list 'observed)))
(print (send x :return-matrix (list 'observed) "ABCDEF"))
(print (send x :return-matrix (list 'observed) "*"
	     :model-list (list 'current)
	     :random-list (list nil)
	     :log-transformed-list (list nil)
	     :complete-list (list nil)
	     :permuted T))

(print (send x :print-sparse-table))
(print (send x :print-sparse-table "ABCDEF"))

(print (send x :plot 'observed 'expected))
(print (send x :plot 'observed 'expected "ABCDEF"))
(print (send x :plot 'observed 'expected "*"
	     :X-model 'current
	     :X-random nil
	     :X-log-transformed nil
	     :Y-model 'current
	     :Y-random nil
	     :Y-log-transformed nil
	     :complete nil))

(print (send x :list-values))
(print (send x :list-values "ABCDEF"))

(print (send x :case-list))
(print (send x :case-list "ABCDEF"))

(print (send x :slice "A" "B"))
(print (send x :slice "A,B"))
(print (send x :slice "A" "B" "C"))
(print (send x :slice "A,B/C"))

