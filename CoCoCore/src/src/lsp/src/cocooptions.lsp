
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for formats in CoCo-object.

(defun option-formats-dialog (x)

  (let* ((dismiss-option-formats
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-formats-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, formats options ")
		:action #'(lambda () (send x :status 'formats))))

	 (page-formats-item
	  (send modal-button-proto :new
		(modal-text "Page-formats  ...")
		:action #'(lambda ()
			    (send x :set-page-formats
				  (car (get-value-dialog
					"Page-formats:"
					:initial
					`',(send x :set-page-formats 'what)
					))))))

	 (table-formats-item
	  (send modal-button-proto :new
		(modal-text "Table-formats  ...")
		:action #'(lambda ()
			    (send x :set-table-formats
				  (car (get-value-dialog
					"Table-formats:"
					:initial
					`',(send x :set-table-formats 'what)
					))))))

	 (test-formats-item
	  (send modal-button-proto :new
		(modal-text "Test-formats  ...")
		:action #'(lambda ()
			    (send x :set-test-formats
				  (car (get-value-dialog
					"Test-formats:"
					:initial
					`',(send x :set-test-formats 'what)
					))))))

	 (print-formats-item
	  (send modal-button-proto :new
		(modal-text "Print-formats  ...")
		:action #'(lambda ()
			    (send x :set-print-formats
				  (car (get-value-dialog
					"Print-formats:"
					:initial
					`',(send x :set-print-formats 'what)
					))))))

	 (paging-length-item
	  (send modal-button-proto :new
		(modal-text "paging-length  ...")
		:action #'(lambda ()
			    (send x :set-paging-length
				  (car (get-value-dialog
					"Paging-length:"
					:initial
					`',(send x :set-paging-length 'what)
					))))))

	 (short-test-output-item
	  (get-linked-toggle-item x 'short-test-output   ))

	 (pausing-of-output-item
	  (get-linked-toggle-item x 'pausing-of-output   ))

	 (option-formats-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-formats)
		      status-item
		      page-formats-item
		      table-formats-item
		      test-formats-item
		      print-formats-item
		      short-test-output-item
		      pausing-of-output-item
		      paging-length-item
		      )))))
  )


;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selection test options
;;; in the CoCo-object.

(defun option-tests-dialog (x)

  (let* ((dismiss-option-tests
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-tests-dialog-window :remove))))

	 (partitioning-item
	  (get-linked-toggle-item x 'partitioning        ))

	 (adjusted-df-item
	  (get-linked-toggle-item x 'adjusted-df         ))

	 (power-lambda-item
	  (send modal-button-proto :new
		(modal-text "Power Lambda  ...")
		:action
		#'(lambda ()
		    (send x :set-power-lambda
			  (get-real-dialog
			   "Power Lambda:"
			   :initial (car (send x :set-power-lambda 'what)))))))

	 (ic-item
	  (get-linked-toggle-item x 'ic                  ))

	 (bic-item
	  (get-linked-toggle-item x 'bic                 ))

	 (ic-kappa-item
	  (send modal-button-proto :new
		(modal-text "IC Kappa  ...")
		:action
		#'(lambda ()
		    (send x :set-switch 'ic 'on)
		    (send x :set-switch 'bic nil)
		    (send x :set-ic 'kappa
			  (get-real-dialog
			   "IC Kappa:"
			   :initial (car (send x :set-ic 'kappa 'what)))))))

	 (decomposable-mode-item
	  (get-linked-toggle-item x 'decomposable-mode   ))

	 (acceptance-item
	  (send modal-button-proto :new
		(modal-text "Acceptance limit ...")
		:action
		#'(lambda ()
		    (send x :set-acceptance
			  (get-real-dialog
			   "Acceptance limit:"
			   :initial (car (send x :set-acceptance 'what)))))))

	 (rejection-item
	  (send modal-button-proto :new
		(modal-text "Rejection limit ...")
		:action
		#'(lambda ()
		    (send x :set-rejection
			  (get-real-dialog
			   "Rejection limit:"
			   :initial (car (send x :set-rejection 'what)))))))

	 (components-item
	  (send modal-button-proto :new
		(modal-text "Components limit ...")
		:action
		#'(lambda ()
		    (send x :set-components
			  (get-real-dialog
			   "Components limit:"
			   :initial (car (send x :set-components 'what)))))))

	 (separators-item
	  (send modal-button-proto :new
		(modal-text "Separators limit ...")
		:action
		#'(lambda ()
		    (send x :set-separators
			  (get-real-dialog
			   "Separators limit:"
			   :initial (car (send x :set-separators 'what)))))))

	 (reuse-tests-item
	  (get-linked-toggle-item x 'reuse-tests         ))

	 (algorithm-item
	  (send modal-button-proto :new
		(modal-text "Algorithm: ...")
		:action #'(lambda ()
			    (send x :set-algorithm
				  (get-choice-dialog
				   "Algorithm:"
				   (list 'a 'b 'c 'd)
				   (send x :set-algorithm))))))

	 (test-item
	  (send modal-button-proto :new
		(modal-text "Selection criteria: ...")
		:action #'(lambda ()
			    (send x :set-test
				  (get-choice-dialog
				   "Selection criteria:"
				   (list 'deviance  'pearson 'power)
				   (send x :set-test))))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, tests ")
		:action #'(lambda () (send x :status 'tests))))

	 (fix-status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, Fixing ")
		:action #'(lambda () (send x :status 'fix))))

	 (fix-edges-item
	  (send modal-button-proto :new
		(modal-text "Fix edges (CoCo-object) ...")
		:action #'(lambda ()
			    (send x :fix-edges
				  (get-string-dialog
				   "Edges (as generating class):"
				   :initial (send x :return-fix 'edges))))))
	 (eh-status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, EH procedure")
		:action #'(lambda () (send x :status 'eh))))

	 (graphical-search-item
	  (get-linked-toggle-item x 'graphical-search    ))


	 (option-tests-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-tests)
		      status-item
		      algorithm-item
		      partitioning-item adjusted-df-item
		      power-lambda-item
		      ic-item bic-item ic-kappa-item
		      decomposable-mode-item
		      test-item
		      acceptance-item rejection-item
		      components-item separators-item
		      reuse-tests-item
		      fix-status-item fix-edges-item
		      eh-status-item graphical-search-item
		      )))))
  )


;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; exact tests in the CoCo-object.

(defun option-exact-dialog (x)

  (let* ((dismiss-option-exact
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-exact-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, Exact tests ")
		:action #'(lambda () (send x :status 'exact))))

	 (exact-test-item
	  (get-linked-toggle-item x 'exact-test          ))

	 (epsilon-item
	  (send modal-button-proto :new
		(modal-text "Epsilon ...")
		:action
		#'(lambda ()
		    (send x :set-exact-epsilon
			  (get-real-dialog
			   "Epsilon limit:"
			   :initial (car (send x :set-exact-epsilon 'what)))))))

	 (exact-test-total-item
	  (get-linked-toggle-item x 'exact-test-total    ))

	 (exact-test-parts-item
	  (get-linked-toggle-item x 'exact-test-parts    ))

	 (exact-test-unparted-item
	  (get-linked-toggle-item x 'exact-test-unparted ))

	 (exact-only-log-l-item
	  (get-linked-toggle-item x 'exact-only-log-l    ))

	 (number-item
	  (send modal-button-proto :new
		(modal-text "Number of tables to generate ...")
		:action
		#'(lambda ()
		    (send x :set-number-of-tables
			  (get-real-dialog
			   "Number of tables to generate:"
			   :initial (car (send x :set-number-of-tables
					       'what)))))))

	 (numbers-item
	  (send modal-button-proto :new
		(modal-text "List of Number of tables to generate .?.")
		:action
		#'(lambda ()
		    (send x :set-list-of-number-of-tables
			  (car
			   (get-value-dialog
			    "List of Number of tables to generate:"
			    :initial
			    `',(send x :set-list-of-number-of-tables'what)
			    :text-length 40
			    ))))))

	 (asymptotic-item
	  (send modal-button-proto :new
		(modal-text "Asymptotic limit ...")
		:action
		#'(lambda ()
		    (send x :set-asymptotic
			  (get-real-dialog
			   "Asymptotic limit:"
			   :initial (car (send x :set-asymptotic 'what)))))))

	 (fast-item
	  (get-linked-toggle-item x 'fast                ))

	 (seed-item
	  (send modal-button-proto :new
		(modal-text "Seed for random generator ...")
		:action
		#'(lambda ()
		    (send x :set-seed
			  (get-real-dialog
			   "Seed limit:"
			   :initial (car (send x :set-seed 'what)))))))

	 (option-exact-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-exact)
		      status-item
		      exact-test-item epsilon-item
		      exact-test-total-item
		      exact-test-parts-item exact-test-unparted-item
		      exact-only-log-l-item
		      number-item numbers-item
		      asymptotic-item fast-item seed-item
		      )))))
  )


;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; the IPS algorithm in the CoCo-object.

(defun option-ips-dialog (x)

  (let* ((dismiss-option-ips
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-ips-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, Ips")
		:action #'(lambda () (send x :status 'ips))))

	 (criterion-item
	  (send modal-button-proto :new
		(modal-text "Criterion, Cell/Sum: ...")
		:action #'(lambda ()
			    (send x :set-ips-stop-criterion
				  (get-choice-dialog
				   "Stop criterion:"
				   (list 'sum 'cell)
				   (send x :set-ips-stop-criterion))))))

	 (epsilon-item
	  (send modal-button-proto :new
		(modal-text "Stopping criterion, Epsilon ...")
		:action
		#'(lambda ()
		    (send x :set-ips-epsilon
			  (get-real-dialog
			   "Epsilon:"
			   :initial (car (send x :set-ips-epsilon 'what)))))))

	 (ips-iterations-item
	  (send modal-button-proto :new
		(modal-text "Maximal number of Iterations ...")
		:action
		#'(lambda ()
		    (send x :set-ips-max-iterations
			  (get-real-dialog
			   "Maximal number of Iterations:"
			   :initial (car (send x :set-ips-max-iterations
					       'what)))))))

	 (option-ips-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-ips)
		      status-item
		      criterion-item epsilon-item
		      ips-iterations-item
		      )))))
  )


;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; the EM algorithm in the CoCo-object.

(defun option-em-dialog (x)

  (let* ((dismiss-option-em
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-em-dialog-window :remove))))
	 (em-item
	  (get-linked-toggle-item x 'em                  ))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, Em")
		:action #'(lambda () (send x :status 'em))))

	 (initial-item
	  (send modal-button-proto :new
		(modal-text "Initial values: ...")
		:action #'(lambda ()
			    (send x :set-em-initial
				  (get-choice-dialog
				   "Initial value:"
				   (list 'uniform 'first 'last
					 'mean 'random 'input)
				   (send x :set-em-initial))))))

	 (epsilon-item
	  (send modal-button-proto :new
		(modal-text "Stopping criterion, Epsilon ...")
		:action
		#'(lambda ()
		    (send x :set-em-epsilon
			  (get-real-dialog
			   "Epsilon:"
			   :initial (car (send x :set-em-epsilon 'what)))))))

	 (em-iterations-item
	  (send modal-button-proto :new
		(modal-text "Maximal number of Iterations ...")
		:action
		#'(lambda ()
		    (send x :set-em-max-iterations
			  (get-real-dialog
			   "Maximal number of Iterations:"
			   :initial (car (send x :set-em-max-iterations
					       'what)))))))

	 (option-em-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-em)
		      status-item
		      em-item
		      initial-item epsilon-item
		      em-iterations-item
		      )))))
  )


;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; datastructure in the CoCo-object.

(defun option-limits-dialog (x)

  (let* ((dismiss-option-limits
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-limits-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, limit options ")
		:action #'(lambda () (send x :status 'limits))))

	 (large-item
	  (get-linked-toggle-item x 'large               ))
	 (huge-item
	  (get-linked-toggle-item x 'huge                ))
	 (sorted-item
	  (get-linked-toggle-item x 'sorted              ))

	 (option-limits-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-limits)
		      status-item
		      large-item
		      huge-item
		      sorted-item
		      )))))
  )


;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; the files in the CoCo-object.

(defun option-files-dialog (x)

  (let* ((dismiss-option-files
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-files-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, files")
		:action #'(lambda () (send x :status 'files))))

	 (keyboard-item
	  (get-linked-toggle-item x 'keyboard            ))

	 (diary-item
	  (get-linked-toggle-item x 'diary               ))

	 (diary-file-item
	  (send modal-button-proto :new
		(modal-text "Name Diary-File ...")
		:action
		#'(lambda ()
		    (send x :set-diary-file
			  (get-string-dialog
			   "Diary file name:"
			   :initial (send x :set-diary-file 'what)))
		    (send x :set-switch 'diary 'on)
		    (send x :set-switch 'keep-diary 'on)
		    )))

	 (keep-diary-item
	  (get-linked-toggle-item x 'keep-diary          ))

	 (log-item
	  (get-linked-toggle-item x 'log                 ))

	 (log-data-item
	  (get-linked-toggle-item x 'log-data            ))

	 (log-file-item
	  (send modal-button-proto :new
		(modal-text "Name Log-File ...")
		:action
		#'(lambda ()
		    (send x :set-log-file
			  (get-string-dialog
			   "Log file name:"
			   :initial (send x :set-log-file 'what)))
		    (send x :set-switch 'log 'on)
		    (send x :set-switch 'keep-log 'on)
		    )))

	 (keep-log-item
	  (get-linked-toggle-item x 'keep-log            ))

	 (dump-item
	  (get-linked-toggle-item x 'dump                ))

	 (dump-file-item
	  (send modal-button-proto :new
		(modal-text "Name Dump-File ...")
		:action
		#'(lambda ()
		    (send x :set-dump-file
			  (get-string-dialog
			   "Dump file name:"
			   :initial (send x :set-dump-file 'what)))
		    (send x :set-switch 'dump 'on)
		    (send x :set-switch 'keep-dump 'on)
		    )))

	 (keep-dump-item
	  (get-linked-toggle-item x 'keep-dump           ))

	 (report-item
	  (get-linked-toggle-item x 'report              ))

	 (report-file-item
	  (send modal-button-proto :new
		(modal-text "Name Report-File ...")
		:action
		#'(lambda ()
		    (send x :set-report-file
			  (get-string-dialog
			   "Report file name:"
			   :initial (send x :set-report-file 'what)))
		    (send x :set-switch 'report 'on)
		    (send x :set-switch 'keep-report 'on)
		    )))

	 (keep-report-item
	  (get-linked-toggle-item x 'keep-report         ))

	 (option-files-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-files)
		      status-item
		      keyboard-item
		      (list diary-item keep-diary-item) diary-file-item
		      (list log-item log-data-item keep-log-item) log-file-item
		      (list dump-item keep-dump-item) dump-file-item
		      (list report-item keep-report-item) report-file-item
		      )))))
  )


;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting ``other'' options
;;; in the CoCo-object.

(defun option-other-dialog (x)

  (let* ((dismiss-option-other
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-other-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, other options ")
		:action #'(lambda () (send x :status 'other))))

	 (graph-mode-item
	  (get-linked-toggle-item x 'graph-mode          ))
	 (report-item
	  (get-linked-toggle-item x 'report              ))
	 (trace-item
	  (get-linked-toggle-item x 'trace               ))
	 (debug-item
	  (get-linked-toggle-item x 'debug               ))
	 (timer-item
	  (get-linked-toggle-item x 'timer               ))
	 (echo-item
	  (get-linked-toggle-item x 'echo                ))
	 (note-item
	  (get-linked-toggle-item x 'note                ))
	 (option-item
	  (get-linked-toggle-item x 'option              ))
	 (warnings-item
	  (get-linked-toggle-item x 'warnings            ))

	 (option-other-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-other)
		      status-item
		      graph-mode-item
		      report-item
		      trace-item
		      debug-item
		      timer-item
		      echo-item
		      note-item
		      option-item
		      warnings-item
		      )))))
  )


;;

(provide "cocooptions")
