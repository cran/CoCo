
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

