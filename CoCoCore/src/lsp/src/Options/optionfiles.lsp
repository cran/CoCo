
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

