
(def x (make-coco))

;;; (print (send x :end ))
;;; (print (send x :resume ))
;;; (print (send x :title ))
;;; (print (send x :current-coco ))

(def options
     (list 'Partitioning 'Keyboard 'Echo 'Diary 'Timer
		     'Graph-mode 'Decomposable-mode
		     'Large 'Short-test-output 'Report 'Reuse-tests
		     'Adjusted-df 'Trace 'Exact-test 'Exact-only-log-l 'Fast
		     'Exact-test-total 'Exact-test-parts 'Exact-test-unparted
		     'Graphical-search 'Note 'Debug 'Option 'Log 'Dump
		     'Sorted 'Keep-Diary 'Keep-Report 'Keep-Log 'Log-Data
		     'Keep-Dump 'Huge 'Ic 'BIC)) ;;; 'Pausing-of-output

(defun test-switch (option)
  (let ((old (send x :set-switch option 'what)))
    (print old)
    (print (send x :set-switch option 'flop))
    (print (send x :set-switch option 'what))
    (print (send x :set-switch option 'on))
    (print (send x :set-switch option 'what))
    (print (send x :set-switch option 'off))
    (print (send x :set-switch option 'what))
    (print (send x :set-switch option (if old 'on 'off)))
    (print (send x :set-switch option 'what))
    )
  )

(dolist (i options)
	(test-switch i))

(print (send x :status 'all))
(print (send x :status 'all))
(print (send x :status 'formats))
(print (send x :status 'tests))
(print (send x :status 'exact))
(print (send x :status 'fix))
(print (send x :status 'ips))
(print (send x :status 'em))
(print (send x :status 'old))
(print (send x :status 'specification))
(print (send x :status 'factors))
(print (send x :status 'observations))
(print (send x :status 'data))
(print (send x :status 'limits))
(print (send x :status 'files))
(print (send x :status 'other))
(print (send x :status 'search))


;;; ;;; (print (send x :set-signall ))
;;; ;;; (print (send x :set-interrupt ))


;;; (print (send m :compute-test-against-model-object ))
