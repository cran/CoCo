
(def x (make-coco))

(print (def TestSpecificationFile (send x :set-specification-file 'what)))
(print (def TestObservationsFile  (send x :set-observations-file  'what)))
(print (def TestDataFile          (send x :set-data-file          'what)))
(print (def TestDiaryFile         (send x :set-diary-file         'what)))
(print (def TestLogFile           (send x :set-log-file           'what)))
(print (def TestDumpFile          (send x :set-dump-file          'what)))
(print (def TestReportFile        (send x :set-report-file        'what)))
                                                                 
(print (def TestSource            (send x :coco-source            'what)))
(print (def Testoutput            (send x :set-output             'what)))

(print (send x :status 'files))

(print (send x :set-specification-file "TestSpecificationFile"  )) 
(print (send x :set-observations-file  "TestObservationsFile"   )) 
(print (send x :set-data-file          "TestDataFile"           )) 
(print (send x :set-diary-file         "TestDiaryFile"          )) 
(print (send x :set-log-file           "TestLogFile"            )) 
(print (send x :set-dump-file          "TestDumpFile"           )) 
(print (send x :set-report-file        "TestReportFile"         )) 
				                                  
;(print (send x :coco-source            "TestSource"             )) 
;(print (send x :set-output             "Testoutput"             )) 

(print (send x :status 'files))

(print (send x :set-specification-file )) 
(print (send x :set-observations-file  )) 
(print (send x :set-data-file          )) 
(print (send x :set-diary-file         )) 
(print (send x :set-log-file           )) 
(print (send x :set-dump-file          )) 
(print (send x :set-report-file        )) 
                                       
(print (send x :coco-source            )) 
(print (send x :set-output             ))

(print (send x :status 'files))

(print (send x :set-specification-file TestSpecificationFile )) 
(print (send x :set-observations-file  TestObservationsFile  )) 
(print (send x :set-data-file          TestDataFile          )) 
(print (send x :set-diary-file         TestDiaryFile         )) 
(print (send x :set-log-file           TestLogFile           )) 
(print (send x :set-dump-file          TestDumpFile          )) 
(print (send x :set-report-file        TestReportFile        )) 
				                                 
;(print (send x :coco-source            TestSource            )) 
;(print (send x :set-output             Testoutput            )) 

(print (send x :status 'files))

(print (send x :set-specification-file nil)) 
(print (send x :set-observations-file  nil)) 
(print (send x :set-data-file          nil)) 
(print (send x :set-diary-file         nil)) 
(print (send x :set-log-file           nil)) 
(print (send x :set-dump-file          nil)) 
(print (send x :set-report-file        nil)) 
                                       
(print (send x :coco-source            nil)) 
(print (send x :set-output             nil))

(print (send x :status 'files))
