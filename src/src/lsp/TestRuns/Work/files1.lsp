
(def x (make-coco))

(def TestSpecificationFile (send x :set-specification-file 'what) )
(def TestObservationsFile  (send x :set-observations-file  'what) )
(def TestDataFile          (send x :set-data-file          'what) )
(def TestDiaryFile         (send x :set-diary-file         'what) )
(def TestLogFile           (send x :set-log-file           'what) )
(def TestDumpFile          (send x :set-dump-file          'what) )
(def TestReportFile        (send x :set-report-file        'what) )
                                                                 
(def TestSource            (send x :coco-source            'what) )
(def Testoutput            (send x :set-output             'what) )

(send x :status 'files)

(send x :set-specification-file "TestSpecificationFile"  ) 
(send x :set-observations-file  "TestObservationsFile"   ) 
(send x :set-data-file          "TestDataFile"           ) 
(send x :set-diary-file         "TestDiaryFile"          ) 
(send x :set-log-file           "TestLogFile"            ) 
(send x :set-dump-file          "TestDumpFile"           ) 
(send x :set-report-file        "TestReportFile"         ) 
				                                  
;(send x :coco-source            "TestSource"             ) 
;(send x :set-output             "Testoutput"             ) 

(send x :status 'files)

(send x :set-specification-file ) 
(send x :set-observations-file  ) 
(send x :set-data-file          ) 
(send x :set-diary-file         ) 
(send x :set-log-file           ) 
(send x :set-dump-file          ) 
(send x :set-report-file        ) 
                                       
(send x :coco-source            ) 
(send x :set-output             )

(send x :status 'files)

(send x :set-specification-file TestSpecificationFile ) 
(send x :set-observations-file  TestObservationsFile  ) 
(send x :set-data-file          TestDataFile          ) 
(send x :set-diary-file         TestDiaryFile         ) 
(send x :set-log-file           TestLogFile           ) 
(send x :set-dump-file          TestDumpFile          ) 
(send x :set-report-file        TestReportFile        ) 
				                                 
;(send x :coco-source            TestSource            ) 
;(send x :set-output             Testoutput            ) 

(send x :status 'files)

(send x :set-specification-file nil) 
(send x :set-observations-file  nil) 
(send x :set-data-file          nil) 
(send x :set-diary-file         nil) 
(send x :set-log-file           nil) 
(send x :set-dump-file          nil) 
(send x :set-report-file        nil) 
                                       
(send x :coco-source            nil) 
(send x :set-output             nil)

(send x :status 'files)
