
### TestTest

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  "slice" ;                           slice ("AB") ;
  "slice" ;                           slice ("B", "C") ;
  "slice" ;                           slice ("CD", "E") ;
  "slice" ;                           slice ("DE|F") ;
  "slice" ;                           slice ("E", "F", "A") ;

  read.model(".;")
  read.model("*;")
  base()
  read.model("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  read.model("ACE,ADE,BC,F.;")
  read.model(".;")

  make.base(3)
  make.current(6)

  printModel("all")

  "print.common.decompositions" ;     .printCommonDecompositions () ;
  "decompose.models" ;                .decompose.models ("AC") ;
  "test" ;                            test () ;
  "find.log.l" ;                      find.log.l () ;
  "find.deviance" ;                   find.deviance () ;

  "XXX.print.test" ;                  ### XXX.print.test () ;

  "compute.test" ;                    compute.test () ;

  printModel("all")

  "compute.test (\"current\", \"base\"     ) " ;   compute.test ("current", "base"     ) ;
  "compute.test (\"current\", \"current\"  ) " ;   compute.test ("current", "current"  ) ;
  "compute.test (\"current\", \"last\"     ) " ;   compute.test ("current", "last"     ) ;
  "compute.test (\"current\", 4            ) " ;   compute.test ("current", 4          ) ;
# "compute.test (\"current\", \"previous\" ) " ;   compute.test ("current", "previous" ) ;
  "compute.test (\"current\", 5            ) " ;   compute.test ("current", 5          ) ;
# "compute.test (\"current\", \"next\"     ) " ;   compute.test ("current", "next"     ) ;
  "compute.test (\"current\", \"*\"        ) " ;   compute.test ("current", "*"        ) ;
  "compute.test (\"current\", \".\"        ) " ;   compute.test ("current", "."        ) ;
  "compute.test (\"*\", \"current\"        ) " ;   compute.test ("*", "current"        ) ;
  "compute.test (\".\", \"current\"        ) " ;   compute.test (".", "current"        ) ;

  "XXX.print.deviance" ;              ### XXX.print.deviance () ;

  printModel("all")

  "compute.deviance" ;                compute.deviance () ;

  "compute.deviance (\"current\", \"base\"     ) " ;   compute.deviance ("current", "base"     ) ;
  "compute.deviance (\"current\", \"current\"  ) " ;   compute.deviance ("current", "current"  ) ;
  "compute.deviance (\"current\", \"last\"     ) " ;   compute.deviance ("current", "last"     ) ;
  "compute.deviance (\"current\", 4            ) " ;   compute.deviance ("current", 4          ) ;
# "compute.deviance (\"current\", \"previous\" ) " ;   compute.deviance ("current", "previous" ) ;
  "compute.deviance (\"current\", 5            ) " ;   compute.deviance ("current", 5          ) ;
# "compute.deviance (\"current\", \"next\"     ) " ;   compute.deviance ("current", "next"     ) ;
  "compute.deviance (\"current\", \"*\"        ) " ;   compute.deviance ("current", "*"        ) ;
  "compute.deviance (\"current\", \".\"        ) " ;   compute.deviance ("current", "."        ) ;
  "compute.deviance (\"*\", \"current\"        ) " ;   compute.deviance ("*", "current"        ) ;
  "compute.deviance (\".\", \"current\"        ) " ;   compute.deviance (".", "current"        ) ;

  printModel("all")

  "exact.test" ;                      test(exact.test = TRUE) ;

  "partitioning.test" ;               test(break.down = "components") ;
  "test.one.edge" ;                   test(only.if.one.edge = T) ;
  "factorize" ;                       test(break.down = "edges") ;
  "factorize" ;                       test(break.down = "edges") ;
  "factorize" ;                       test(break.down = "edges", set = "ABCDEF") ;
  "factorize" ;                       test(break.down = "edges", set = "FEDCBA") ;

  "factorize" ;                       test(break.down = "interactions" ) ;
  "factorize" ;                       test(break.down = "interactions", set = "ABCDEF") ;
  "factorize" ;                       test(break.down = "interactions", set = "FEDCBA") ;

  "show.tests" ;                      show.tests () ;
  "dispose.of.tests" ;                clean.up(code = "tests") ;
  "show.tests" ;                      show.tests () ;

  status("observations")

  "dispose.of.tables" ;               clean.up(code = "tables") ;

  status("observations")

  "dispose.of.probabilities" ;        clean.up(code = "estimates") ;

  status("data")

  .quit()
