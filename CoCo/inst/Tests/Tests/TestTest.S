
### TestTest

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  "slice" ;                           partialAssociations ("AB") ;
  "slice" ;                           partialAssociations ("B", "C") ;
  "slice" ;                           partialAssociations ("CD", "E") ;
  "slice" ;                           partialAssociations ("DE|F") ;
  "slice" ;                           partialAssociations ("E", "F", "A") ;

  enterModel(".;")
  enterModel("*;")
  makeBase("current")
  enterModel("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  enterModel("ACE,ADE,BC,F.;")
  enterModel(".;")

  makeBase(3)
  makeCurrent(6)

  showModel("all")

  "print.common.decompositions" ;     showTest(break.down = "show.common.decompositions" ) ;
  "decompose.models" ;                showTest(break.down = "decompose.models", set = "AC") ;
  "test" ;                            showTest () ;
  "find.log.l" ;                      .show.log.lik () ;
  "find.deviance" ;                   showDeviance () ;

  "XXX.print.test" ;                  ### XXX.print.showTest () ;

  "compute.test" ;                    returnTest () ;

  showModel("all")

  "returnTest (\"current\", \"base\"     ) " ;   returnTest ("current", "base"     ) ;
  "returnTest (\"current\", \"current\"  ) " ;   returnTest ("current", "current"  ) ;
  "returnTest (\"current\", \"last\"     ) " ;   returnTest ("current", "last"     ) ;
  "returnTest (\"current\", 4            ) " ;   returnTest ("current", 4          ) ;
# "returnTest (\"current\", \"previous\" ) " ;   returnTest ("current", "previous" ) ;
  "returnTest (\"current\", 5            ) " ;   returnTest ("current", 5          ) ;
# "returnTest (\"current\", \"next\"     ) " ;   returnTest ("current", "next"     ) ;
  "returnTest (\"current\", \"*\"        ) " ;   returnTest ("current", "*"        ) ;
  "returnTest (\"current\", \".\"        ) " ;   returnTest ("current", "."        ) ;
  "returnTest (\"*\", \"current\"        ) " ;   returnTest ("*", "current"        ) ;
  "returnTest (\".\", \"current\"        ) " ;   returnTest (".", "current"        ) ;

  "XXX.print.deviance" ;              ### XXX.print.deviance () ;

  showModel("all")

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

  showModel("all")

  "exact.test" ;                      showTest(exact.test = TRUE) ;

  "partitioning.test" ;               showTest(break.down = "components") ;
  "test.one.edge" ;                   showTest(only.if.one.edge = T) ;
  "factorize" ;                       showTest(break.down = "edges") ;
  "factorize" ;                       showTest(break.down = "edges") ;
  "factorize" ;                       showTest(break.down = "edges", set = "ABCDEF") ;
  "factorize" ;                       showTest(break.down = "edges", set = "FEDCBA") ;

  "factorize" ;                       showTest(break.down = "interactions" ) ;
  "factorize" ;                       showTest(break.down = "interactions", set = "ABCDEF") ;
  "factorize" ;                       showTest(break.down = "interactions", set = "FEDCBA") ;

  "show.tests" ;                      showTests () ;
  "dispose.of.tests" ;                .clean.up(code = "tests") ;
  "show.tests" ;                      showTests () ;

  showOptions("observations")

  "dispose.of.tables" ;               .clean.up(code = "tables") ;

  showOptions("observations")

  "dispose.of.probabilities" ;        .clean.up(code = "estimates") ;

  showOptions("data")

  quitCoCo()
