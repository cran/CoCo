
### TestModelList

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  enterModel(".;")
  enterModel("*;")
  makeBase("current")
  enterModel("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  enterModel("ACE,ADE,BC,F.;")
  enterModel(".;")

  showModel("all")

  makeBase(2)
  makeCurrent(6)

  showModel("all")

  "makeBase (\"base\"     ) " ;      makeBase ("base"     ) ; showModel("base") ;
  "makeBase (\"current\"  ) " ;      makeBase ("current"  ) ; showModel("base") ;
  "makeBase (\"last\"     ) " ;      makeBase ("last"     ) ; showModel("base") ;
  "makeBase (3            ) " ;      makeBase (3          ) ; showModel("base") ;
  "makeBase (\"previous\" ) " ;      makeBase ("previous" ) ; showModel("base") ;
  "makeBase (3            ) " ;      makeBase (3          ) ; showModel("base") ;
  "makeBase (\"next\"     ) " ;      makeBase ("next"     ) ; showModel("base") ; 

  showModel("all")

  makeBase(2)
  makeCurrent(6)

  showModel("all")

  "makeCurrent (\"base\"     ) " ;   makeCurrent ("base"     ) ; showModel("current") ;
  "makeCurrent (\"current\"  ) " ;   makeCurrent ("current"  ) ; showModel("current") ;
  "makeCurrent (\"last\"     ) " ;   makeCurrent ("last"     ) ; showModel("current") ;
  "makeCurrent (3            ) " ;   makeCurrent (3          ) ; showModel("current") ;
  "makeCurrent (\"previous\" ) " ;   makeCurrent ("previous" ) ; showModel("current") ;
  "makeCurrent (3            ) " ;   makeCurrent (3          ) ; showModel("current") ;
  "makeCurrent (\"next\"     ) " ;   makeCurrent ("next"     ) ; showModel("current") ;

  "current" ;                         makeCurrent("last") ;
  "base" ;                            makeBase("current") ;

  showModel("all")

  makeBase(2)
  makeCurrent(6)

  showModel("all")

  "returnModelNumber (\"base\"     ) " ;   returnModelNumber ("base"     ) ;
  "returnModelNumber (\"current\"  ) " ;   returnModelNumber ("current"  ) ;
  "returnModelNumber (\"last\"     ) " ;   returnModelNumber ("last"     ) ;
  "returnModelNumber (3            ) " ;   returnModelNumber (3          ) ;
  "returnModelNumber (\"previous\" ) " ;   returnModelNumber ("previous" ) ;
  "returnModelNumber (3            ) " ;   returnModelNumber (3          ) ;
  "returnModelNumber (\"next\"     ) " ;   returnModelNumber ("next"     ) ;

  "XXX.return.edge.list.list" ;       ### XXX.return.edge.list.list () ;
  "XXX.return.edge.list" ;            ### XXX.return.edge.list () ;

  "returnFix (\"what\")  " ;         returnFix ("what")  ;
  "returnFix (\"edges\") " ;         returnFix ("edges") ;
  "returnFix (\"in\")    " ;         returnFix ("in")    ;
  "returnFix (\"out\")   " ;         returnFix ("out")   ;

  .fix.edges("AB,BC,CA");
  ehForceFix("DE,EF,FD", fix = "out");
  ehForceFix("CDE", fix = "in");
  "returnFix (\"what\")  " ;         returnFix ("what")  ;
  "returnFix (\"edges\") " ;         returnFix ("edges") ;
  "returnFix (\"in\")    " ;         returnFix ("in")    ;
  "returnFix (\"out\")   " ;         returnFix ("out")   ;

  "coco.print.formula" ;              showFormula () ;

  "return.expression" ;               returnExpression (model = "*", type = "expression") ;
  "XXX.return.components" ;           #### XXX.returnSets (model = "*") ;

  "print.vertex.order" ;              showVertexOrder () ;
  "dispose.of.formula" ;              .dispose.of.fitted.values () ;

   quitCoCo()
