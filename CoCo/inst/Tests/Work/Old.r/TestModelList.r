
### TestModelList

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  read.model(".;")
  read.model("*;")
  base()
  read.model("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  read.model("ACE,ADE,BC,F.;")
  read.model(".;")

  printModel("all")

  make.base(2)
  make.current(6)

  printModel("all")

  "make.base (\"base\"     ) " ;      make.base ("base"     ) ; printModel("base") ;
  "make.base (\"current\"  ) " ;      make.base ("current"  ) ; printModel("base") ;
  "make.base (\"last\"     ) " ;      make.base ("last"     ) ; printModel("base") ;
  "make.base (3            ) " ;      make.base (3          ) ; printModel("base") ;
  "make.base (\"previous\" ) " ;      make.base ("previous" ) ; printModel("base") ;
  "make.base (3            ) " ;      make.base (3          ) ; printModel("base") ;
  "make.base (\"next\"     ) " ;      make.base ("next"     ) ; printModel("base") ; 

  printModel("all")

  make.base(2)
  make.current(6)

  printModel("all")

  "make.current (\"base\"     ) " ;   make.current ("base"     ) ; printModel("current") ;
  "make.current (\"current\"  ) " ;   make.current ("current"  ) ; printModel("current") ;
  "make.current (\"last\"     ) " ;   make.current ("last"     ) ; printModel("current") ;
  "make.current (3            ) " ;   make.current (3          ) ; printModel("current") ;
  "make.current (\"previous\" ) " ;   make.current ("previous" ) ; printModel("current") ;
  "make.current (3            ) " ;   make.current (3          ) ; printModel("current") ;
  "make.current (\"next\"     ) " ;   make.current ("next"     ) ; printModel("current") ;

  "current" ;                         current () ;
  "base" ;                            base () ;

  printModel("all")

  make.base(2)
  make.current(6)

  printModel("all")

  "return.model.number (\"base\"     ) " ;   return.model.number ("base"     ) ;
  "return.model.number (\"current\"  ) " ;   return.model.number ("current"  ) ;
  "return.model.number (\"last\"     ) " ;   return.model.number ("last"     ) ;
  "return.model.number (3            ) " ;   return.model.number (3          ) ;
  "return.model.number (\"previous\" ) " ;   return.model.number ("previous" ) ;
  "return.model.number (3            ) " ;   return.model.number (3          ) ;
  "return.model.number (\"next\"     ) " ;   return.model.number ("next"     ) ;

  "XXX.return.edge.list.list" ;       ### XXX.return.edge.list.list () ;
  "XXX.return.edge.list" ;            ### XXX.return.edge.list () ;

  "return.fix (\"what\")  " ;         return.fix ("what")  ;
  "return.fix (\"edges\") " ;         return.fix ("edges") ;
  "return.fix (\"in\")    " ;         return.fix ("in")    ;
  "return.fix (\"out\")   " ;         return.fix ("out")   ;

  fix.edges("AB,BC,CA");
  eh.fix("DE,EF,FD", fix = "out");
  eh.fix("CDE", fix = "in");
  "return.fix (\"what\")  " ;         return.fix ("what")  ;
  "return.fix (\"edges\") " ;         return.fix ("edges") ;
  "return.fix (\"in\")    " ;         return.fix ("in")    ;
  "return.fix (\"out\")   " ;         return.fix ("out")   ;

  "coco.print.formula" ;              printFormula () ;

  "return.expression" ;               return.complex (model = "*", type = "expression") ;
  "XXX.return.components" ;           #### XXX.return.components (model = "*") ;

  "print.vertex.order" ;              printVertexOrder () ;
  "dispose.of.formula" ;              dispose.of.fitted.values () ;

   .quit()
