
### TestPrintModel

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

  printModel("current")
  printModel("base")
  printModel("last")
  printModel("all")
  printModel("next") #
  printModel("previous") #
  printModel("interval", 1, 4) 
  printModel("number", 2)
  printModel("list", 2:3)
  printModel(2)
  printModel()
  printModel(F)
  printModel(T)

  printModel(2:3)
  printModel(c("current", "base"))
  printModel(c("base", "current"))

  printModel("interval", 1, 6)

  "print.interval" ;                  printModel (c(4, 5)) ;
  "print.interval" ;                  printModel ("interval", a = 4, b = 5) ;
  "print.models" ;                    printModel (c("base", "current")) ;
  "print.model" ;                     printModel () ;
  "describe.interval" ;               printModel (c(4, 5), describe.model = TRUE) ;
  "describe.models" ;                 printModel (c("base", "current"), describe.model = TRUE) ;
  "describe.model" ;                  printModel (describe.model = TRUE) ;
   printModel("all")
  "dispose.of.interval" ;             dispose.of.model (c(4, 5)) ;
   printModel("all")
   make.current(2)
   make.base(1)
  "dispose.of.models" ;               dispose.of.model (c("base", "current")) ;
   printModel("all")
  "dispose.of.model" ;                dispose.of.model ("all") ;
   printModel("all")

   .quit()
