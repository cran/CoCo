
### TestPrintModel

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

  showModel("current")
  showModel("base")
  showModel("last")
  showModel("all")
  showModel("next") #
  showModel("previous") #
  showModel("interval", 1, 4) 
  showModel("number", 2)
  showModel("list", 2:3)
  showModel(2)
  showModel()
  showModel(F)
  showModel(T)

  showModel(2:3)
  showModel(c("current", "base"))
  showModel(c("base", "current"))

  showModel("interval", 1, 6)

  "print.interval" ;                  showModel (c(4, 5)) ;
  "print.interval" ;                  showModel ("interval", a = 4, b = 5) ;
  "print.models" ;                    showModel (c("base", "current")) ;
  "print.model" ;                     showModel () ;
  "describe.interval" ;               showModel (c(4, 5), describe.model = TRUE) ;
  "describe.models" ;                 showModel (c("base", "current"), describe.model = TRUE) ;
  "describe.model" ;                  showModel (describe.model = TRUE) ;
   showModel("all")
  "dispose.of.interval" ;             disposeOfModel (c(4, 5)) ;
   showModel("all")
   makeCurrent(2)
   makeBase(1)
  "dispose.of.models" ;               disposeOfModel (c("base", "current")) ;
   showModel("all")
  "dispose.of.model" ;                disposeOfModel ("all") ;
   showModel("all")

   quitCoCo()
