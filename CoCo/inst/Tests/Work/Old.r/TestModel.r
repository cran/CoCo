
### TestModel

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  "read.model" ;                      read.model () ;
  "return.model" ;                    return.model () ;
  "return.model.set" ;                return.model.set (as.string = FALSE) ;
  "return.model.set.string" ;         return.model.set (as.string = TRUE ) ;
  "read.n.interactions" ;             read.model (order = 2) ;
  "make.model" ;                      make.model () ;
  "collaps.model" ;                   editModel (action = "collaps.model", modification = "*") ;
  "collaps.model" ;                   editModel (action = "collaps.model", omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", omit.test = F) ;
  "normal.to.dual" ;                  editModel (action = "normal.to.dual"  ) ;
  "dual.to.normal" ;                  editModel (action = "dual.to.normal"  ) ;
  "normal.to.dual" ;                  editModel (action = "normal.to.dual", omit.test = T) ;
  "dual.to.normal" ;                  editModel (action = "dual.to.normal", omit.test = T) ;
  "normal.to.dual" ;                  editModel (action = "normal.to.dual", omit.test = F) ;
  "dual.to.normal" ;                  editModel (action = "dual.to.normal", omit.test = F) ;

  printModel("all")

  "read.model" ;                      read.model ("*") ;
  "return.model" ;                    return.model ("*") ;
  "return.model.set" ;                return.model.set ("*", as.string = FALSE) ;
  "return.model.set.string" ;         return.model.set ("*", as.string = TRUE ) ;
  "read.n.interactions" ;             read.model (order = 2, "*") ;
  "make.model" ;                      make.model ("*") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "*") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "*",     omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.", omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "*",     omit.test = F) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.", omit.test = F) ;

  "read.model" ;                      read.model ("ACE") ;
  "return.model" ;                    return.model () ;
  "return.model.set" ;                return.model.set (as.string = FALSE) ;
  "return.model.set.string" ;         return.model.set (as.string = TRUE ) ;

  printModel("all")

  "read.model" ;                      read.model ("ACE,ADE,BC,F.;") ;
  "return.model" ;                    return.model ("ACE,ADE,BC,F.;") ;
  "return.model.set" ;                return.model.set ("ACE,ADE,BC,F.;", as.string = FALSE) ;
  "return.model.set.string" ;         return.model.set ("ACE,ADE,BC,F.;", as.string = TRUE ) ;
  "read.n.interactions" ;             read.model ("ACE,ADE,BC,F.;", order = 2) ;
  "make.model" ;                      make.model ("ACE,ADE,BC,F.;") ;
  "collaps.model" ;                   editModel (action = "collaps.model", model = "ACE,ADE,BC,F.;") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.") ;
  "collaps.model" ;                   editModel (action = "collaps.model", model = "ACE,ADE,BC,F.;", omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.",          omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", model = "ACE,ADE,BC,F.;", omit.test = F) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.",          omit.test = F) ;

  printModel("all")

  .quit()
