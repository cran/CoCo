
### TestModel

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  "read.model" ;                      enterModel () ;
  "return.model" ;                    returnModel () ;
  "return.model.set" ;                returnModelVariates (as.string = FALSE) ;
  "return.model.set.string" ;         returnModelVariates (as.string = TRUE ) ;
  "read.n.interactions" ;             enterModel (order = 2) ;
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

  showModel("all")

  "read.model" ;                      enterModel ("*") ;
  "return.model" ;                    returnModel ("*") ;
  "return.model.set" ;                returnModelVariates ("*", as.string = FALSE) ;
  "return.model.set.string" ;         returnModelVariates ("*", as.string = TRUE ) ;
  "read.n.interactions" ;             enterModel (order = 2, "*") ;
  "make.model" ;                      make.model ("*") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "*") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "*",     omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.", omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "*",     omit.test = F) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.", omit.test = F) ;

  "read.model" ;                      enterModel ("ACE") ;
  "return.model" ;                    returnModel () ;
  "return.model.set" ;                returnModelVariates (as.string = FALSE) ;
  "return.model.set.string" ;         returnModelVariates (as.string = TRUE ) ;

  showModel("all")

  "read.model" ;                      enterModel ("ACE,ADE,BC,F.;") ;
  "return.model" ;                    returnModel ("ACE,ADE,BC,F.;") ;
  "return.model.set" ;                returnModelVariates ("ACE,ADE,BC,F.;", as.string = FALSE) ;
  "return.model.set.string" ;         returnModelVariates ("ACE,ADE,BC,F.;", as.string = TRUE ) ;
  "read.n.interactions" ;             enterModel ("ACE,ADE,BC,F.;", order = 2) ;
  "make.model" ;                      make.model ("ACE,ADE,BC,F.;") ;
  "collaps.model" ;                   editModel (action = "collaps.model", model = "ACE,ADE,BC,F.;") ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.") ;
  "collaps.model" ;                   editModel (action = "collaps.model", model = "ACE,ADE,BC,F.;", omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.",          omit.test = T) ;
  "collaps.model" ;                   editModel (action = "collaps.model", model = "ACE,ADE,BC,F.;", omit.test = F) ;
  "collaps.model" ;                   editModel (action = "collaps.model", "ABCD.",          omit.test = F) ;

  showModel("all")

  quitCoCo()
