
### TestEdit

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  read.model(".;")
  read.model("*;")
  base()
  read.model("ACDE,ABCF.;")

  editModel(action = "drop.interactions", modification = "ABCF.;")

  "generate.graphical" ;              editModel(action = "generate.graphical") ;
  "generate.graphical" ;              editModel(action = "generate.graphical", omit.test = FALSE) ;
  "generate.graphical" ;              editModel(action = "generate.graphical", omit.test = TRUE) ;
  printModel("all")

  editModel(action = "drop.edges", modification = "AC,BF.;")
  "generate.decomposable" ;           editModel(action = "generate.graphical") ;
  "generate.decomposable" ;           editModel(action = "generate.graphical", omit.test = FALSE) ;
  "generate.decomposable" ;           editModel(action = "generate.graphical", omit.test = TRUE) ;
  printModel("all")

  read.model("ACE,ADE,BC,F;")
  "drop.factor" ;                     editModel(action = "drop.factor", modification = "E") ;
  "drop.edges" ;                      editModel(action = "drop.edges",  modification = "AE") ;
  "add.edges" ;                       editModel(action = "add.edges",   modification = "CD") ;
  "drop.interactions" ;               editModel(action = "drop.interactions", modification = "AE") ;
  "add.interactions" ;                editModel(action = "add.interactions",  modification = "ACDE") ;
  printModel("all")

  "reduce.generator" ;                editModel(action = "reduce.generator", modification = "AE") ;
  "remove.generator" ;                editModel(action = "remove.generator", modification = "AE") ;
  "remove.total.interaction" ;        editModel(action = "remove.total.interaction", modification = "AE") ;
  printModel("all")

  read.model("ACE,ADE,BC,F;")
  base()
  read.model(order = 1, set = "*") ;

  "meet.of.models" ;                  editModel(action = "meet.of.models") ;
  "meet.of.models" ;                  editModel(action = "meet.of.models", omit.test = FALSE) ;
  "meet.of.models" ;                  editModel(action = "meet.of.models", omit.test = TRUE) ;
  printModel("all")

  "join.of.models" ;                  editModel(action = "join.of.models") ;
  "join.of.models" ;                  editModel(action = "join.of.models", omit.test = FALSE) ;
  "join.of.models" ;                  editModel(action = "join.of.models", omit.test = TRUE) ;
  printModel("all")

  "difference.of.models" ;            editModel(action = "difference.of.models", dispose = FALSE) ;
  "difference.of.models" ;            editModel(action = "difference.of.models", edges = F, dispose = FALSE) ;
  "difference.of.models" ;            editModel(action = "difference.of.models", edges = T, dispose = FALSE) ;
  printModel("all")

  "meet.of.models" ;                  editModel(action = "meet.of.models", dispose = TRUE) ;
  "meet.of.models" ;                  editModel(action = "meet.of.models", omit.test = F, dispose = TRUE) ;
  "meet.of.models" ;                  editModel(action = "meet.of.models", omit.test = T, dispose = TRUE) ;
  printModel("all")

  "join.of.models" ;                  editModel(action = "join.of.models", dispose = TRUE) ;
  "join.of.models" ;                  editModel(action = "join.of.models", omit.test = F, dispose = TRUE) ;
  "join.of.models" ;                  editModel(action = "join.of.models", omit.test = T, dispose = TRUE) ;
  printModel("all")

  "difference.of.models" ;            editModel(action = "difference.of.models", dispose = TRUE) ;
  "difference.of.models" ;            editModel(action = "difference.of.models", edges = F, dispose = TRUE) ;
  "difference.of.models" ;            editModel(action = "difference.of.models", edges = T, dispose = TRUE) ;
  printModel("all")

  .quit()
