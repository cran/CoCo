
### TestEdit

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  ## read.model(".;")
  ## read.model("*;")
  read.model("ACDE,ABCF.;")

  base()

  editModel(action = "drop.interactions",        modification = "ABCF.;") ; printModel("last")
  editModel(action = "reduce.generator",         modification = "ABCF.;") ; printModel("last")
  editModel(action = "remove.generator",         modification = "ABCF.;") ; printModel("last")
  editModel(action = "remove.total.interaction", modification = "ABCF.;") ; printModel("last")

  printModel(1:5)

  editModel(action = "drop.interactions",        modification = "ABC.;") ; printModel("last")
  editModel(action = "reduce.generator",         modification = "ABC.;") ; printModel("last")
  editModel(action = "remove.generator",         modification = "ABC.;") ; printModel("last")
  editModel(action = "remove.total.interaction", modification = "ABC.;") ; printModel("last")

  printModel(6:9)

  read.model("ACE,ADE,BC,F;")

  editModel(action = "drop.interactions",        modification = "ACE.;") ; printModel("last")
  editModel(action = "reduce.generator",         modification = "ACE.;") ; printModel("last")
  editModel(action = "remove.generator",         modification = "ACE.;") ; printModel("last")
  editModel(action = "remove.total.interaction", modification = "ACE.;") ; printModel("last")

  printModel(11:14)

  editModel(action = "drop.interactions",        modification = "AE.;") ; printModel("last")
  editModel(action = "reduce.generator",         modification = "AE.;") ; printModel("last")
  editModel(action = "remove.generator",         modification = "AE.;") ; printModel("last")
  editModel(action = "remove.total.interaction", modification = "AE.;") ; printModel("last")

  printModel(15:18)

  .quit()
