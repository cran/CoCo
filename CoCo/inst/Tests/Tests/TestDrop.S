
### TestEdit

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  ## enterModel(".;")
  ## enterModel("*;")
  enterModel("ACDE,ABCF.;")

  makeBase("current")

  editModel(action = "drop.interactions",        modification = "ABCF.;") ; showModel("last")
  editModel(action = "reduce.generator",         modification = "ABCF.;") ; showModel("last")
  editModel(action = "remove.generator",         modification = "ABCF.;") ; showModel("last")
  editModel(action = "remove.total.interaction", modification = "ABCF.;") ; showModel("last")

  showModel(1:5)

  editModel(action = "drop.interactions",        modification = "ABC.;") ; showModel("last")
  editModel(action = "reduce.generator",         modification = "ABC.;") ; showModel("last")
  editModel(action = "remove.generator",         modification = "ABC.;") ; showModel("last")
  editModel(action = "remove.total.interaction", modification = "ABC.;") ; showModel("last")

  showModel(6:9)

  enterModel("ACE,ADE,BC,F;")

  editModel(action = "drop.interactions",        modification = "ACE.;") ; showModel("last")
  editModel(action = "reduce.generator",         modification = "ACE.;") ; showModel("last")
  editModel(action = "remove.generator",         modification = "ACE.;") ; showModel("last")
  editModel(action = "remove.total.interaction", modification = "ACE.;") ; showModel("last")

  showModel(11:14)

  editModel(action = "drop.interactions",        modification = "AE.;") ; showModel("last")
  editModel(action = "reduce.generator",         modification = "AE.;") ; showModel("last")
  editModel(action = "remove.generator",         modification = "AE.;") ; showModel("last")
  editModel(action = "remove.total.interaction", modification = "AE.;") ; showModel("last")

  showModel(15:18)

  quitCoCo()
