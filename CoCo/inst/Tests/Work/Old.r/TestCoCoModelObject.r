
### TestCoCoModelObject

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  MainEffects <- make.model(".")
  printModel(MainEffects)

  Saturated <- make.model("*")
  printModel(Saturated)

  "current"
  current()

  Model.3 <- make.model("ACDE,ABCF.;")
  printModel(Model.3)

  editModel(action = "drop.interactions", modification = "ABCF.;")
  Model.4 <- make.model("last")
  printModel(Model.4)

  editModel(action = "drop.edges", modification = "AC,BF.;")
  Model.5 <- make.model("last")
  printModel(Model.5)

  Model.6 <- make.model("ACE,ADE,BC,F;")
  printModel(Model.6)

  "backward 1"
  backward(only = T, reversed = F, sorted = T, short = F,
           p.accepted = F, p.rejected = F, decomposable.mode = T,
           recursive = T, coherent = T, headlong = T, 
           follow = T, least.significant = T, separators = F, edges = T,
           model = F)
  Model.backward.from.saturated <- make.model("last")
  printModel(Model.backward.from.saturated)

  "backward 2"
  backward(only = T, reversed = F, sorted = T, short = F,
           p.accepted = F, p.rejected = F, decomposable.mode = T,
           recursive = T, coherent = T, headlong = T, 
           follow = T, least.significant = T, separators = F, edges = T,
           model = Model.3)
  Model.backward.from.3 <- make.model("last")
  printModel(Model.backward.from.3)

  printModel("all")

  printModel(MainEffects)
  printModel(Saturated)
  printModel(Model.3)
  printModel(Model.4)
  printModel(Model.5)
  printModel(Model.6)
  printModel(Model.backward.from.saturated)
  printModel(Model.backward.from.3)

  make.current(Model.backward.from.3)
  printModel("all")
  Model.3
  make.base(Model.3)
  printModel("all")

  printTable("expected", "*", MainEffects)
  printTable("expected", "*", Saturated)
  printTable("expected", "*", Model.3)
  printTable("expected", "*", Model.4)
  printTable("expected", "*", Model.5)
  printTable("expected", "*", Model.6)
  printTable("observed", "D", Saturated)
  printTable("expected", "*", Model.backward.from.saturated)
  printTable("observed", "E", Saturated)
  printTable("expected", "*", Model.backward.from.3)
  printTable("observed", "F", Saturated)

  rm( ReinisCoCoObject, Saturated, MainEffects, Model.3, Model.4, Model.5, Model.6)
  rm( Model.backward.from.3, Model.backward.from.saturated)

  .quit()
