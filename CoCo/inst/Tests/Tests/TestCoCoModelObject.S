
### TestCoCoModelObject

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  MainEffects <- make.model(".")
  showModel(MainEffects)

  Saturated <- make.model("*")
  showModel(Saturated)

  "current"
  makeCurrent("last")

  Model.3 <- make.model("ACDE,ABCF.;")
  showModel(Model.3)

  editModel(action = "drop.interactions", modification = "ABCF.;")
  Model.4 <- make.model("last")
  showModel(Model.4)

  editModel(action = "drop.edges", modification = "AC,BF.;")
  Model.5 <- make.model("last")
  showModel(Model.5)

  Model.6 <- make.model("ACE,ADE,BC,F;")
  showModel(Model.6)

  "backward 1"
  backward(only = T, reversed = F, sorted = T, short = F,
           p.accepted = F, p.rejected = F, decomposable.mode = T,
           recursive = T, coherent = T, headlong = T, 
           follow = T, least.significant = T, separators = F, edges = T,
           model = F)
  Model.backward.from.saturated <- make.model("last")
  showModel(Model.backward.from.saturated)

  "backward 2"
  backward(only = T, reversed = F, sorted = T, short = F,
           p.accepted = F, p.rejected = F, decomposable.mode = T,
           recursive = T, coherent = T, headlong = T, 
           follow = T, least.significant = T, separators = F, edges = T,
           model = Model.3)
  Model.backward.from.3 <- make.model("last")
  showModel(Model.backward.from.3)

  showModel("all")

  showModel(MainEffects)
  showModel(Saturated)
  showModel(Model.3)
  showModel(Model.4)
  showModel(Model.5)
  showModel(Model.6)
  showModel(Model.backward.from.saturated)
  showModel(Model.backward.from.3)

  makeCurrent(Model.backward.from.3)
  showModel("all")
  Model.3
  makeBase(Model.3)
  showModel("all")

  showTable("expected", "*", MainEffects)
  showTable("expected", "*", Saturated)
  showTable("expected", "*", Model.3)
  showTable("expected", "*", Model.4)
  showTable("expected", "*", Model.5)
  showTable("expected", "*", Model.6)
  showTable("observed", "D", Saturated)
  showTable("expected", "*", Model.backward.from.saturated)
  showTable("observed", "E", Saturated)
  showTable("expected", "*", Model.backward.from.3)
  showTable("observed", "F", Saturated)

  rm( ReinisCoCoObject, Saturated, MainEffects, Model.3, Model.4, Model.5, Model.6)
  rm( Model.backward.from.3, Model.backward.from.saturated)

  quitCoCo()
