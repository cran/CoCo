
  library(CoCo);
  data(Reinis);
  CoCoObject <- makeCoCo();
  enterTable(Reinis, object = CoCoObject);

  # Enter the saturated model:	
  enterModel("*", object = CoCoObject);

  # Generate some models, here by "backward":
  backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE);

  # Show the model list:
  showModel("all", object = CoCoObject);

  # Test the last model against the saturated model (the first model):
  makeCurrent("last", object = CoCoObject);
  showTest(object = CoCoObject);

  # "last" is default for makeCurrent:
  showModel("all", object = CoCoObject);
  makeCurrent(object = CoCoObject);
  showModel("all", object = CoCoObject);

  # "current" is default for makeBase:
  makeBase(object = CoCoObject);
  showModel("all", object = CoCoObject);
  makeBase("current", object = CoCoObject);
  showModel("all", object = CoCoObject);

  # Generate some more models, here by "editModel":
	
  enterModel("ADEF,ABDG.;", object = CoCoObject);
  makeBase(object = CoCoObject);

  editModel(action = "drop.interactions", modification = "ABDG.;", object = CoCoObject);
  makeCurrent("last", object = CoCoObject);
  showTest(object = CoCoObject);

  editModel(action = "drop.edges", modification = "AD,BG.;", object = CoCoObject);
  makeCurrent("last", object = CoCoObject);
  showTest(object = CoCoObject);

  enterModel("ADF,AEF,BD,G.;", object = CoCoObject);
  makeCurrent("last", object = CoCoObject);
  showTest(object = CoCoObject);

  enterModel(".;", object = CoCoObject);

  showModel("all", object = CoCoObject);

  makeBase(2, object = CoCoObject);
  makeCurrent(6, object = CoCoObject);

  showModel("all", object = CoCoObject);
	
  # Show the first 10 models:
  makeCurrent(10, object = CoCoObject)
  while(makeCurrent("previous", object = CoCoObject)) showModel(object = CoCoObject)

  # Show the first 10 models:
  makeCurrent(10, object = CoCoObject)
  while(returnModelNumber("current", object = CoCoObject) < 15) {
	 makeCurrent("next", object = CoCoObject); 
	 showModel(object = CoCoObject); }

  endCoCo(object = CoCoObject);

  fullModel <- makeModel(enterModel("*", object = CoCoObject));

  showModel("all")
  isSubmodel(9, 1)
