
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE);

# Show the model list:
showModel("all", object = CoCoObject);

returnDeviance("last", 1, object = CoCoObject);

homogeneModel <- makeModel(enterModel("[[ab]] / [[aby][abx]] / [[x^][abxy][y^]]",
	                   object = CoCoObject));

returnDeviance("last", 1, object = CoCoObject);

quitCoCo()
