library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE);

# Show the model list:
showModel("all", object = CoCoObject);

isSubmodel(); # TRUE
isSubmodel(1, 4); # FALSE
isSubmodel(4, 1); # TRUE

homogeneModel <- makeModel(enterModel("[[ab]] / [[aby][abx]] / [[x^][abxy][y^]]",
	                   object = CoCoObject));

# Numbers:

n6 <- returnModelNumber(homogeneModel);

isSubmodel(); # TRUE
isSubmodel(1, n6); # FALSE%
isSubmodel(n6, 1); # TRUE%

# Text string with tags:

isSubmodel("base", "current"); # FALSE%
isSubmodel("current", "base"); # TRUE%
isSubmodel("last", "base"); # TRUE%

# Objects:

isSubmodel(fullModel, homogeneModel); # FALSE%
isSubmodel(homogeneModel, fullModel); # TRUE%

# Non existing models:

isSubmodel("current", 7); # NULL
isSubmodel(7, "base"); # NULL

makeCurrent(7);
isSubmodel(1, 4); # FALSE
isSubmodel(4, 1); # TRUE

showModel("all", object = CoCoObject);

makeCurrent(4);
isSubmodel("next", "base"); # Using current!
isSubmodel("previous", "base"); #  Using current!

# Text string with generating class:

isSubmodel("aby"); # TRUE
isSubmodel("ab", "a,b"); # FALSE

endCoCo();
