library(MASS)
data(immer)
library(CoCoCg);
CoCoObject <- makeCoCoCg();
enterDataFrame(immer, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));
fullGraph <- dynamic.Graph(fullModel, title = "Full");

mainModel <- makeModel(enterModel("[[:Var][:Loc]] / [[:Var:Y2][:Var:Y1][:Loc:Y2][:Loc:Y1]] / [[:Var:Y1:Y2][:Loc:Y1:Y2]]", object = CoCoObject));
mainGraph <- dynamic.Graph(mainModel, title = "Main");
makeBase();
backward(recursive = TRUE, object = CoCoObject);

lastModel <- makeModel("last", object = CoCoObject);
backwardGraph <- dynamic.Graph(lastModel, title = "Last");

quitCoCo()
