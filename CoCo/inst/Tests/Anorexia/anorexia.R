
library(MASS)
data(anorexia)
library(CoCoCg);
CoCoObject <- makeCoCoCg();
enterDataFrame(anorexia, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# fullGraph <- dynamic.Graph(fullModel, title = "Full");

backward(recursive = TRUE, object = CoCoObject);

# lastModel <- makeModel("last", object = CoCoObject);
# backwardGraph <- dynamic.Graph(lastModel, title = "Last");

showModel("all", object = CoCoObject);
makeCurrent(object = CoCoObject);

# These two calls does not produce the same deviance,
# since the second tests the inhomogene model:

showTest(object = CoCoObject);
showDeviance(object = CoCoObject);

# The default models for showTest and showDeviance are "current" and "base",
# but the default models for returnTest and returnDeviance are FALSE,
# which both will refere to "base" when no models are set:

returnTest(object = CoCoObject); # model.1 is by default "base"!!!
returnTest("base", "base", object = CoCoObject);
returnTest("current", object = CoCoObject);
returnTest("current", "base", object = CoCoObject);
returnTest(model.2 = 1, object = CoCoObject);
returnTest("base", "current", object = CoCoObject);

returnDeviance(object = CoCoObject); # model.1 is by default "base"!!!
returnDeviance("base", "base", object = CoCoObject);
returnDeviance("current", object = CoCoObject);
returnDeviance("current", "base", object = CoCoObject);
returnDeviance(model.2 = 1, object = CoCoObject);
returnDeviance("base", "current", object = CoCoObject);

showTest(3, 1, object = CoCoObject);
showDeviance(3, 1, object = CoCoObject);
returnTest(3, 1, object = CoCoObject);
returnDeviance(3, 1, object = CoCoObject);

showTest(1, 1, object = CoCoObject);
showDeviance(1, 1, object = CoCoObject);
returnTest(1, 1, object = CoCoObject);
returnDeviance(1, 1, object = CoCoObject);

# inhoModel <- makeModel(enterModel("[[:Treat]] / [[:Prewt][:Treat:Postwt]] / [[:Treat:Prewt:Postwt]]", object = CoCoObject));

# inhoModel <- makeModel(enterModel("[[:Treat]] / [[:Prewt][:Treat:Postwt]] / [:Prewt^][:Treat:Prewt:Postwt][:Treat:Postwt^]]", object = CoCoObject));

# inhoModel <- makeModel(enterModel("[[:Treat]] / [[:Prewt][:Treat:Postwt]] / [:Prewt^][:Prewt:Postwt][:Treat:Postwt^]]", object = CoCoObject));

inhoModel <- makeModel(enterModel("[[:Prewt:Postwt][:Treat:Postwt]]", object = CoCoObject));

showModel("all", object = CoCoObject);
makeCurrent(object = CoCoObject);

showTest(object = CoCoObject);
showDeviance(object = CoCoObject);
returnTest("current", "base", object = CoCoObject);
returnDeviance("current", "base", object = CoCoObject);

quitCoCo()
