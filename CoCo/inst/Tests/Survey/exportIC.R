library(CoCoCg);
library(MASS)
data(survey);

surveyCoCo <- makeCoCoCg();

names(survey) <- c("g", "W", "V", "h", "f", "U", "c", "e", "s", "X", "m", "Y")

result <- enterDataFrame(survey, object = surveyCoCo);
showOptions("specification", object = surveyCoCo);

enterModel(".;", object = surveyCoCo);

optionsCoCo("bic" = TRUE); optionsCoCo("ic" = TRUE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = surveyCoCo);

g <- dynamic.Graph("last")

exportCoCo("surveyIC.xpt")

showModel("all")
makeCurrent()
showFormula()

endCoCo(object = surveyCoCo);

q();
