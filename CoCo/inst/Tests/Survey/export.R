library(CoCoCg);
library(MASS)
data(survey);

surveyCoCo <- makeCoCoCg();

# names(survey) <- c("g", "W", "V", "h", "f", "U", "c", "e", "s", "X", "m", "Y")

result <- enterDataFrame(survey, object = surveyCoCo);
showOptions("specification", object = surveyCoCo);

enterModel(".;", object = surveyCoCo);

optionsCoCo("bic" = FALSE); optionsCoCo("ic" = FALSE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = surveyCoCo);

g <- dynamic.Graph("last")

exportCoCo("survey.xpt")

showModel("all")
makeCurrent()
showFormula()

endCoCo(object = surveyCoCo);

q();
