library(CoCoCg);
library(MASS)
data(survey);

surveyCoCo <- makeCoCoCg();

names(survey) <- c("g", "W", "V", "h", "f", "U", "c", "e", "s", "X", "m", "Y")

result <- enterDataFrame(survey, object = surveyCoCo);

enterModel("g,h,f,c,e,s,m.", object = surveyCoCo);

optionsCoCo("bic" = TRUE); optionsCoCo("ic" = TRUE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = surveyCoCo);

optionsCoCo("bic" = FALSE); optionsCoCo("ic" = FALSE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = surveyCoCo);

enterModel(".;", object = surveyCoCo);

optionsCoCo("bic" = TRUE); optionsCoCo("ic" = TRUE);
forward(sorted = TRUE, object = surveyCoCo);

optionsCoCo("bic" = FALSE); optionsCoCo("ic" = FALSE);
forward(sorted = TRUE, object = surveyCoCo);

enterModel("hc,f,s,m,gW,V,U,WX,eX,XY.", object = surveyCoCo);

exportCoCo("survey.xpt")

showModel("all")
makeCurrent()
showFormula()

endCoCo(object = surveyCoCo);

exportCoCo("survey.xpt");
