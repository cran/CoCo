library(MASS)
data(survey);
names(survey)

table(survey["Sex"])
stem(unlist(survey["Wr.Hnd"]))
stem(unlist(survey["NW.Hnd"]))
table(survey["Fold"])
Fold <- as.factor(ifelse(survey["Fold"]=="R on L", "R on L", "Other"))
stem(unlist(survey["Pulse"]))
table(survey["Clap"])
Clap <- as.factor(ifelse(survey["Clap"]=="Right", "Right", "Other"))
table(survey["Exer"])
Exer <- as.factor(ifelse(survey["Exer"]=="Freq", "Freq", "None-Some"))
table(survey["Smoke"])
Smoke <- as.factor(ifelse(survey["Smoke"]=="Never", "No", "Yes"))
stem(unlist(survey["Height"]))
table(survey["M.I"])
stem(unlist(survey["Age"]))

Survey <- survey
Survey["Fold"]  <- Fold
Survey["Clap"]  <- Clap
Survey["Exer"]  <- Exer
Survey["Smoke"] <- Smoke
Survey

library(CoCoCg);
SurveyCoCo <- makeCoCoCg();
names(Survey) <- c("g", "W", "V", "h", "f", "U", "c", "e", "s", "X", "m", "Y")
result <- enterDataFrame(Survey, object = SurveyCoCo);
showOptions("specification", object = SurveyCoCo);

enterModel(".", object = SurveyCoCo);

optionsCoCo("bic" = FALSE); optionsCoCo("ic" = FALSE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);

g <- dynamic.Graph("last")

exportCoCo("survey2.xpt")

showModel("all")
makeCurrent()
showFormula()

endCoCo(object = SurveyCoCo);

q();
