library(MASS)
data(survey);
names(survey)

table(survey["Sex"])
Sex <- ifelse(survey["Sex"]=="Male", 1, 2)
stem(unlist(survey["Wr.Hnd"]))
stem(unlist(survey["NW.Hnd"]))
table(survey["W.Hnd"])
Whnd <- ifelse(survey["W.Hnd"]=="Left", 1, 2)
table(survey["Fold"])
Fold <- ifelse(survey["Fold"]== "L on R", 1, ifelse(survey["Fold"]== "Neither", 2, 3))
stem(unlist(survey["Pulse"]))
table(survey["Clap"])
Clap <- ifelse(survey["Clap"]== "Left", 1, ifelse(survey["Fold"]== "Neither", 2, 3))
table(survey["Exer"])
Exer <- ifelse(survey["Exer"]=="None", 1, ifelse(survey["Exer"]=="Some", 2, 3))
table(survey["Smoke"])
Smoke <- ifelse(survey["Smoke"]=="Never", 1, 
                ifelse(survey["Smoke"]=="Occas", 2, 
                       ifelse(survey["Smoke"]=="Regul", 3, 4)))
stem(unlist(survey["Height"]))
table(survey["M.I"])
Mi <- ifelse(survey["M.I"]=="Metric", 1, 2)
stem(unlist(survey["Age"]))

Survey <- survey
Survey["Sex"]  <- Sex
Survey["W.Hnd"] <- Whnd
Survey["Fold"]  <- Fold
Survey["Clap"]  <- Clap
Survey["Exer"]  <- Exer
Survey["Smoke"] <- Smoke
Survey["M.I"]  <- Mi

Exclude <- apply(Survey, 1, function(x) any(is.na(x)))

# . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  #

library(CoCoCg);
SurveyCoCo <- makeCoCoCg();
names(Survey) <- c("g", "W", "V", "h", "f", "U", "c", "e", "s", "X", "m", "Y")
result <- enterDataFrame(Survey[!Exclude,], object = SurveyCoCo);
showOptions("specification", object = SurveyCoCo);

# . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  #

enterModel(".", object = SurveyCoCo);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

lastModel <- makeModel("last", object = SurveyCoCo);
Graph <- dynamic.Graph(lastModel);

# . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  #

enterModel("*", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);

lastModel <- makeModel("last", object = SurveyCoCo);
GraphBackward <- dynamic.Graph(lastModel);

exportCoCo("survey-continuous-exclude.xpt", object = SurveyCoCo);

endCoCo(object = SurveyCoCo);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  #

library(CoCoCg);

SurveyCoCo <- makeCoCoCg();
importCoCo("survey-continuous.xpt", object = SurveyCoCo);

lastModel <- makeModel("last", object = SurveyCoCo);
Graph <- dynamic.Graph(lastModel);

endCoCo(object = SurveyCoCo);
