library(MASS)
data(survey);
names(survey)

f <- function(x) {
i <- sort(unlist(x))[c(1, 59, 119, 178, 237)]
ifelse(x < i[2], 1, ifelse(x < i[3], 2, ifelse(x < i[4], 3, 4)))
}

table(survey["Sex"])
Sex <- ifelse(survey["Sex"]=="Male", 1, 2)
stem(unlist(survey["Wr.Hnd"]))
Wr.Hnd <- f(survey["Wr.Hnd"])
stem(unlist(survey["NW.Hnd"]))
NW.Hnd <- f(survey["NW.Hnd"])
table(survey["W.Hnd"])
W.Hnd <- ifelse(survey["W.Hnd"]=="Left", 1, 2)
table(survey["Fold"])
Fold <- ifelse(survey["Fold"]== "L on R", 1, ifelse(survey["Fold"]== "Neither", 2, 3))
stem(unlist(survey["Pulse"]))
Pulse <- f(survey["Pulse"])
table(survey["Clap"])
Clap <- ifelse(survey["Clap"]== "Left", 1, ifelse(survey["Fold"]== "Neither", 2, 3))
table(survey["Exer"])
Exer <- ifelse(survey["Exer"]=="None", 1, ifelse(survey["Exer"]=="Some", 2, 3))
table(survey["Smoke"])
Smoke <- ifelse(survey["Smoke"]=="Never", 1, 
                ifelse(survey["Smoke"]=="Occas", 2, 
                       ifelse(survey["Smoke"]=="Regul", 3, 4)))
stem(unlist(survey["Height"]))
Height <- f(survey["Height"])
table(survey["M.I"])
Mi <- ifelse(survey["M.I"]=="Metric", 1, 2)
stem(unlist(survey["Age"]))
Age <- f(survey["Age"])

Survey <- survey
Survey["Sex"]    <- Sex
Survey["Wr.Hnd"] <- Wr.Hnd
Survey["NW.Hnd"] <- NW.Hnd
Survey["W.Hnd"]  <- W.Hnd
Survey["Fold"]   <- Fold
Survey["Pulse"]  <- Pulse
Survey["Clap"]   <- Clap
Survey["Exer"]   <- Exer
Survey["Smoke"]  <- Smoke
Survey["Height"] <- Height
Survey["M.I"]    <- Mi
Survey["Age"]    <- Age

Survey

# . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  #

library(CoCoCg);
# SurveyCoCo <- makeCoCo();
SurveyCoCo <- makeCoCoCg();
# names(Survey) <- c("g", "W", "V", "h", "f", "U", "c", "e", "s", "X", "m", "Y")
result <- enterDataFrame(Survey, to.factor = 1:12, object = SurveyCoCo);
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
ForwardGraph <- dynamic.Graph(lastModel);

# . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  . . . . .  #

# enterModel("*", object = SurveyCoCo);
# makeBase("last", object = SurveyCoCo);
# backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
# showModel("all", object = SurveyCoCo);

# makeCurrent("last", object = SurveyCoCo);
# makeBase("last", object = SurveyCoCo);
# forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
# showModel("all", object = SurveyCoCo);

# makeCurrent("last", object = SurveyCoCo);
# makeBase("last", object = SurveyCoCo);
# backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
# showModel("all", object = SurveyCoCo);

# makeCurrent("last", object = SurveyCoCo);
# makeBase("last", object = SurveyCoCo);
# forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
# showModel("all", object = SurveyCoCo);

# makeCurrent("last", object = SurveyCoCo);
# makeBase("last", object = SurveyCoCo);
# backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, object = SurveyCoCo);
# showModel("all", object = SurveyCoCo);

# lastModel <- makeModel("last", object = SurveyCoCo);
# GraphBackward <- dynamic.Graph(lastModel);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  #

exportCoCo("survey-discrete.xpt", object = SurveyCoCo);

endCoCo(object = SurveyCoCo);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  #

library(CoCoCg);

SurveyCoCo <- makeCoCoCg();
importCoCo("survey-discrete.xpt", object = SurveyCoCo);

lastModel <- makeModel("last", object = SurveyCoCo);
Graph <- dynamic.Graph(lastModel);

endCoCo(object = SurveyCoCo);
