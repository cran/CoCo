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

library(CoCoCg);
SurveyCoCo <- makeCoCoCg();
names(Survey) <- c("g", "W", "V", "h", "f", "U", "c", "e", "s", "X", "m", "Y")
result <- enterDataFrame(Survey, object = SurveyCoCo);
showOptions("specification", object = SurveyCoCo);

enterModel(".", object = SurveyCoCo);

enterModel("g,h,f,c,e,s,m.", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = SurveyCoCo);
showModel("all", object = SurveyCoCo);
showTable("observed", "hc", object = SurveyCoCo);
showTable("observed", "gce", object = SurveyCoCo);
showTable("observed", "hce", object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
editModel("add.interactions", "W,V,U,X,Y", object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
forward(sorted = TRUE, object = SurveyCoCo);

editModel("add.edges", "hY, cY, gW", omit.test = FALSE, object = SurveyCoCo);
editModel("add.edges", "hY, cY, gW, XY, gV", omit.test = FALSE, object = SurveyCoCo);
editModel("add.edges", "hY, cY, gW, XY, gV, VX, eX", omit.test = FALSE, object = SurveyCoCo);
editModel("add.edges", "hY, cY, gW, XY, gV, VX", omit.test = FALSE, object = SurveyCoCo);

editModel("add.edges", "hY, cY, gW, XY, gV", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

editModel("drop.edges", "hc", omit.test = FALSE, object = SurveyCoCo);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
forward(sorted = TRUE, object = SurveyCoCo);

editModel("add.edges", "Vf", omit.test = FALSE, object = SurveyCoCo);
editModel("add.edges", "Vf, Wf", omit.test = FALSE, object = SurveyCoCo);
editModel("add.edges", "Vf, Wf, VU", omit.test = FALSE, object = SurveyCoCo);
editModel("add.edges", "Vf, Wf, VU, WU, Vh", omit.test = FALSE, object = SurveyCoCo);
# editModel("add.edges", "Vf, Wf, VU, WU, Vh, WV", omit.test = FALSE, object = SurveyCoCo);
# editModel("add.edges", "Vf, Wf, VU, WU, Vh, WV, eX", omit.test = FALSE, object = SurveyCoCo);
# editModel("add.edges", "Vf, Wf, VU, WU, Vh, WV", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

editModel("drop.edges", "WV, ex", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

editModel("drop.edges", "gc", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

editModel("drop.edges", "Vf", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

editModel("drop.edges", "VU", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

editModel("drop.edges", "Wf", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

forward(sorted = TRUE, object = SurveyCoCo);

editModel("add.edges", "Us", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

forward(sorted = TRUE, object = SurveyCoCo);

# %: Wh, WX, UX, cs

editModel("add.edges", "sX", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

forward(sorted = TRUE, object = SurveyCoCo);

# %: gY, gX, WV, VX, fX, eX, Xm

editModel("add.edges", "Wf", omit.test = FALSE, object = SurveyCoCo);
makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);
backward(sorted = TRUE, object = SurveyCoCo);

forward(sorted = TRUE, object = SurveyCoCo);

editModel("add.edges", "gh", omit.test = FALSE, object = SurveyCoCo);

lastModel <- makeModel("last")
dynamic.Graph("last")

exportCoCo("survey1.xpt", object = SurveyCoCo);

endCoCo(object = SurveyCoCo);

library(CoCoCg);
SurveyCoCo <- makeCoCoCg();
importCoCo("survey1.xpt", object = SurveyCoCo);

lastModel <- makeModel("last", object = SurveyCoCo);
dynamic.Graph(lastModel);

makeCurrent("last", object = SurveyCoCo);
makeBase("last", object = SurveyCoCo);

backward(sorted = TRUE, object = SurveyCoCo);
forward(sorted = TRUE, object = SurveyCoCo);

endCoCo(object = SurveyCoCo);
