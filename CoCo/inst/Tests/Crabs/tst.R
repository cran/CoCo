library(CoCoCg);
library(MASS)
data(crabs);

crabsCoCo <- makeCoCoCg();
result <- enterDataFrame(crabs[,-3], object = crabsCoCo);

enterModel("*;", object = crabsCoCo);

.set.switch(15971, "on")
.set.switch(15981, "on")
.set.switch(15988, "on")
.set.switch(17450, "on")
.set.switch(17470, "on")
.set.switch(17471, "on")
.set.switch(17478, "on")
.set.switch(25541, "on")
.set.switch(25611, "on")
.set.switch(25612, "on")
.set.switch(25618, "on")
.set.switch(25621, "on")
.set.switch(25622, "on")
.set.switch(25628, "on")
.set.switch(22111, "on")
.set.switch(22112, "on")
.set.switch(22113, "on")
.set.switch(22041, "on")
.set.switch(22101, "on")
.set.switch(22102, "on")
.set.switch(22103, "on")
.set.switch(1352, "on")
.set.switch(13520, "on")
.set.switch(13521, "on")

showTable("observed", "[:sp:sex]", object = crabsCoCo);

showTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]", 
		       matrix = TRUE, object = crabsCoCo);

showTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]", matrix = TRUE);

showTable("moment", "[:sp:sex:FL:RW:CL:CW:BD]", 
		       matrix = TRUE, object = crabsCoCo);

returnTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]");

returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]");

# backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, 
#          object = crabsCoCo);

m5 <- "[[:sp:sex]] / [[:sp:sex:FL][:sp:sex:RW][:sex:CL][:sp:sex:BD][:sp:sex:CW]] / [[:sex:FL:CL][:sp:sex:FL:CW][:sp:sex:FL:BD][:sp:sex:RW:CW][:sex:CL:CW][:sex:CL:BD][:sp:sex:CW:BD]]"
m4 <- "[[:sp:sex]] / [[:sp:sex:RW][:sp:sex:BD][:sp:sex:CW][:sp:sex:FL][:sex:CL]] / [[:sp:sex:CW:BD][:sex:CL:BD][:sex:CL:CW][:sp:sex:RW:BD][:sp:sex:RW:CW][:sp:sex:FL:BD][:sp:sex:FL:CW][:sex:FL:CL]]"
m3 <- "[[:sp:sex]] / [[:sp:sex:FL][:sp:sex:RW][:sex:CL][:sp:sex:BD][:sp:sex:CW]] / [[:sp:sex:FL:RW][:sex:FL:CL][:sp:sex:FL:CW][:sp:sex:FL:BD][:sp:sex:RW:CW][:sp:sex:RW:BD][:sex:CL:CW][:sex:CL:BD][:sp:sex:CW:BD]]"
m2 <- "[[:sp:sex]] / [[:sp:sex:BD][:sp:sex:CW][:sex:CL][:sp:sex:RW][:sp:sex:FL]] / [[:sp:sex:CW:BD][:sex:CL:BD][:sex:CL:CW][:sp:sex:RW:BD][:sp:sex:RW:CW][:sex:RW:CL][:sp:sex:FL:BD][:sp:sex:FL:CW][:sex:FL:CL][:sp:sex:FL:RW]]"
m1 <- "[[:sp:sex]] / [[:sp:sex:BD][:sp:sex:CW][:sp:sex:CL][:sp:sex:RW][:sp:sex:FL]] / [[:sp:sex:CW:BD][:sp:sex:CL:BD][:sp:sex:CL:CW][:sp:sex:RW:BD][:sp:sex:RW:CW][:sp:sex:RW:CL][:sp:sex:FL:BD][:sp:sex:FL:CW][:sp:sex:FL:CL][:sp:sex:FL:RW]]"

enterModel(m1, object = crabsCoCo);
enterModel(m2, object = crabsCoCo);
enterModel(m3, object = crabsCoCo);
enterModel(m4, object = crabsCoCo);
enterModel(m5, object = crabsCoCo);

makeCurrent("last", object = crabsCoCo)
returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]");

Rfirst <- returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", model = 1);
Rlast  <- returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", model = 6);
Sfirst <- returnTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", model = 1);
Slast  <- returnTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", model = 6);

Rfirst$h-Rlast$h
Rfirst$K-Rlast$K

# Sfirst$Mean-Slast$Mean

endCoCo(object = crabsCoCo);
