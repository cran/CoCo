library(CoCoCg);
library(MASS)
data(crabs);

crabsCoCo <- makeCoCoCg();
result <- enterDataFrame(crabs[,-3], object = crabsCoCo);

enterModel("*;", object = crabsCoCo);

showTable("observed", "[:sp:sex]", object = crabsCoCo);

showTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]", 
		       matrix = TRUE, object = crabsCoCo);

showTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]", matrix = TRUE);

showTable("moment", "[:sp:sex:FL:RW:CL:CW:BD]", 
		       matrix = TRUE, object = crabsCoCo);

returnTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]", split = TRUE);

returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", split = TRUE);

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
returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", split = TRUE);

showTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", model = 1);
Rfirst <- returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", split = TRUE, model = 1);
Rfirst
showTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", model = 6);
Rlast  <- returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", split = TRUE, model = 6);
Rlast
showTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", model = 1);
Sfirst <- returnTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", split = TRUE, model = 1);
Sfirst
showTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", model = 6);
Slast  <- returnTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", split = TRUE, model = 6);
Slast

Rfirst$h-Rlast$h
Rfirst$K-Rlast$K

# Sfirst$Mean-Slast$Mean

endCoCo(object = crabsCoCo);
