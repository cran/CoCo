library(CoCoCg);
library(MASS)
data(crabs);

crabsCoCo <- makeCoCoCg();

names(crabs) <- c("a", "b", "i", "x", "y", "z", "u", "v")

result <- enterDataFrame(crabs[,-3], object = crabsCoCo);

enterModel("*;", object = crabsCoCo);

backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, 
         object = crabsCoCo);

exportCoCo("crabs.xpt")

endCoCo(object = crabsCoCo);
