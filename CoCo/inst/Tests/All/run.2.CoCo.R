library(CoCo);
library(CoCoCg);

system("mkdir -p Dia")
system("mkdir -p Rpt")
system("mkdir -p Dmp")
system("mkdir -p Log")
system("mkdir -p Xpt")

run.discrete.forward <- function(data, name, 
				 size = c(dim(data)[2], h(data), dim(data)[1]),
				 do.eh = size[1] < 7, print.tables = TRUE,
				 accumulated = FALSE, to.factor = NULL,
				 missing.values = c("."), delta = 0) {
print(paste(" - - - - - - - - - - - - - - - ", name, " - - - - - - - - - - - - - - - "))
}