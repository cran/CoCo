
source("init.R")

source("run.1.CoCo.R")

library(base); # ;

data(esoph)
											                           	 		   
  # Accumulated case list:
  esoph.ca <- cbind(esoph[,c(4,1:3)], type = "Case")
  esoph.co <- cbind(esoph[,c(5,1:3)], type = "Control")
  dimnames(esoph.ca)[[2]][1] <- "n"
  dimnames(esoph.co)[[2]][1] <- "n"
  Esoph <- rbind(esoph.ca, esoph.co)

  for (i in 1:dim(Esoph)[2]) Esoph[,i] <- as.numeric(Esoph[,i])

  ESOPH <- matrix(unlist(c(Esoph)), ncol=5)
  ESOPH <- ESOPH[ESOPH[,1]!=0, ]
  dimnames(ESOPH)[[2]] <- dimnames(Esoph)[[2]]

  data(esoph);                 f("ESOPH",                "", ESOPH, Accumulated = TRUE); # R	   #   5   3   2   88      "esoph"                 

q()
