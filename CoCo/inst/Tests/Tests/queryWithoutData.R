

  n5 <- "[[:Sex:Smoking]] / [[:Sex:FEV][:Smoking:FEV][:Sex:Smoking:Height]] / [[:Sex:FEV^][:Smoking:FEV^][:Sex:Height:FEV][:Smoking:Height:FEV][:Height^]]"
 
  n1 <- "mdp,da,am,dg,gs,sa.;"
  n2 <- ":sex:hair,:fev."
 
  m1 <- "[[mdp][da][am][dg][gs][sa]]"
  m2 <- "[[:sex:hair][:fev]]"
  m3 <- "[:sex:hair][:fev]"
  m4 <- "[[ab]] / [[ay][by][abx]] / [[ay^][by^][axy][bxy][x^]]"
 
  m5 <- "[[:Sex:Smoking]] / [[:Sex:FEV][:Smoking:FEV][:Sex:Smoking:Height]] / [[:Sex:FEV][:Smoking:FEV][:Sex:Height:FEV][:Smoking:Height:FEV][:Height]]"


 library(CoCo)

 returnVertexOrder(model = "mdp,da,am,dg,gs,sa", data = "See.model")

 library(CoCoCg)

 returnVertexOrder(model = "mdp,da,am,dg,gs,sa", data = "See.model", continuous = c(1,2))
 returnVertexOrder(model = "mdp,da,am,dg,gs,sa", data = "Do.not.end", continuous = c(1,2))


 library(CoCoCg)

 x <- rep(1, 6)
 names(x) <- c("g", "a", "s", "d", "p", "m")

 returnVertexOrder(model = "mdp,da,am,dg,gs,sa", data = data.frame(t(x)))


 x <- rep(1, 6)
 names(x) <- c("g", "a", "s", "d", "p", "m")
 x <- data.frame(t(x))
 x <- x[FALSE,]

#  returnVertexOrder(model = "mdp,da,am,dg,gs,sa", data = x)


 library(CoCo)
