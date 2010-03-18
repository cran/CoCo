
  # library(CoCo);
  # library(CoCoOldData);

  UterineCervix411 <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
  title = "Carcinoma of the uterine cervix; A Handbook of Small Data Sets, edited by D.J. Hand et al., from Chapman and Hall, ISBN 0 412 39920 2.")

  set.data.file("UterineCervix411.dat")
  read.data();

  enterModel("*", object = UterineCervix411)

  endCoCo();

  # quitCoCo()


