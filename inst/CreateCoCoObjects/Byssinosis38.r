
  # library(CoCo);
  # library(CoCoOldData);

  Byssinosis38 <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE,
  title = "Byssinosis: Higgens, J.E. and Koch, G.G. (1977), Variable selection and generalized chi-square ..., Int. Stat. Revies, 45, 51-62")

  set.data.file("Byssinosis38.dat")
  read.data();

  endCoCo();

  # quitCoCo()
