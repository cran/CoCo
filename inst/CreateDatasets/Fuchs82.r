
  # library(CoCo);
  # library(CoCoOldData);

  Fuchs82Object <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
  title = "Fuchs, C., Maximum likelihood estimation and model selection in contingency tables with missing data, JASS. 77(378), 1982")

  set.data.file("Fuchs82.dat")
  read.data();

  Fuchs82 <- returnTable()

  endCoCo();

  # quitCoCo()
