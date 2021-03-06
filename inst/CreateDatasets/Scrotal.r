
  # library(CoCo);
  # library(CoCoOldData);

  Scrotal94Object <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE,
  title = "Scrotal: David Madigan and Adrian E. Raftery (1994): Model Selection and ... Using Occam's Window. JASA. 428, 1535-1546.")

  set.data.file("Scrotal.dat")
  read.data();

  Scrotal94 <- returnTable()

  endCoCo();

  # quitCoCo()
