
  library(CoCo);
  library(CoCoOldData);

  Hochberg77Object <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE,
  title = "Hochberg, Y (1977): On the use of double sampling schemes in analyzing categorical data with misclassification errors. JASA. 72, 914-921.")

  set.data.file("Hochberg77.dat")
  read.data();

  Hochberg77 <- returnTable()

  endCoCo();

  # quitCoCo()
