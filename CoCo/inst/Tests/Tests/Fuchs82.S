
  library(CoCo);
  library(CoCoOldData);

  Fuchs82 <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
                       title = "Fuchs, C., J. Amer. Statist Soc. 77, 1982")

  set.data.file("Fuchs82.dat")
  read.data();

  quitCoCo()
