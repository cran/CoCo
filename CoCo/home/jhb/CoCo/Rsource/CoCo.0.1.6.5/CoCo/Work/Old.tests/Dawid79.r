
  # library(CoCo);
  # library(CoCoOldData);

  David79 <- make.coco(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
  title = "Dawid, A. P. and Skene, A. M. (1979). Maximum likelihood estimation of observed errorrates using the EM ... Appl. Stat. 28, 20-28.")

  set.data.file("David79.dat")
  read.data();

  coco.end();

  # .quit()
