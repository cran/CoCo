
  library(CoCo);
  library(CoCoOldData);

  Byssinosis38 <- make.coco(n = 2048, p = 1024, q = 128, uniq.title = TRUE, title = "Byssinosis38")

  set.data.file("Byssinosis38.dat")
  read.data();

  .quit()
