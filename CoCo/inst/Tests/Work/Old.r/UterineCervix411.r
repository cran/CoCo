
  library(CoCo);
  library(CoCoOldData);

  UterineCervix <- make.coco(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
                       title = "UterineCervix - 411")

  set.data.file("UterineCervix411.dat")
  read.data();

  read.model("*", coco.id=UterineCervix)

  .quit()
