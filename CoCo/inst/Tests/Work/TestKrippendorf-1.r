
### TestKrippendorf.r

  library(CoCo);

  without <- make.coco(n = 2048, p = 1024, q = 128,
		       title = "Without structural zero", 
                       location = c(50, 50), manager = T, silent = F)
  set.table.formats(c(6, 2, 1, 1))
  enter.names("smv", rep(2, 3))
  n <- c(11, 2209, 0, 111, 48, 239, 72, 2074)
  enter.table(n)
  read.model("*")
  read.model("mv,vs")
  test()

  with <- make.coco(n = 2048, p = 1024, q = 128,
		       title = "With structural zero", 
                       location = c(50, 50), manager = T, silent = F)
  set.table.formats(c(6, 2, 1, 1))
  enter.names("smv", rep(2, 3))
  n <- c(11, 2209, -1, 111, 48, 239, 72, 2074)
  enter.table(n)
  read.model("*")
  read.model("mv,vs")
  test()

  print(return.vector("expected", "*", coco.id = without))
  print(return.vector("expected", "*", coco.id = with))

  print(return.vector("expected", "*", coco.id = with) -
        return.vector("expected", "*", coco.id = without))

  .quit()
