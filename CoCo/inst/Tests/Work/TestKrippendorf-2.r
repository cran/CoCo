
### TestKrippendorf.r

  library(CoCo);

  without <- make.coco(n = 2048, p = 1024, q = 128,
		       title = "Without structural zero", 
                       location = c(50, 50), manager = T, silent = F)
  set.table.formats(c(6, 2, 1, 1))
  enter.names("smv", rep(2, 3))
  n <- c(11, 2209, 0, 111, 48, 239, 72, 2074)
  enter.table(n)
  A.1 <- make.model("*")
  print(return.vector("expected", "*"), digits = 4)
  A.2 <- make.model("mv,vs")
  print(return.vector("expected", "*"), digits = 4)
  test()

  with <- make.coco(n = 2048, p = 1024, q = 128,
		       title = "With structural zero", 
                       location = c(50, 50), manager = T, silent = F)
  set.table.formats(c(6, 2, 1, 1))
  enter.names("smv", rep(2, 3))
  n <- c(11, 2209, -1, 111, 48, 239, 72, 2074)
  enter.table(n)
  B.1 <- make.model("*")
  print(return.vector("expected", "*"), digits = 4)
  B.2 <- make.model("mv,vs")
  print(return.vector("expected", "*"), digits = 4) 
  test()

  print(return.vector("expected", "*", model = A.2), digits = 4)
  print(return.vector("expected", "*", model = B.2), digits = 4)

  print(return.vector("expected", "*", model = B.2) -
        return.vector("expected", "*", model = A.2), digits = 4)

#  .quit(save = "yes")
