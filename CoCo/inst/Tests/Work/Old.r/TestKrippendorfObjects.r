
### TestKrippendorf.r

  library(CoCo);

  without <- make.coco(n = 2048, p = 1024, q = 128,
		       title = "Without structural zero", 
                       location = c(50, 50), manager = T, silent = F)
  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)
  enter.names("smv", rep(2, 3))
  n <- c(11, 2209, 0, 111, 48, 239, 72, 2074)
  enter.table(n)
  A.1 <- make.model("*")
  A.0 <- make.model(".")
  A.a <- make.model("ms,sv")
  A.b <- make.model("sm,mv")
  A.2 <- make.model("mv,vs")
  test()

  with <- make.coco(n = 2048, p = 1024, q = 128,
		       title = "With structural zero", 
                       location = c(50, 50), manager = T, silent = F)
  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)
  enter.names("smv", rep(2, 3))
  n <- c(11, 2209, -1, 111, 48, 239, 72, 2074)
  enter.table(n)
  B.1 <- make.model("*")
  B.0 <- make.model(".")
  B.a <- make.model("ms,sv")
  B.b <- make.model("sm,mv")
  B.2 <- make.model("mv,vs")
  test()

  print(return.table("expected", "*", model = A.2))
  print(return.table("expected", "*", model = B.2))

  print(return.table("expected", "*", model = B.2) -
        return.table("expected", "*", model = A.2))

  .quit(save = "yes")
