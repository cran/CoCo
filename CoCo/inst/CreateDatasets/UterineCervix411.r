
  # library(CoCo);
  # library(CoCoOldData);

  UterineCervix411Object <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
  title = "Carcinoma of the uterine cervix; A Handbook of Small Data Sets, edited by D.J. Hand et al., from Chapman and Hall, ISBN 0 412 39920 2.")

  set.data.file("UterineCervix411.dat")
  read.data();

  UterineCervix411.x <- returnTable()

  VarDesc <- returnVariableDescription();
  SparseTable <- returnTable("sparse.table", "*");
  UterineCervix411 <- matrix(SparseTable[[2]], 
                    ncol = length(VarDesc$names) + 1, byrow = T);
  dimnames(UterineCervix411) <- list(NULL, c("count", VarDesc$names));
  rm(VarDesc, SparseTable);

  enterModel("*", object = UterineCervix411Object)

  endCoCo();

  # quitCoCo()


