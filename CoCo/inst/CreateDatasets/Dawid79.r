
  # library(CoCo);
  # library(CoCoOldData);

  require(CoCo);
  require(CoCoOldData);

  Dawid79Object <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
  title = "Dawid, A. P. and Skene, A. M. (1979). Maximum likelihood estimation of observed errorrates using the EM ... Appl. Stat. 28, 20-28.")

  set.data.file("Dawid79.dat")
  read.data();

  VarDesc <- returnVariableDescription();
  SparseTable <- returnTable("sparse.table", "*");
  Dawid79 <- matrix(SparseTable[[2]], 
                    ncol = length(VarDesc$names) + 1, byrow = T);
  dimnames(Dawid79) <- list(NULL, c("count", VarDesc$names));
  rm(VarDesc, SparseTable);

  # showTable("observed", "*", output.form = "sparse.table");

  # showTable("observed", "abc", output.form = "case.list");
  # showTable("observed", "*", output.form = "list.all.values");
  # showTable("observed", "abc", output.form = "list.all.values");

  endCoCo();


  # quitCoCo()
