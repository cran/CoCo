
  library(CoCo);
  library(CoCoOldData);

  Reinis <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE,
  title = "Reinis et al (1981): Prognostic significance of the risk profile in the prevention of coronary heart disease. Bratis. lek. Listy. 76: 137-150", 
  location = c(50, 50), manager = T, silent = F)

  enterNames("ABCDEF", rep(2, 6))

  n <- c(44, 40, 112, 67, 129,145,  12, 23,
         35, 12,  80, 33, 109, 67,   7,  9,
         23, 32,  70, 66,  50, 80,   7, 13,
         24, 25,  73, 57,  51, 63,   7, 16,
          5,  7,  21,  9,   9, 17,   1,  4,
          4,  3,  11,  8,  14, 17,   5,  2,
          7,  3,  14, 14,   9, 16,   2,  3,
          4,  0,  13, 11,   5, 14,   4,  4)

  enterTable(n)

  rm(n)

  endCoCo();

  # quitCoCo()
