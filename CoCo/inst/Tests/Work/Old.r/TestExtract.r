
  library(CoCo);
  library(CoCoOldData);

  Fuchs82 <- make.coco()

  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)

  set.data.file("Fuchs82.dat")
  read.data();

  New <- extract.data()

  # .quit()
