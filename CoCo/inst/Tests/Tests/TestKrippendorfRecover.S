
  library(CoCo)

  showTable("expected", model = A.2)
  showTable("expected", model = B.2)
  showTable("expected", model = A.1)
  showTable("expected", model = B.1)

  print(returnTable("expected", "*", model = A.2))
  print(returnTable("expected", "*", model = B.2))

  print(returnTable("expected", "*", model = B.2) -
        returnTable("expected", "*", model = A.2))

  showTable("expected", model = A.2)
  showTable("expected", model = B.2)
  showTable("expected", model = A.1)
  showTable("expected", model = B.1)

  print(returnTable("expected", "*", model = A.2))
  print(returnTable("expected", "*", model = B.2))

  print(returnTable("expected", "*", model = B.2) -
        returnTable("expected", "*", model = A.2))


  str(A.1)
  str(A.2)

  str(B.1)
  str(B.2)

  quitCoCo()

