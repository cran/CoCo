
  library(CoCo)

  printTable("expected", model = A.2)
  printTable("expected", model = B.2)
  printTable("expected", model = A.1)
  printTable("expected", model = B.1)

  print(return.table("expected", "*", model = A.2))
  print(return.table("expected", "*", model = B.2))

  print(return.table("expected", "*", model = B.2) -
        return.table("expected", "*", model = A.2))

  printTable("expected", model = A.2)
  printTable("expected", model = B.2)
  printTable("expected", model = A.1)
  printTable("expected", model = B.1)

  print(return.table("expected", "*", model = A.2))
  print(return.table("expected", "*", model = B.2))

  print(return.table("expected", "*", model = B.2) -
        return.table("expected", "*", model = A.2))


  str(A.1)
  str(A.2)

  str(B.1)
  str(B.2)

  .quit()

