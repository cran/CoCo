
### TestIs

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  read.model(".;")
  read.model("*;")
  base()
  read.model("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  read.model("ACE,ADE,BC,F.;")
  read.model(".;")

  printModel("all")

  make.base(2)
  make.current(6)

  printModel("all")

  "is.graphical (\"base\"     ) " ;      is.model( "graphical", "base"     ) ;
  "is.graphical (\"current\"  ) " ;      is.model( "graphical", "current"  ) ;
  "is.graphical (\"last\"     ) " ;      is.model( "graphical", "last"     ) ;
  "is.graphical (4            ) " ;      is.model( "graphical", 4          ) ;
# "is.graphical (\"previous\" ) " ;      is.model( "graphical", "previous" ) ;
  "is.graphical (5            ) " ;      is.model( "graphical", 5          ) ;
# "is.graphical (\"next\"     ) " ;      is.model( "graphical", "next"     ) ;

  printModel("all")

  "is.decomposable (\"base\"     ) " ;   is.model ( "decomposable", "base"     ) ;
  "is.decomposable (\"current\"  ) " ;   is.model ( "decomposable", "current"  ) ;
  "is.decomposable (\"last\"     ) " ;   is.model ( "decomposable", "last"     ) ;
  "is.decomposable (4            ) " ;   is.model ( "decomposable", 4          ) ;
# "is.decomposable (\"previous\" ) " ;   is.model ( "decomposable", "previous" ) ;
  "is.decomposable (5            ) " ;   is.model ( "decomposable", 5          ) ;
# "is.decomposable (\"next\"     ) " ;   is.model ( "decomposable", "next"     ) ;

  make.base(4)
  make.current(6)

  printModel("all")

  "is.submodel.of () " ;   is.submodel.of () ;

  "is.submodel.of (\"base\"     ) " ;   is.submodel.of ("base"     ) ;
  "is.submodel.of (\"current\"  ) " ;   is.submodel.of ("current"  ) ;
  "is.submodel.of (\"last\"     ) " ;   is.submodel.of ("last"     ) ;
  "is.submodel.of (4            ) " ;   is.submodel.of (4          ) ;
# "is.submodel.of (\"previous\" ) " ;   is.submodel.of ("previous" ) ;
  "is.submodel.of (5            ) " ;   is.submodel.of (5          ) ;
# "is.submodel.of (\"next\"     ) " ;   is.submodel.of ("next"     ) ;
  "is.submodel.of (\"*\"        ) " ;   is.submodel.of ("*"        ) ;
  "is.submodel.of (\".\"        ) " ;   is.submodel.of ("."        ) ;

  printModel("all")

  "is.submodel.of (\"current\", \"base\"     ) " ;   is.submodel.of ("current", "base"     ) ;
  "is.submodel.of (\"current\", \"current\"  ) " ;   is.submodel.of ("current", "current"  ) ;
  "is.submodel.of (\"current\", \"last\"     ) " ;   is.submodel.of ("current", "last"     ) ;
  "is.submodel.of (\"current\", 4            ) " ;   is.submodel.of ("current", 4          ) ;
# "is.submodel.of (\"current\", \"previous\" ) " ;   is.submodel.of ("current", "previous" ) ;
  "is.submodel.of (\"current\", 5            ) " ;   is.submodel.of ("current", 5          ) ;
# "is.submodel.of (\"current\", \"next\"     ) " ;   is.submodel.of ("current", "next"     ) ;
  "is.submodel.of (\"current\", \"*\"        ) " ;   is.submodel.of ("current", "*"        ) ;
  "is.submodel.of (\"current\", \".\"        ) " ;   is.submodel.of ("current", "."        ) ;
  "is.submodel.of (\"*\", \"current\"        ) " ;   is.submodel.of ("*", "current"        ) ;
  "is.submodel.of (\".\", \"current\"        ) " ;   is.submodel.of (".", "current"        ) ;

  printModel("all")

  "is.in.one.clique (\"AB;\"               ) " ;   is.set (query = "in.one.clique", "AB;"             ) ;
  "is.in.one.clique (\"AB;\", \"base\"     ) " ;   is.set (query = "in.one.clique", "AB;", "base"     ) ;
  "is.in.one.clique (\"AB;\", \"current\"  ) " ;   is.set (query = "in.one.clique", "AB;", "current"  ) ;
  "is.in.one.clique (\"AB;\", \"last\"     ) " ;   is.set (query = "in.one.clique", "AB;", "last"     ) ;
  "is.in.one.clique (\"AB;\", 4            ) " ;   is.set (query = "in.one.clique", "AB;", 4          ) ;
# "is.in.one.clique (\"AB;\", \"previous\" ) " ;   is.set (query = "in.one.clique", "AB;", "previous" ) ;
  "is.in.one.clique (\"AB;\", 5            ) " ;   is.set (query = "in.one.clique", "AB;", 5          ) ;
# "is.in.one.clique (\"AB;\", \"next\"     ) " ;   is.set (query = "in.one.clique", "AB;", "next"     ) ;
  "is.in.one.clique (\"AB;\", \"*\"        ) " ;   is.set (query = "in.one.clique", "AB;", "*"        ) ;
  "is.in.one.clique (\"AB;\", \".\"        ) " ;   is.set (query = "in.one.clique", "AB;", "."        ) ;

  printModel("all")

  "is.submodel.of (4            ) " ;              is.submodel.of (4          ) ;

  printModel("all")

  "is.in.one.clique (\"AB\"                ) " ;   is.set (query = "in.one.clique", "AB"              ) ;

  printModel("all")

  .quit()
