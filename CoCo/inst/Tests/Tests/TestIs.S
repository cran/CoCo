
### TestIs

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  enterModel(".;")
  enterModel("*;")
  makeBase("current")
  enterModel("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  enterModel("ACE,ADE,BC,F.;")
  enterModel(".;")

  showModel("all")

  makeBase(2)
  makeCurrent(6)

  showModel("all")

  "is.graphical (\"base\"     ) " ;      propertyModel( "graphical", "base"     ) ;
  "is.graphical (\"current\"  ) " ;      propertyModel( "graphical", "current"  ) ;
  "is.graphical (\"last\"     ) " ;      propertyModel( "graphical", "last"     ) ;
  "is.graphical (4            ) " ;      propertyModel( "graphical", 4          ) ;
# "is.graphical (\"previous\" ) " ;      propertyModel( "graphical", "previous" ) ;
  "is.graphical (5            ) " ;      propertyModel( "graphical", 5          ) ;
# "is.graphical (\"next\"     ) " ;      propertyModel( "graphical", "next"     ) ;

  showModel("all")

  "is.decomposable (\"base\"     ) " ;   propertyModel ( "decomposable", "base"     ) ;
  "is.decomposable (\"current\"  ) " ;   propertyModel ( "decomposable", "current"  ) ;
  "is.decomposable (\"last\"     ) " ;   propertyModel ( "decomposable", "last"     ) ;
  "is.decomposable (4            ) " ;   propertyModel ( "decomposable", 4          ) ;
# "is.decomposable (\"previous\" ) " ;   propertyModel ( "decomposable", "previous" ) ;
  "is.decomposable (5            ) " ;   propertyModel ( "decomposable", 5          ) ;
# "is.decomposable (\"next\"     ) " ;   propertyModel ( "decomposable", "next"     ) ;

  makeBase(4)
  makeCurrent(6)

  showModel("all")

  "isSubmodel () " ;   isSubmodel () ;

  "isSubmodel (\"base\"     ) " ;   isSubmodel ("base"     ) ;
  "isSubmodel (\"current\"  ) " ;   isSubmodel ("current"  ) ;
  "isSubmodel (\"last\"     ) " ;   isSubmodel ("last"     ) ;
  "isSubmodel (4            ) " ;   isSubmodel (4          ) ;
# "isSubmodel (\"previous\" ) " ;   isSubmodel ("previous" ) ;
  "isSubmodel (5            ) " ;   isSubmodel (5          ) ;
# "isSubmodel (\"next\"     ) " ;   isSubmodel ("next"     ) ;
  "isSubmodel (\"*\"        ) " ;   isSubmodel ("*"        ) ;
  "isSubmodel (\".\"        ) " ;   isSubmodel ("."        ) ;

  showModel("all")

  "isSubmodel (\"current\", \"base\"     ) " ;   isSubmodel ("current", "base"     ) ;
  "isSubmodel (\"current\", \"current\"  ) " ;   isSubmodel ("current", "current"  ) ;
  "isSubmodel (\"current\", \"last\"     ) " ;   isSubmodel ("current", "last"     ) ;
  "isSubmodel (\"current\", 4            ) " ;   isSubmodel ("current", 4          ) ;
# "isSubmodel (\"current\", \"previous\" ) " ;   isSubmodel ("current", "previous" ) ;
  "isSubmodel (\"current\", 5            ) " ;   isSubmodel ("current", 5          ) ;
# "isSubmodel (\"current\", \"next\"     ) " ;   isSubmodel ("current", "next"     ) ;
  "isSubmodel (\"current\", \"*\"        ) " ;   isSubmodel ("current", "*"        ) ;
  "isSubmodel (\"current\", \".\"        ) " ;   isSubmodel ("current", "."        ) ;
  "isSubmodel (\"*\", \"current\"        ) " ;   isSubmodel ("*", "current"        ) ;
  "isSubmodel (\".\", \"current\"        ) " ;   isSubmodel (".", "current"        ) ;

  showModel("all")

  "is.in.one.clique (\"AB;\"               ) " ;   propertySet (query = "in.one.clique", "AB;"             ) ;
  "is.in.one.clique (\"AB;\", \"base\"     ) " ;   propertySet (query = "in.one.clique", "AB;", "base"     ) ;
  "is.in.one.clique (\"AB;\", \"current\"  ) " ;   propertySet (query = "in.one.clique", "AB;", "current"  ) ;
  "is.in.one.clique (\"AB;\", \"last\"     ) " ;   propertySet (query = "in.one.clique", "AB;", "last"     ) ;
  "is.in.one.clique (\"AB;\", 4            ) " ;   propertySet (query = "in.one.clique", "AB;", 4          ) ;
# "is.in.one.clique (\"AB;\", \"previous\" ) " ;   propertySet (query = "in.one.clique", "AB;", "previous" ) ;
  "is.in.one.clique (\"AB;\", 5            ) " ;   propertySet (query = "in.one.clique", "AB;", 5          ) ;
# "is.in.one.clique (\"AB;\", \"next\"     ) " ;   propertySet (query = "in.one.clique", "AB;", "next"     ) ;
  "is.in.one.clique (\"AB;\", \"*\"        ) " ;   propertySet (query = "in.one.clique", "AB;", "*"        ) ;
  "is.in.one.clique (\"AB;\", \".\"        ) " ;   propertySet (query = "in.one.clique", "AB;", "."        ) ;

  showModel("all")

  "isSubmodel (4            ) " ;              isSubmodel (4          ) ;

  showModel("all")

  "is.in.one.clique (\"AB\"                ) " ;   propertySet (query = "in.one.clique", "AB"              ) ;

  showModel("all")

  quitCoCo()
