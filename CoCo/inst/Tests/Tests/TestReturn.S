
### TestReturn

  library(CoCo);

##  ReinisCoCoObject <- Reinis()

  source("TestSubSet.S")

  "dimension" ;                       numberVariates () ;
  "return.marginal.dimension" ;       returnNcells () ;
  "return.factor.type.list" ;         .return.factor.type.list () ;
  "return.level.list" ;               .return.level.list () ;
  "return.missing.list" ;             .return.missing.list () ;
  "return.name.list.string" ;         .return.name.list.string () ;

  excludeMissing("in", ":Sex:Drugs")

  "dimension" ;                       numberVariates () ;
  "return.marginal.dimension" ;       returnNcells () ;
  "return.factor.type.list" ;         .return.factor.type.list () ;
  "return.level.list" ;               .return.level.list () ;
  "return.missing.list" ;             .return.missing.list () ;
  "return.name.list.string" ;         .return.name.list.string () ;

  "dimension" ;                       numberVariates (full = T) ;
  "return.marginal.dimension" ;       returnNcells (full = F) ;
  "return.marginal.dimension" ;       returnNcells (full = T) ;
  "return.marginal.dimension" ;       returnNcells (full = F, total = F) ;
  "return.marginal.dimension" ;       returnNcells (full = T, total = T) ;
  "return.marginal.dimension" ;       returnNcells (":Sex", full = F) ;
  "return.marginal.dimension" ;       returnNcells (":Sex", full = T) ;
  "return.marginal.dimension" ;       returnNcells (":Sex", full = F, total = F) ;
  "return.marginal.dimension" ;       returnNcells (":Sex", full = T, total = T) ;
  "return.factor.type.list" ;         .return.factor.type.list (full = T) ;
  "return.level.list" ;               .return.level.list (full = T) ;
  "return.missing.list" ;             .return.missing.list (full = T) ;
  "return.name.list.string" ;         .return.name.list.string (full = T) ;

  quitCoCo()
