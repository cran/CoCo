
### TestReturn

  library(CoCo);

##  ReinisCoCoObject <- Reinis()

  source("TestSubset.S")

  "dimension" ;                       dimension () ;
  "return.marginal.dimension" ;       return.marginal.dimension () ;
  "return.factor.type.list" ;         return.factor.type.list () ;
  "return.level.list" ;               return.level.list () ;
  "return.missing.list" ;             return.missing.list () ;
  "return.name.list.string" ;         return.name.list.string () ;

  exclude.missing("in", ":Sex:Drugs")

  "dimension" ;                       dimension () ;
  "return.marginal.dimension" ;       return.marginal.dimension () ;
  "return.factor.type.list" ;         return.factor.type.list () ;
  "return.level.list" ;               return.level.list () ;
  "return.missing.list" ;             return.missing.list () ;
  "return.name.list.string" ;         return.name.list.string () ;

  "dimension" ;                       dimension (full = T) ;
  "return.marginal.dimension" ;       return.marginal.dimension (full = F) ;
  "return.marginal.dimension" ;       return.marginal.dimension (full = T) ;
  "return.marginal.dimension" ;       return.marginal.dimension (full = F, total = F) ;
  "return.marginal.dimension" ;       return.marginal.dimension (full = T, total = T) ;
  "return.marginal.dimension" ;       return.marginal.dimension (":Sex", full = F) ;
  "return.marginal.dimension" ;       return.marginal.dimension (":Sex", full = T) ;
  "return.marginal.dimension" ;       return.marginal.dimension (":Sex", full = F, total = F) ;
  "return.marginal.dimension" ;       return.marginal.dimension (":Sex", full = T, total = T) ;
  "return.factor.type.list" ;         return.factor.type.list (full = T) ;
  "return.level.list" ;               return.level.list (full = T) ;
  "return.missing.list" ;             return.missing.list (full = T) ;
  "return.name.list.string" ;         return.name.list.string (full = T) ;

  .quit()
