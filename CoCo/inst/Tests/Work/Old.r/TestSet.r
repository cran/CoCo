
### TestSet

  library(CoCo);
  library(CoCoOldData);
  ReinisCoCoObject <- Reinis()

### TestSet

  "make.current.coco" ;
# make.current.coco () ;
  status("files")
# make.current.coco (2) ;
  status("files")
  make.current.coco (1) ;
  status("files")

  "current.coco" ;
  # current.coco (c(ReinisCoCoObject@.identification, 1)) ;
  status("files")
  current.coco (ReinisCoCoObject) ;
  status("files")

  "set.print.formats" ;               .set.print.formats () ;
  "set.table.formats" ;               .set.table.formats () ;
  "set.test.formats" ;                .set.test.formats () ;
  "set.page.formats" ;                .set.page.formats () ;
  "set.paging.length" ;               .set.paging.length () ;
  "set.ips.stop.criterion" ;          .set.ips.stop.criterion () ;
  "set.ips.stop" ;                    .set.ips.stop.DEPRECATED () ;
  "set.ips.epsilon" ;                 .set.ips.epsilon () ;
  "set.ips.max.iterations" ;          .set.ips.max.iterations () ;
  "set.em.initial" ;                  .set.em.initial () ;
  "set.em.epsilon" ;                  .set.em.epsilon () ;
  "set.em.max.iterations" ;           .set.em.max.iterations () ;
  "set.algorithm" ;                   .set.algorithm.DEPRECATED () ;
  "set.acceptance" ;                  .set.acceptance.DEPRECATED () ;
  "set.rejection" ;                   .set.rejection.DEPRECATED () ;
  "set.test" ;                        .set.test () ;
  "set.power.lambda" ;                .set.power.lambda () ;
  "set.ic" ;                          .set.ic () ;
  "set.components" ;                  .set.components.DEPRECATED () ;
  "set.separators" ;                  .set.separators.DEPRECATED () ;
  "set.exact.test" ;                  .set.exact.test () ;
  "set.asymptotic" ;                  .set.asymptotic () ;
  "coco.set.seed" ;                   .set.seed.coco () ;
  "set.number.of.tables" ;            .set.number.of.tables () ;
  "set.list.of.number.of.tables" ;    .set.list.of.number.of.tables () ;
  "set.exact.epsilon" ;               .set.exact.epsilon () ;
  "set.read" ;                        set.use.variables () ;

# ?
"gryf"
.set.test("gryf")

"set.print.formats" ;              .set.print.formats("what")
"set.table.formats" ;              .set.table.formats("what")
"set.test.formats" ;               .set.test.formats("what")
"set.page.formats" ;               .set.page.formats("what")
"set.paging.length" ;              .set.paging.length("what")
"set.ips.stop.criterion" ;         .set.ips.stop.criterion("what")
# ?
"set.ips.stop" ;                   .set.ips.stop.DEPRECATED("what")
"set.ips.epsilon" ;                .set.ips.epsilon("what")
"set.ips.max.iterations" ;         .set.ips.max.iterations("what")
"set.em.initial" ;                 .set.em.initial("what")
for (i in c("uniform", "first", "last", "mean", "random", "input")) {
  .set.em.initial(i)
  .set.em.initial("what")
}
"set.em.epsilon" ;                 .set.em.epsilon("what")

"set.em.max.iterations" ;          .set.em.max.iterations("what")
"set.algorithm" ;                  .set.algorithm.DEPRECATED("what")
for (i in c("a", "b", "c")) {
  .set.algorithm.DEPRECATED(i)
  .set.algorithm.DEPRECATED("what")
}
"set.acceptance" ;                 .set.acceptance.DEPRECATED("what")
"set.rejection" ;                  .set.rejection.DEPRECATED("what")
"set.test" ;                       .set.test("what")
for (i in c("lr", "pearson", "power", "chisq", "pearson", "power")) {
  .set.test(i)
  .set.test("what")
}
for (i in c("lr")) {
  print(i)
  .set.test(i)
  .set.test("what")
}
"set.power.lambda" ;               .set.power.lambda("what")
"set.ic" ;                         .set.ic("what")
"set.components" ;                 .set.components.DEPRECATED("what")
"set.separators" ;                 .set.separators.DEPRECATED("what")
"set.exact.test" ;                 .set.exact.test("what")
for (i in c("off", "flop", "on", "all", "deviance")) {
  .set.exact.test(i)
  .set.exact.test("what")
}
"set.asymptotic" ;                 .set.asymptotic("what")
"coco.set.seed" ;                  .set.seed.coco("what")
"set.number.of.tables" ;           .set.number.of.tables("what")
"set.list.of.number.of.tables" ;   .set.list.of.number.of.tables("what")
"set.exact.epsilon" ;              .set.exact.epsilon("what")
#
"set.read" ;                       set.use.variables("what")
"set.datastructure" ;              set.datastructure("what")
for (i in c("all", "necessary", "file", "large")) {
  set.datastructure(i)
  set.datastructure("what")
}

status("all")

"set.print.formats" ;              .set.print.formats( c(12, 8) ) ;           .set.print.formats("what")
"set.table.formats" ;              .set.table.formats( c(12, 6, 4, 4) ) ;     .set.table.formats("what")
"set.test.formats" ;               .set.test.formats( c(12, 3, 8, 3) ) ;      .set.test.formats("what")
"set.page.formats" ;               .set.page.formats( c(63, 79) ) ;           .set.page.formats("what")
"set.paging.length" ;              .set.paging.length( c(63) ) ;              .set.paging.length("what")
"set.ips.stop.criterion" ;         .set.ips.stop.criterion( "cell" ) ;        .set.ips.stop.criterion("what")
"set.ips.stop.criterion" ;         .set.ips.stop.criterion( "sum" ) ;         .set.ips.stop.criterion("what")
"set.ips.epsilon" ;                .set.ips.epsilon( c( 0.1 ) ) ;             .set.ips.epsilon("what")
"set.ips.max.iterations" ;         .set.ips.max.iterations( c( 10 ) ) ;       .set.ips.max.iterations("what")
"set.em.epsilon" ;                 .set.em.epsilon( c( 0.01 ) ) ;             .set.em.epsilon("what")
"set.em.max.iterations" ;          .set.em.max.iterations( c( 1 ) ) ;         .set.em.max.iterations("what")
"set.acceptance" ;                 .set.acceptance.DEPRECATED( c( 0.10 ) ) ;             .set.acceptance.DEPRECATED("what")
"set.rejection" ;                  .set.rejection.DEPRECATED( c( 0.25 ) ) ;              .set.rejection.DEPRECATED("what")
"set.power.lambda" ;               .set.power.lambda( c( 0.75 ) ) ;           .set.power.lambda("what")
"set.ic" ;                         .set.ic( "aic" ) ;                         .set.ic("what")
"set.ic" ;                         .set.ic( "bic" ) ;                         .set.ic("what")
"set.ic" ;                         .set.ic( "off" ) ;                         .set.ic("what")
"set.ic" ;                         .set.ic( "on" ) ;                          .set.ic("what")
"set.ic" ;                         .set.ic( .10 ) ;                           .set.ic("what")
"set.ic" ;                         .set.ic( "kappa" , .75 ) ;                 .set.ic("what")
#
"set.ic" ;                         .set.ic( "kappa", "what" ) ;               .set.ic("what")
"set.components" ;                 .set.components.DEPRECATED( c( .50 ) ) ;              .set.components.DEPRECATED("what")
"set.separators" ;                 .set.separators.DEPRECATED( c( .60 ) ) ;              .set.separators.DEPRECATED("what")
"set.asymptotic" ;                 .set.asymptotic( c( .70 ) ) ;              .set.asymptotic("what")
"coco.set.seed" ;                  .set.seed.coco( c( 1 ) ) ;                 .set.seed.coco("what")
"coco.set.seed" ;                  .set.seed.coco( "random" ) ;               .set.seed.coco("what")
"set.number.of.tables" ;           .set.number.of.tables( c( 500 ) ) ;        .set.number.of.tables("what")
#
"set.list.of.number.of.tables" ;   .set.list.of.number.of.tables("what") ;    .set.list.of.number.of.tables("what")
"set.exact.epsilon" ;              .set.exact.epsilon( c( 0.0001 ) ) ;        .set.exact.epsilon("what")
#
"set.read" ;                       set.use.variables( "all" ) ;              set.use.variables("what")
"set.read" ;                       set.use.variables( "subset" ) ;           set.use.variables("what")

status("all")

.quit()
