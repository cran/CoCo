
#  ".clear.coco.objects" <-
#  function (coco.object = NULL, pos = .GlobalEnv) 
#  {
#      result <- NULL
#      if (length(result) > 0) 
#          cat("Warning, ended CoCo-objects:", result, "\n")
#  }
".First.lib" <-
function (lib, pkg) 
{
    require(CoCoCore)
}
#  ".new.coco" <-
#  function (object = .current.coco, type = 1, uniq.title = FALSE, 
#      title = "A CoCo object") 
#  {
#      result <- object
#      if (.is.nil.model(title)) 
#          title <- "A CoCo object"
#      return(result)
#  }
#  ".new.coco.model" <-
#  function (number, model, object = .current.coco, title = "") 
#  {
#      result <- number
#      return(result)
#  }
#  ".object.of.model" <-
#  function (model, data.thing = NULL, object = .current.coco, names = NULL, 
#      levels = NULL, to.factor = NULL) 
#  {
#      result <- object
#      .my.trace(".object.of.model,        start:", level = 20000, 
#          name = "ooo", model = object, object = model)
#      if (.is.nil.model(model)) 
#          result <- .object.of.thing(data.thing = data.thing, to.factor = to.factor, 
#              object = object)
#      else if (class(model) == "CoCoModelClass") {
#          coco.object <- model
#          result <- .recover(coco.object, recover = TRUE)
#      }
#      else if (is.character(data.thing)) {
#          result <- make.coco()
#          if (is.null(names)) 
#              names <- sort(unique(unlist(.split.model.gc(model, 
#                  split.generators = TRUE))))
#          if (is.null(levels)) 
#              levels <- rep(1, length(names))
#          enterNames(names = paste(names, sep = "", collapse = ""), 
#              levels = levels, object = result)
#          enterList(levels, object = result)
#          enterModel("*")
#      }
#      else result <- .object.of.thing(data.thing = data.thing, 
#          to.factor = to.factor, object = object)
#      .my.trace(".object.of.model,        stop:", level = 20000, 
#          name = "OOO", key = -1, id = .return.reference(result), 
#          number = .return.model.number(result), object = result)
#      return(result)
#  }
#  ".object.of.thing" <-
#  function (data.thing = NULL, object = .current.coco, to.factor = NULL) 
#  {
#      result <- object
#      .my.trace(".object.of.thing,        start:", level = 20000, 
#          name = "ooo", model = object)
#      if (is.null(data.thing)) {
#          coco.object <- object
#          result <- .recover(coco.object, recover = TRUE)
#      }
#      else if ((class(data.thing) == "CoCoClass") || (class(data.thing) == 
#          "CoCoModelClass")) {
#          coco.object <- data.thing
#          result <- .recover(coco.object, recover = TRUE)
#      }
#      else if (class(data.thing) == "table") {
#          result <- make.coco()
#          enterNames(names = paste(":", names(dimnames(data.thing)), 
#              sep = "", collapse = ""), levels = dim(data.thing), 
#              object = result)
#          enterTable(data.thing, object = result)
#          enterModel("*")
#      }
#      else if (class(data.thing) == "data.frame") {
#          result <- make.cococg()
#          print(to.factor)
#          enterDataFrame(data.thing, to.factor = to.factor, object = result)
#          enterModel("*")
#      }
#      else if (is.character()) {
#      }
#      .my.trace(".object.of.thing,        stop:", level = 20000, 
#          name = "OOO", key = -1, id = .return.reference(result), 
#          number = .return.model.number(result), object = result)
#      return(result)
#  }
#  ".onLoad" <-
#  function (lib, pkg) 
#  {
#  }
#  ".recover.model" <-
#  function (coco.model.object) 
#  {
#      return(coco.model.object)
#  }
#  ".recover.reference" <-
#  function (coco.object) 
#  {
#      id <- .return.reference(coco.object)
#      return(id)
#  }
#  ".return.key" <-
#  function (object = .current.coco) 
#  {
#      if (all(is.character(object))) 
#          return(object[1])
#      else return(FALSE)
#  }
#  ".return.model.number" <-
#  function (coco.model = .current.coco) 
#  {
#      if (all(is.number(coco.model))) 
#          return(coco.model[1])
#      else return(FALSE)
#  }
#  ".return.model.of.object" <-
#  function (model, object = .current.coco) 
#  {
#      return(FALSE)
#  }
#  ".return.object.model.number" <-
#  function (number, recover = TRUE, object = .current.coco) 
#  {
#      return(FALSE)
#  }
#  ".return.reference" <-
#  function (object = .current.coco) 
#  {
#      if (all(is.number(object))) 
#          return(object[1])
#      else return(FALSE)
#  }
#  ".return.type" <-
#  function (object = .current.coco) 
#  {
#      if (all(is.number(object))) 
#          return(object[2])
#      else return(FALSE)
#  }
".set.asymptotic" <-
function (limit = 0.25, object = .current.coco) 
{
    call.coco.reals(76, limit, 1, FALSE, object = object)
}
".set.coco.value" <-
function (argument, slotid, value, pos = .GlobalEnv, append = FALSE) 
{
    result <- argument
    return(result)
}
".set.em.epsilon" <-
function (epsilon = 0.001, object = .current.coco) 
{
    call.coco.reals(58, epsilon, 1, FALSE, object = object)
}
".set.em.initial" <-
function (code = "what", object = .current.coco) 
{
    x <- c("uniform", "first", "last", "mean", "random", "input")
    x[call.coco.longs(57, .encode(x, code, c(1, 2, 3, 4, 5, 6), 
        1), 1, ifelse(code == "what", -1, FALSE), object = object)]
}
".set.em.max.iterations" <-
function (max = 100, object = .current.coco) 
{
    call.coco.longs(59, max, 1, FALSE, object = object)
}
".set.exact.epsilon" <-
function (epsilon = 1e-07, object = .current.coco) 
{
    call.coco.reals(84, epsilon, 1, FALSE, object = object)
}
".set.exact.test" <-
function (hit = "flop", object = .current.coco) 
{
    x <- c("off", "flop", "on", "all", "deviance")
    y <- call.coco.simple(75, .encode(c("what", x), hit, c(-1, 
        1:5), 2), object = object)
    x[y]
}
".set.ips.epsilon" <-
function (epsilon = 1e-07, object = .current.coco) 
{
    call.coco.reals(55, epsilon, 1, FALSE, object = object)
}
".set.ips.max.iterations" <-
function (max = 100, object = .current.coco) 
{
    call.coco.longs(56, max, 1, FALSE, object = object)
}
".set.ips.stop.criterion" <-
function (code = "what", object = .current.coco) 
{
    c("cell", "sum")[call.coco.simple(53, .encode(c("what", "cell", 
        "sum"), code, c(-1, 1, 2), 1), object = object)]
}
".set.list.of.number.of.tables" <-
function (list.of.numbers = "what", object = .current.coco) 
{
    result <- call.coco.longs(85, list.of.numbers, 25, FALSE, 
        object = object)
    return(result[1:max((1:length(result))[result != 0])])
}
".set.number.of.tables" <-
function (number = 1000, object = .current.coco) 
{
    if ((number == "variating")) 
        call.coco.longs(80, 0, 1, 1, object = object)
    else call.coco.longs(80, number, 1, FALSE, object = object)
}
".set.page.formats" <-
function (args = "what", object = .current.coco) 
{
    call.coco.longs(48, args, 2, FALSE, object = object)
}
".set.paging.length" <-
function (length = "what", object = .current.coco) 
{
    call.coco.longs(63, length, 1, FALSE, object = object)
}
".set.power.lambda" <-
function (lambda = 0.666667, object = .current.coco) 
{
    call.coco.reals(71, ifelse(lambda == "Null", 1, lambda), 
        1, FALSE, object = object)
}
".set.print.formats" <-
function (args = "what", object = .current.coco) 
{
    call.coco.longs(45, args, 2, FALSE, object = object)
}
".set.seed.coco" <-
function (seed = "random", object = .current.coco) 
{
    if ((seed == "random")) 
        call.coco.longs(78, 0, 1, 2, object = object)
    else call.coco.longs(78, seed, 1, FALSE, object = object)
}
".SetSlotValue" <-
function (object, slotid, value) 
{
    return(object)
}
".set.table.formats" <-
function (args = "what", object = .current.coco) 
{
    call.coco.longs(46, args, 4, FALSE, object = object)
}
".set.test" <-
function (code = "what", object = .current.coco) 
{
    x <- c("deviance", "pearson", "power")
    x[call.coco.simple(70, .encode(c("what", "lr", "deviance", 
        "chisq", "pearson", "power"), code, c(-1, 1, 1, 2, 2, 
        3), 1), object = object)]
}
".set.test.formats" <-
function (args = "what", object = .current.coco) 
{
    call.coco.longs(47, args, 4, FALSE, object = object)
}
