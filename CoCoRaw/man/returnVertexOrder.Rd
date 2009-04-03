\name{returnVertexOrder}
\alias{returnVertexOrder}

\title{ Return vertex order }

\description{ Return vertex orderings, including MaxCard, LexM and paths. }

\usage{
returnVertexOrder(model = FALSE, invers.order = FALSE, default.order = TRUE,
                  max.card = FALSE, path.order = FALSE, sub.path = FALSE,
                  marked = paste(";"), u = "", v = "",
                  names = NULL, levels = NULL, to.factor = NULL,
                  discrete = NULL, continuous = NULL, data = NULL,
                  object = .object.of.model(model, data = data, names = names,
                                            levels = levels, to.factor = to.factor,
                                            discrete = discrete,
                                            continuous = continuous, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }

  \item{invers.order}{ Logical, if \code{invers.order} is TRUE
                       then . }
  \item{default.order}{ Logical, if \code{default.order} is TRUE
                       then . }
  \item{max.card}{ Logical, if \code{max.card} is TRUE
                       then . }
  \item{path.order}{ Logical, if \code{path.order} is TRUE
                       then . }
  \item{sub.path}{ Logical, if \code{sub.path} is TRUE
                       then . }
  \item{marked}{ A character string, \code{marked} 
                       then . }
  \item{u}{ A character string, \code{u} 
                       then . }
  \item{v}{ A character string, \code{v} 
                       then . }

  \item{names}{ A character string, 
		\code{names} ... }
  \item{levels}{ A vector of integers, 
		 \code{levels} ... }
  \item{to.factor}{ A vector of integers, 
		    \code{to.factor} ... }
  \item{discrete}{ A vector of integers or a character string, 
		   \code{discrete} ... }
  \item{continuous}{ A vector of integers or a character string,
		     \code{continuous} ... }

  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

% \value{ }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

\seealso{ ~~objects to See Also as \code{\link{coco.init}}, ~~~ }

\examples{
library(CoCo);

# Without "data" does not require ending the CoCo object:

# returnVertexOrder("ab,xy", continuous = "xy");

# With CoCo object:

data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);

# Enter the saturated model:	
enterModel("*", object = CoCoObject);

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, 
         follow = TRUE, object = CoCoObject);

# Show the model list:
showModel("all", object = CoCoObject);

n <- returnModelNumber("last", object = CoCoObject);
for (i in 1:n) {
  showFormula(i, object = CoCoObject);
  showVertexOrder(i, object = CoCoObject);
  print(returnVertexOrder(i, object = CoCoObject));
}

returnVertexOrder(9, object = CoCoObject);
returnVertexOrder(9, max.card = TRUE, object = CoCoObject);
returnVertexOrder(9, default.order = FALSE, max.card = TRUE, object = CoCoObject);
returnVertexOrder(9, default.order = FALSE, max.card = FALSE, object = CoCoObject);

returnVertexOrder(9, marked = "AB;", object = CoCoObject);
returnVertexOrder(9, marked = "EF;", object = CoCoObject);

returnVertexOrder(9, u = "B;", v = "F;", 
                  path.order = TRUE, sub.path = TRUE, object = CoCoObject);
returnVertexOrder(9, u = "C;", v = "F;", 
                  path.order = TRUE, sub.path = TRUE, object = CoCoObject);

endCoCo(CoCoObject);

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data(Fuchs82);

CoCoObject <- makeCoCo();
enterTable(Fuchs82, object = CoCoObject);

enterModel("mdp,da,am,dg,gs,sa", object = CoCoObject);

returnVertexOrder(model = "current", 
                  path.order = TRUE, marked = paste("pd"),
                  u = "m", v = "g", object = CoCoObject);

returnVertexOrder(model = "current", invers.order = TRUE,
                  path.order = TRUE, marked = paste("pd"),
                  u = "m", v = "g", object = CoCoObject);

returnVertexOrder(model = "current", sub.path = TRUE,
                  path.order = TRUE, marked = paste("pd"),
                  u = "m", v = "g", object = CoCoObject);

returnVertexOrder(model = "current", 
                  path.order = TRUE, marked = paste("adps"),
                  u = "m", v = "g", object = CoCoObject);

returnVertexOrder(model = "current", sub.path = TRUE,
                  path.order = TRUE, marked = paste("adps"),
                  u = "m", v = "g", object = CoCoObject);

returnVertexOrder(model = "current", 
                  path.order = TRUE, marked = paste("adms"),
                  u = "p", v = "g", object = CoCoObject);


returnVertexOrder(model = "current", sub.path = TRUE,
                  path.order = TRUE, marked = paste("adms"),
                  u = "p", v = "g", object = CoCoObject);

returnVertexOrder(model = "current", 
                  path.order = TRUE, marked = paste("adms"),
                  u = "g", v = "p", object = CoCoObject);


returnVertexOrder(model = "current", sub.path = TRUE,
                  path.order = TRUE, marked = paste("adms"),
                  u = "g", v = "p", object = CoCoObject); 


showVertexOrder();

# Default order:
VertexOrder <- returnVertexOrder(model = "current",
                                 invers.order = FALSE,
                                 object = CoCoObject); 
VertexOrder

# Default order, inverse:
InverseOrder <- returnVertexOrder(model = "current",
                                  invers.order = TRUE,
                                  object = CoCoObject); 
InverseOrder

# Lex order:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = FALSE,
                  object = CoCoObject); 

# Lex order, inverse:
returnVertexOrder(model = "current", invers.order = TRUE,
                  default.order = FALSE,  max.card = FALSE,
                  object = CoCoObject); 

# Max-card order:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = TRUE,
                  object = CoCoObject); 

# Max-card order, inverse:
returnVertexOrder(model = "current", invers.order = TRUE,
                  default.order = FALSE,  max.card = TRUE,
                  object = CoCoObject); 

##

# Marked order, default:
returnVertexOrder(model = "current", invers.order = FALSE,
                  marked = "g", object = CoCoObject); 

# Marked order, Lex:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = FALSE,
                  marked = "g", object = CoCoObject); 

# (Marked order), Max-card:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = TRUE,
                  marked = "g", object = CoCoObject); 

##

# Marked order, default:
returnVertexOrder(model = "current", invers.order = FALSE,
                  marked = "d", object = CoCoObject); 

# Marked order, Lex:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = FALSE,
                  marked = "d", object = CoCoObject); 

# (Marked order), Max-card:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = TRUE,
                  marked = "d", object = CoCoObject); 

##

# Marked order, default:
returnVertexOrder(model = "current", invers.order = FALSE,
                  marked = "p", object = CoCoObject); 

# Marked order, Lex:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = FALSE,
                  marked = "p", object = CoCoObject); 

# (Marked order), Max-card:
returnVertexOrder(model = "current", invers.order = FALSE,
                  default.order = FALSE,  max.card = TRUE,
                  marked = "p", object = CoCoObject); 

endCoCo(CoCoObject);

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

library(CoCoCg);

data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE,
         object = CoCoObject);

homogeneModel <- makeModel(enterModel("[[ab]] / [[aby][abx]] / [[x^][abxy][y^]]",
	                     object = CoCoObject));

enterModel("ab,bx,xy", object = CoCoObject);

enterModel("ax,xy,yb", object = CoCoObject);

enterModel("xa,ab,by", object = CoCoObject);

# Show the model list:
showModel("all", object = CoCoObject);

n <- returnModelNumber("last", object = CoCoObject);
for (i in 1:n) {
  showFormula(i, object = CoCoObject);
  print(returnVertexOrder(i, object = CoCoObject));
}

endCoCo(CoCoObject);
}
\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
