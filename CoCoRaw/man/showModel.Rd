\name{showModel}
\alias{showModel}

\title{ Show the model }

\description{ Print on standard output the model.
	      The model can also be given a longer description with
	      adjacency matrix and expression for estimates.
}

\usage{
showModel(model = FALSE, a = FALSE, b = FALSE, describe.model = FALSE, 
          data = NULL, object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ Numeric, model object, text string, or logical.
	 A numeric should give a valid model number in the \code{object},
         see \code{\link{makeCurrent}} about the model list.
         The argument can also be a CoCo model object, see
         \code{\link[CoCoObjects]{makeModel}} about creating model objects.
         The text strings \code{"base"}, \code{"current"}, or \code{"last"}
         refers to the three models.
	 The text strings \code{"interval"} will show the interval
	 given by the arguments \code{a} and \code{b}.
         The default value \code{"current"} for the CURRENT model
         can also be given as the logical FALSE (or NULL).
	 \code{model} can also be a vector or list with the above,
	 but not recursive.
         \code{model} can not be a character string with a generating class.
	 }
  \item{a}{ Numeric, \code{a} is the first model of the interval. }
  \item{b}{ Numeric, \code{b} is the last model of the interval. }
  \item{describe.model}{ Logical, if \code{describe.model} is TRUE
			 the the longer description is given. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{enterModel}} and \code{\link{makeCurrent}}. }

\examples{
library(CoCo);
data(Byssinosis38);
CoCoObject <- makeCoCo();
enterTable(Byssinosis38, object = CoCoObject);

# Generate some models:

enterModel("*", object = CoCoObject);
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, 
         follow = TRUE, object = CoCoObject);
backwardResult <- returnModelNumber("last", object = CoCoObject);
enterModel(".;", object = CoCoObject);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = CoCoObject);
makeCurrent(backwardResult, object = CoCoObject);

# Show the models:

showModel("all", object = CoCoObject);
showModel("last", object = CoCoObject);
showModel("base", object = CoCoObject);
showModel("current", object = CoCoObject);
showModel("number", 3, object = CoCoObject);

#  showModel(FALSE, object = CoCoObject);
#  showModel(TRUE, object = CoCoObject);
#  showModel(NULL, object = CoCoObject);

#  showModel(c(1, 2, c(7)), object = CoCoObject);
#  showModel("list", list(1, 2), object = CoCoObject);

#  showModel(c("current", 7, 9, "last"), object = CoCoObject);
#  showModel(c("current", "base", "last"), object = CoCoObject);

#  showModel("list", list("current", 2), object = CoCoObject);

# Show the model "backwardResult" down to 1:

while(makeCurrent("previous", 
                  object = CoCoObject)) showModel(object = CoCoObject);

# Show the model 10 to 15 3 times:

showModel(10:15, object = CoCoObject);

showModel("interval", 10, 15, object = CoCoObject);

makeCurrent(10, object = CoCoObject)
while(returnModelNumber("current", object = CoCoObject) < 15) {
	 makeCurrent("next", object = CoCoObject); 
	 showModel(object = CoCoObject); }

makeCurrent(backwardResult, object = CoCoObject);

showModel(6, describe.model = TRUE, object = CoCoObject);
showModel("current", describe.model = TRUE, object = CoCoObject);

showModel(1, describe.model = TRUE, object = CoCoObject);

# "base" and "last" gives shorter description:

showModel("base", describe.model = TRUE, object = CoCoObject);
showModel("last", describe.model = TRUE, object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
