\name{showVertexOrder}
\alias{showVertexOrder}

\title{ Show the vertex ordering }

\description{ Print on standard output the vertex ordering used to decompose
	      the model. }

\usage{
showVertexOrder(model = FALSE, data = NULL, 
                object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE }

\references{ Badsberg, J.H.: A guide to CoCo, JSS, 2001
             ( http://www.jstatsoft.org/v06/i04/ )
             and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

\seealso{ \code{\link{returnVertexOrder}}. }

\examples{
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);

# Enter the saturated model:	
enterModel("*", object = CoCoObject);

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, 
         coherent = TRUE, follow = TRUE, object = CoCoObject);

# Show the model list:
showModel("all", object = CoCoObject);

n <- returnModelNumber("last", object = CoCoObject);
for (i in 1:n) {
  showFormula(i, object = CoCoObject);
  showVertexOrder(i, object = CoCoObject);
}

endCoCo(CoCoObject);

library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, 
         follow = TRUE, object = CoCoObject);

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
  # showVertexOrder(i, object = CoCoObject); # Fault !!!!
}

endCoCo(CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
