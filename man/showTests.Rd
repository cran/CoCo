\name{showTests}
\alias{showTests}
\title{ Show all the stored tests of a CoCo object }
\description{
  Print on standard output all the stored tests in the CoCo object.
}

\usage{
showTests(data = NULL, object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ Reuse of tests in the CoCo object can be turned of by
       \code{optionsCoCo(reuse.tests=FALSE)}. Thus no tests
       are inserted into the test list of the CoCo object.
}

\seealso{\code{\link{optionsCoCo}},
         \code{\link{showTest}}, \code{\link{showDeviance}},
         \code{\link{returnTest}}, \code{\link{showTest}}, \code{\link{eh}},
         \code{\link{backward}} and \code{\link{forward}}. }

\examples{
# For 'CoCoCg':

# library(CoCoCg);
# data(Rats);
# CoCoObject <- makeCoCoCg();
# enterDataFrame(Rats, object = CoCoObject);

# For 'CoCo', discrete data:

# Attach data, here table of counts:
data(Reinis);

# Create a CoCo-object:
CoCoObject <- makeCoCo();

# Enter the table of counts into the CoCo-object:
enterTable(Reinis, object = CoCoObject);

# Enter the saturated model into the CoCo-object, and return a model object:
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Display a graph of the model (requires 'library(CoCoGraph)'):
# fullGraph <- dg(fullModel, title = "Full", vertexColor = "blue",
#                 width = 300, height = 300, returnLink = TRUE);

# Do a backward elimination of edges:
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE,
         follow = TRUE, object = CoCoObject);

# Show the model list:
showModel("all", object = CoCoObject);

# Show the test list:
showTests(object = CoCoObject);

# Close the CoCo-object:
endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
