\name{returnModelVariates}
\alias{returnModelVariates}

\title{ Return the variates of the model }

\description{ Return the variables of the model. }

\usage{
returnModelVariates(model = FALSE, as.string = TRUE, data = NULL,
                    object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
  \item{as.string}{ Logical, if \code{as.string} is false then
		    a vector of integers is returned.
		    1 (0) for variables (not) in the model. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{
  A character string with the variates of the model,
  or a vector of integers,  1 (0) for variables (not) in the model.
}

\references{Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\seealso{ \code{\link{returnModel}}. }

\examples{
example.requiring.CoCoCg <- function() {
library(CoCoCg);

data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

enterModel("ab,by", object = CoCoObject);
enterModel("bx", object = CoCoObject);

returnModelVariates(1, object = CoCoObject);
returnModelVariates(object = CoCoObject);

returnModelVariates(1, as.string = FALSE, object = CoCoObject);
returnModelVariates(as.string = FALSE, object = CoCoObject);
returnModelVariates(3, as.string = FALSE, object = CoCoObject);

returnModelVariates("by", as.string = FALSE, object = CoCoObject);

endCoCo(object = CoCoObject);
}
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
