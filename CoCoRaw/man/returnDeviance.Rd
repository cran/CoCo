\name{returnDeviance}
\alias{returnDeviance}
\title{ Return likelihoods, deviance, dimensions, etc. }
\description{
  Returns the test statistics (deviance and F-test statistics) with p-values
  based on the likelihoods and the dimensions of the two argument models.
  For discrete models also an adjustment of the degrees of freedom
  is returned.
}
\usage{
returnDeviance(model.1 = "current", model.2 = "base", data = NULL, 
               object = .object.of.models(model.1, model.2, 
                                          data = data, ...), ...)
}
\arguments{
  \item{model.1}{ See \code{\link{isSubmodel}}. 
  % If both model arguments are the default value FALSE,
  % then the BASE model will be used for both models.
  % If \code{model.2} is set different from FALSE then the default value
  % of \code{model.1} is "current" model.
  }
  \item{model.2}{ See \code{\link{isSubmodel}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{A labeled vector of reals. }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ A vector of zeros is returned if an error occurs.
       \code{\link{returnTest}} will return NULL
       if the models are not available or nested.
       % Note that when FALSE (default) is given for both models,
       % the "base" will be used for both models. 
}

\seealso{\code{\link{enterModel}}, \code{\link[CoCoObjects]{makeModel}},
         \code{\link{showModel}}, \code{\link{makeCurrent}},
         \code{\link{returnModel}}, \code{\link{returnModelNumber}},
         \code{\link{partialAssociations}}, \code{\link{optionsCoCo}},
         \code{\link{showTest}}, \code{\link{showDeviance}},
         \code{\link{returnTest}}, 
         \code{\link{backward}} and \code{\link{forward}}. }

\examples{ 

library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# ( Should be replaced by calculation of the tests of David Edwards book. )

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, 
         follow = TRUE, object = CoCoObject);

# Show the model list:
showModel("all", object = CoCoObject);

returnDeviance("last", 1, object = CoCoObject);

homogeneModel <- makeModel(enterModel("[[ab]] / [[aby][abx]] / [[x^][abxy][y^]]",
	                   object = CoCoObject));

returnDeviance("last", 1, object = CoCoObject);

endCoCo(object = CoCoObject);

}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
