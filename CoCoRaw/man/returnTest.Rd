\name{returnTest}
\alias{returnTest}
\title{ Return the Pearson Chi-square, the power divergence, etc. }

\description{
 Return for discrete data
 the Pearson \eqn{\chi^2}{X^2}, the power divergence, and the deviance,
 and for tests of two ordinal variable conditional independent
 given discrete also the Goodman and Kruskal's Gamma coefficient,
 and for mixed data the deviance and the F-test.
 For discrete data also exact p-values can be returned.
}

\usage{
returnTest(model.1 = "current", model.2 = "base", push.pop = FALSE, data = NULL, 
           object = .object.of.models(model.1, model.2, data = data, ...), ...)
}

\arguments{
  \item{model.1}{ See \code{\link{isSubmodel}}. 
  % If both model arguments are 
  % the default value FALSE, then the BASE model will be used for both models.
  % If \code{model.2} is set different from FALSE then the default value
  % of \code{model.1} is "current" model.
  }
  \item{model.2}{ See \code{\link{isSubmodel}}. }
  \item{push.pop}{ Logical, if \code{push.pop} is TRUE, both model arguments
		   are numeric and the object argument is a CoCoCg object
		   then model pointers are restored by less calls of CoCoCg. 
		   See also \code{\link{returnModelNumber}}
		   and \code{\link{makeCurrent}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{A labeled vector of reals, or NULL if the an error occur,
       e.g. the models are not available or not nested. }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ % Note that when FALSE (default) is given for both models,
         % the "base" will be used for both models. 

         % Something is wrong inversion 1.6.1 for \code{\link{showTest}},
         % \code{\link{returnTest}}, and \code{\link{backward}}:
         % Homogene models are produced in backward,
         % but tests are preformed as the models are inhomogene?
%       }

\seealso{\code{\link{enterModel}}, \code{\link[CoCoObjects]{makeModel}},
         \code{\link{showModel}}, \code{\link{makeCurrent}},
         \code{\link{returnModel}}, \code{\link{returnModelNumber}},
         \code{\link{partialAssociations}}, \code{\link{optionsCoCo}},
         \code{\link{showTest}}, \code{\link{showDeviance}},
         \code{\link{returnDeviance}}, 
         \code{\link{backward}} and \code{\link{forward}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
