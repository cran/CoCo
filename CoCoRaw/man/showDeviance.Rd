\name{showDeviance}
\alias{showDeviance}

\title{ Show the difference between likelihoods, dimension, etc. }

\description{
  Print on standard output the test statistics (deviance and F-test statistics)
  with p-values based on likelihoods and dimensions of the two argument
  models. For discrete models the adjustment of the degrees of freedom
  is also reported.
}

\usage{
showDeviance(model.1 = "current", model.2 = "base", data = NULL, 
             object = .object.of.models(model.1, model.2, data = data, ...), ...)
}

\arguments{
  \item{model.1}{ See \code{\link{isSubmodel}}. }
  \item{model.2}{ See \code{\link{isSubmodel}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{}

\value{
  The value TRUE is returned.
  If an error occurs then a warning is printed.
}

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ Something is wrong inversion 1.6.1 for \code{\link{showTest}},
%        \code{\link{returnTest}}, and \code{\link{backward}}:
%        Homogene models are produced in backward,
%        but tests are preformed as the models are inhomogene? }

\seealso{\code{\link{enterModel}}, \code{\link[CoCoObjects]{makeModel}},
         \code{\link{showModel}}, \code{\link{makeCurrent}},
         \code{\link{returnModel}}, \code{\link{returnModelNumber}},
         \code{\link{partialAssociations}}, \code{\link{optionsCoCo}},
         \code{\link{showTest}}, 
         \code{\link{returnTest}}, \code{\link{returnDeviance}},
         \code{\link{backward}} and \code{\link{forward}}. }

\examples{

library(MASS);
data(anorexia);
library(CoCoCg);

# showDeviance(model.1 ="[[:Prewt:Postwt][:Treat:Postwt]];",
#              model.2 ="*;", data = anorexia);
# endCoCo();

#

CoCoObject <- makeCoCoCg();
enterDataFrame(anorexia, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# fullGraph <- dG(fullModel, title = "Full");

backward(recursive = TRUE, object = CoCoObject);

# lastModel <- makeModel("last", object = CoCoObject);
# backwardGraph <- dG(lastModel, title = "Last");

showModel("all", object = CoCoObject);
makeCurrent(object = CoCoObject);

# These two calls does not produce the same deviance,
# since the second correctly tests the inhomogene model:

showTest(object = CoCoObject);
showDeviance(object = CoCoObject);

# The default models for showTest and showDeviance are "current" and "base",
# but the default models for returnTest and returnDeviance was/are FALSE,
# which both will refer to "base" when no models are set:

returnTest(object = CoCoObject); # model.1 was/is by default "base"!!!
returnTest(FALSE, FALSE, object = CoCoObject);
returnTest("base", "base", object = CoCoObject);
returnTest("current", object = CoCoObject);
returnTest("current", "base", object = CoCoObject);
returnTest(model.2 = 1, object = CoCoObject);
returnTest("base", "current", object = CoCoObject);

returnDeviance(object = CoCoObject); # model.1 was/is by default "base"!!!
returnDeviance(FALSE, FALSE, object = CoCoObject);
returnDeviance("base", "base", object = CoCoObject);
returnDeviance("current", object = CoCoObject);
returnDeviance("current", "base", object = CoCoObject);
returnDeviance(model.2 = 1, object = CoCoObject);
returnDeviance("base", "current", object = CoCoObject);

showTest(3, 1, object = CoCoObject);
showDeviance(3, 1, object = CoCoObject);
returnTest(3, 1, object = CoCoObject);
returnDeviance(3, 1, object = CoCoObject);

showTest(1, 1, object = CoCoObject);
showDeviance(1, 1, object = CoCoObject);
returnTest(1, 1, object = CoCoObject);
returnDeviance(1, 1, object = CoCoObject);

# inhoModel <- makeModel(enterModel("[[:Treat]] / [[:Prewt][:Treat:Postwt]] / [[:Treat:Prewt:Postwt]]", object = CoCoObject));

# inhoModel <- makeModel(enterModel("[[:Treat]] / [[:Prewt][:Treat:Postwt]] / [:Prewt^][:Treat:Prewt:Postwt][:Treat:Postwt^]]", object = CoCoObject));

# inhoModel <- makeModel(enterModel("[[:Treat]] / [[:Prewt][:Treat:Postwt]] / [:Prewt^][:Prewt:Postwt][:Treat:Postwt^]]", object = CoCoObject));

inhoModel <- makeModel(enterModel("[[:Prewt:Postwt][:Treat:Postwt]]", object = CoCoObject));

showModel("all", object = CoCoObject);
makeCurrent(object = CoCoObject);

showTest(object = CoCoObject);
showDeviance(object = CoCoObject);
returnTest("current", "base", object = CoCoObject);
returnDeviance("current", "base", object = CoCoObject);

exportCoCo("anorexia.xpt");

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
