\name{showTest}
\alias{showTest}
\title{ Show the Pearson $\chi^2$, the power divergence, etc. }

\description{
 Print on standard output for discrete data
 the Pearson $\chi^2$, the power divergence, and the deviance,
 and for tests of two ordinal variable conditional independent
 given discrete also the Goodman and Kruskal's Gamma coefficient,
 and for mixed data the deviance and the F-test statistic, all with p-values.
 For discrete data also exact p-values can be printed.
}

\usage{
showTest(model.1 = "current", model.2 = "base", exact.test = NULL, 
         break.down = "", set = ";", only.if.one.edge = FALSE, data = NULL, 
         object = .object.of.models(model.1, model.2, data = data, ...), ...)
}

\arguments{
  \item{model.1}{ See \code{\link{isSubmodel}}. }
  \item{model.2}{ See \code{\link{isSubmodel}}. }
  \item{exact.test}{ Logical, if \code{exact.test} is TRUE then exact p-values
		     are computed by Monte Carlo simulation. }
  \item{break.down}{ Text-string:  
  \code{"edges"}: Break down the test in a sequence of tests,
	   each test a test between two models differing by one edge.
	   The order of the edges are controlled by the argument \code{set}.
  \code{"interactions"}: Break down the test in a sequence of tests, each test 
		  a test between two models differing by one interaction term.
		  The order of the terms are controlled by
		  the argument \code{set}.
  \code{"components"}:  Find sets that are complete separators for both models,
		 and show tests collapsed to each component.
  \code{"show.common.decompositions"}:  Do not perform a test, show
				 separators which are complete in both models.
  \code{"decompose.models"}: Do not perform a test, but decompose both
		      models with respect to the argument \code{set}.
		      }
  \item{set}{ Text string with set of variables,
	      see the argument \code{break.down}. }
  \item{only.if.one.edge}{ Logical, if \code{only.if.one.edge} is set to TRUE
			   then the test is only performed
			   if the models differs with one edge and only
			   one edge. }
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

\note{ % Something is wrong inversion 1.6.1 for \code{\link{showTest}},
       % \code{\link{returnTest}}, and \code{\link{backward}}:
       % Homogene models are produced in backward,
       % but tests are preformed as the models are inhomogene?

       }

\seealso{\code{\link{enterModel}}, \code{\link[CoCoObjects]{makeModel}},
         \code{\link{showModel}}, \code{\link{makeCurrent}},
         \code{\link{returnModel}}, \code{\link{returnModelNumber}},
         \code{\link{partialAssociations}}, \code{\link{optionsCoCo}},
         \code{\link{showDeviance}},
         \code{\link{returnTest}}, \code{\link{returnDeviance}}, 
         \code{\link{backward}} and \code{\link{forward}}. }

\examples{ 
# data(HairEyeColor);
# library(CoCo);
# showTest(":H:S,:E:S", "*", data = HairEyeColor, exact.test = "on");
# endCoCo();

library(MASS);
data(cabbages);
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(cabbages, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, 
         follow = TRUE, object = CoCoObject);

makeCurrent("last", object = CoCoObject);
showTest("last", object = CoCoObject);

returnTable("moment", ":Cult:Date:HeadWt:VitC", object = CoCoObject);
returnTable("moment", ":Cult:Date:HeadWt:VitC", matrix = TRUE, 
            object = CoCoObject);

returnTable("mk", ":Cult:Date:HeadWt:VitC", matrix = TRUE, 
            object = CoCoObject);

showTable("mk", ":Cult:Date:HeadWt:VitC", matrix = TRUE, 
           object = CoCoObject);

showTable("mk", ":Cult:Date:HeadWt:VitC", 
           object = CoCoObject);

showTable("ms", ":Cult:Date:HeadWt:VitC", 
           object = CoCoObject);

endCoCo(CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
