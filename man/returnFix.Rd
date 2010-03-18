\name{returnFix}
\alias{returnFix}

\title{ Return the edges fixed in model selection }

\description{
  Return the edges fixed
  (e.i. not considered for removal in the latest call of
  \code{\link{backward}}
   or not considered for entering in the latest call of
  \code{\link{forward}}),
  or edges banned or forced into the EH procedure by \code{\link{ehForceFix}}.
}

\usage{
returnFix(code = "edges", data = NULL, 
          object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{code}{ A text string with
	       \code{"edges"} (edges not considered for removal in the 
			       latest call of \code{\link{backward}}
			       or not considered for entering in the 
			       latest call of \code{\link{forward}}),
               \code{"in"} (edges forced into EH by
		     \code{ehForceFix("what", fix = "in", ...)}),
               or \code{"out"} (edges banned in EH by
		     \code{ehForceFix("what", fix = "out", ...)}).
		     }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{ A text string with a generating class of the fixing. }

\author{Jens Henrik Badsberg}

\note{ Fixing of edges is set by \code{\link{backward}}, \code{\link{forward}},
       and \code{\link{ehForceFix}}.
}

\seealso{ \code{\link{forward}}, \code{\link{backward}},
	   \code{\link{eh}}, \code{\link{ehForceFix}},
           \code{\link{returnFix}} and \code{\link{showOptions}}. }

\examples{
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);

# Enter the saturated model:	
enterModel("*", object = CoCoObject);

# Set a fixing of edges in "backward":

backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE,
         fix.edges = "[[AB][EF]]", object = CoCoObject);

returnFix(object = CoCoObject);

ehForceFix("DE", fix = "in", object = CoCoObject)
ehForceFix("EF", fix = "out", object = CoCoObject)

returnFix("edges", object = CoCoObject);
returnFix("in",    object = CoCoObject);
returnFix("out",   object = CoCoObject);

eh(object = CoCoObject);

returnFix("edges", object = CoCoObject);
returnFix("in",    object = CoCoObject);
returnFix("out",   object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
