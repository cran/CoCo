\name{showOptions}
\alias{showOptions}

\title{ Show the options of the CoCo object }

\description{ Print on standard output the options of the CoCo object. }

\usage{
showOptions(hit = "all", dump = FALSE, data = NULL, 
            object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{hit}{ A character string \code{"all"}, \code{"formats"}, 
              \code{"tests"}, \code{"exact"}, \code{"fix"}, \code{"ips"}, 
              \code{"cips"}, \code{"mips"}, \code{"em"}, \code{"old"}, 
              \code{"specification"}, \code{"factors"}, \code{"observations"}, 
              \code{"data"}, \code{"limits"}, \code{"files"}, \code{"other"}, 
              \code{"search"}, or \code{"eh"}. }
  \item{dump}{ Logical, if \code{dump} is TRUE, then write CoCo commands for
	       setting the options. Only implemented for CoCoCg. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\seealso{ \code{\link{optionsCoCo}}. }

\examples{
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
showOptions(object = CoCoObject);
showOptions("specification", object = CoCoObject);

sinkCoCo(file.name = "Diary.tmp", type = "diary", object = CoCoObject);
showOptions("formats", object = CoCoObject);
showOptions("formats", dump = TRUE, object = CoCoObject);

sinkCoCo(file.name = "Diary.tmp", type = "diary", object = CoCoObject);
showOptions("tests", object = CoCoObject);
showOptions("tests", dump = TRUE, object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
