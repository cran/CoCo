\name{enterDataFrame}
\alias{enterDataFrame}

\title{ Enter a data.frame into a CoCo object }

\description{ Enter a data frame into a CoCo object
	      with declaring the variables. }

\usage{
enterDataFrame(data.frame, to.factor = NULL, missing.values = c("."), 
               setslot = TRUE, data = NULL, 
               object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{data.frame}{ A object of class \code{"data.frame"}.
		     The argument \code{data.frame} is
		     with continuous and discrete variables
		     as respectively numerics and factors. 
		     The names of the variables are extracted from
		     the column names of the \code{data.frame}. }
  \item{to.factor}{ A numeric vector. Numeric columns of \code{data.frame}
		    with indices in the argument \code{to.factor}
		    are converted into factors. }
  \item{missing.values}{ A vector or list of values to be considered as
			 missing values.
			 If the argument \code{missing.values} is not a list
			 then values in \code{missing.values} are considered
			 as missing values for all the discrete variables,
			 that is the factors.
			 If \code{missing.values} is a list,
			 then it should have length equal to
			 the number of columns of the data.frame,
			 and values of the \code{i}-the variable are 
			 considered as missing if they are among the values
			 of the \code{i}-te element of \code{missing.values}.
			 }
  \item{setslot}{ See \code{\link{enterNames}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{enterNames}}. }

\examples{
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
