\name{enterNames}
\alias{enterNames}

\title{ Enter a specification of variables into the CoCo object }

\description{ Enter a specification of variables. 
	      After specifying the variables by the function
	      the observations should be entered by 
	      \code{\link{enterList}} or \code{\link{enterTable}}. }

\usage{
enterNames(names = c("a", "b", "c"), levels = c(2, 2, 2), 
           missing = NULL, setslot = TRUE, data = NULL, 
           object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{names}{ A vector of character strings with the names of the variables.
		If any of the \code{names} of the variables has more than
		one character then all the names should have colon,
		\code{":"}, as the first character. }
  \item{levels}{ A vector of integers.
		 The number of \code{levels}, excluding missing levels,
		 for each of the discrete variable.
		 For continuous variables the number of \code{levels}
		 should be zero, \code{0}.
		 }
  \item{missing}{ A vector of integers.
		 The number of \code{missing} levels 
		 for each variable.  }
  \item{setslot}{ Logical, if \code{setslot} is TRUE then
                  the entered values are added to the CoCo \code{object},
		  such that the \code{object} can be recovered
		  after ending the CoCo \code{object} by,
		  e.g. \code{\link{endCoCo}},
		  or after terminating the R session and restarting R. }
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

\note{ Between \code{\link{enterNames}} and
       \code{\link{enterList}} or \code{\link{enterTable}}
       a subset of the variables can be selected by
       \code{\link{setUseVariables}}.
     }

% \section{ToDo}{....}

\seealso{ \code{\link{setUseVariables}}, \code{\link{excludeMissing}},
	  \code{\link{enterList}}, and \code{\link{enterTable}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
