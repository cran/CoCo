\name{disposeOfQtable}
\alias{disposeOfQtable}

\title{ Dispose of Q-tables and Q-lists (structurally zero cells) }

\description{ Dispose of a Q-table or a Q-list. }

\usage{
disposeOfQtable(set, data = NULL, 
                object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{set}{ A character string identifying the Q-table or Q-list
	      to dispose of by the \code{set} 
	      of names of the variables of the table. }
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

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{enterQlist}} and \code{\link{enterQtable}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
