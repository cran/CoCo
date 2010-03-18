\name{is.gc}
\alias{is.gc}

\title{ Generating classes }

\description{ Internal CoCoCore function: 
              Test for objects of type '"character"', 
	      but not one of the for CoCo reserved text strings.
}

\usage{
is.gc(model = "*")
}

\arguments{
  \item{model}{ A character string.
                See also \code{\link{returnModel}}. }
}

\details{ This function is not to be called by the user. }

\value{ Logical }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{is.character}}. }

\examples{ 
is.gc("ab,bc.")
is.gc("[[ab][bc]]")
is.gc("[[ab]] / [[axy]] / [[axy]]")
is.gc("current")
is.gc(1)
}

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
