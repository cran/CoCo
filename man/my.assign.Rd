\name{my.assign}
\alias{my.assign}

\title{ Assign a value to a name }

\description{
  Internal CoCoCore function: 
  Assign a value to a name in an environment.
}

\usage{
my.assign(x, value, frame)
}

\arguments{
  \item{x}{ A character string with the name
	    of the variable to assign to. }
  \item{value}{ The \code{value} to assign \code{x}. }
  \item{frame}{ Numeric, the argument \code{frame} is
		zero for \code{\link{.GlobalEnv}}. }
}

\details{ This function is not to be called by the user. }

\value{ See \code{\link{assign}}. }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{assign}} and \code{\link{coco.init}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
