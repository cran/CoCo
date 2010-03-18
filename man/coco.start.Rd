\name{coco.start}
\alias{coco.start}
\alias{coco.resume}

\title{ Enter a CoCo object }

\description{ CoCo is started within R, 
	      and CoCo commands are then expected from standard input.

	      \code{\link{coco.resume}} will enter an existing CoCo
	      object. }

\usage{
coco.start(n = 131072, p = 65536, q = 1024, 
           r = 65536, s = 65536, ss = 65536, t = 65536)

coco.resume(object = .currentCoCo())
}

\arguments{
  \item{n}{ See \code{\link{coco.init}}. }
  \item{p}{ See \code{\link{coco.init}}. }
  \item{q}{ See \code{\link{coco.init}}. }
  \item{r}{ See \code{\link{coco.init}}. }
  \item{s}{ See \code{\link{coco.init}}. }
  \item{ss}{ See \code{\link{coco.init}}. }
  \item{t}{ See \code{\link{coco.init}}. }
  \item{object}{ See \code{\link{endCoCo}}. }
}

\details{ These functions are to be called with care. }

\value{ NULL }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

\section{ToDo}{ This will crash both CoCo and R under Windows. }

\seealso{ \code{\link{coco.init}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
