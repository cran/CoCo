\name{make.coco}
\alias{make.coco}
\alias{make.cococg}

\title{ Make a CoCo object }

\description{ Make a CoCo object. }

\usage{
make.coco(n = 65536, p = 65536, q = 1024, 
          r = 65536, s = 65536, ss = 65536, t = 65536, 
          uniq.title = FALSE, title = FALSE, type = 1, 
          location = c(700, 550), manager = TRUE, 
          silent = FALSE, sh.lib.name = NULL, ...)

make.cococg(n = 65536, p = 65536, q = 1024, 
          r = 65536, s = 65536, ss = 65536, t = 65536, 
          uniq.title = FALSE, title = FALSE, 
          location = c(700, 550), manager = TRUE, 
          silent = FALSE, sh.lib.name = NULL)
}

\arguments{
  \item{n}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{p}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{q}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{r}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{s}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{ss}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{t}{ See \code{\link[CoCoCore]{coco.init}}. }

  \item{uniq.title}{ Logical, if \code{uniq.title} is TRUE
		     then it is assumed that the \code{title}
		     can be used as a unique key of the CoCo object. }
  \item{title}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{type}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{location}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{manager}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{silent}{ See \code{\link[CoCoCore]{coco.init}}. }
  \item{sh.lib.name}{ See \code{\link[CoCoCore]{coco.load}}. }
  \item{\dots}{ Additional arguments. }
}

\details{ The distributed source code is only able to handle one concurrent
	  CoCo object. Use \code{\link[CoCoRaw]{endCoCo}}. }

% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{coco.init}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
