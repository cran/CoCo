\name{endCoCo}
\alias{endCoCo}
\alias{quitCoCo}

\title{ End a CoCo object }

\description{ End a CoCo object
              and remove the temporary files of the CoCo object. }

\usage{
endCoCo(object = .current.coco, silent = FALSE)

quitCoCo(..., object = .current.coco)
}

\arguments{
  \item{\dots}{ Additional arguments to \code{\link{quit}}. }
  \item{object}{ See \code{\link[CoCoRaw]{exportCoCo}}. }
  \item{silent}{ Logical, if TRUE then CoCo is more \code{silent}. }
}

% \details{ }

% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

\note{ The distributed source code is only able to handle one concurrent
CoCo object. Use \code{\link{endCoCo}}. }

% \section{ToDo}{....}

\seealso{ \code{\link{coco.init}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
