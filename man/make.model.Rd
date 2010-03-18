\name{make.model}
\alias{make.model}

\title{ Make a CoCo model object }

\description{ Make a CoCo model object. }

\usage{
make.model(model = "*", title = "", push.pop = FALSE, object = .currentCoCo())
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
  \item{title}{ A charater string.
		If \code{title} is not set the the generating class
		of the model is used. }
  \item{push.pop}{ See \code{\link{editModel}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
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

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{enterModel}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
