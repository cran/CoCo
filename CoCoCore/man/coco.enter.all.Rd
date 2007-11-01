\name{coco.enter.all}
\alias{coco.enter.all}
\alias{coco.replace.all}

\title{ Call CoCo with all arguemnts }

\description{ Internal CoCoCore functions:
	      Call Coco with all arguments. }

\usage{
coco.enter.all(code, string, long, double, 
               sub.code = FALSE, object = CoCoCore::.currentCoCo())

coco.replace.all(code, string = "", long = NULL, double = NULL, 
                 sub.code = FALSE, object = CoCoCore::.currentCoCo())
}

\arguments{
  \item{code}{ Numeric, between 1 and 255. }
  \item{sub.code}{ Numeric. }
  \item{string}{ A charater string. The argument \code{arg.char}
		 should be long enough for returned values. }
  \item{long}{ A vector of integers. The argument \code{arg.long}
	       should be long enough for returned values. }
  \item{double}{ A vector of reals. The argument \code{arg.double}
		 should be long enough for returned values. }
  \item{object}{ See \code{\link[CoCoRaw]{endCoCo}}. }

}

\details{ These functions are not to be called by the user. }

% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

\note{ 
	\code{\link{coco.enter.all}} is called in
		\code{.return.complex},
		\code{\link[CoCoRaw]{optionsCoCo}},
		\code{\link[CoCoRaw]{returnVertexOrder}}.

	\code{\link{coco.replace.all}} is called in
		\code{\link[CoCoRaw]{optionsCoCo}},
		\code{\link[CoCoRaw]{returnSets}},
		\code{\link[CoCoRaw]{returnTable}}.

}

% \section{ToDo}{....}

\seealso{ \code{\link{call.coco}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
