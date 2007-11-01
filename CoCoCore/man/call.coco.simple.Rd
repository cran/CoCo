\name{call.coco.simple}
\alias{call.coco.simple}
\alias{coco.simple.command}

\title{ Call Coco with only code and subcode }

\description{ Internal CoCoCore function:
	      Call Coco with only code and subcode. }

\usage{
call.coco.simple(code, sub.code = FALSE, object = CoCoCore::.currentCoCo())

coco.simple.command(code, sub.code = FALSE, object = CoCoCore::.currentCoCo())
}

\arguments{
  \item{code}{ Numeric, between 1 and 255. }
  \item{sub.code}{ Numeric. }
  \item{object}{ See \code{\link[CoCoRaw]{endCoCo}}. }
}

\details{ This function is not to be called by the user. }

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

	\code{\link{call.coco.simple}} is called in
		\code{.set.test},
		\code{.set.datastructure},
		\code{\link[CoCoRaw]{optionsCoCo}}.

	\code{\link{coco.simple.command}} is called in
		\code{.visit.interval},
		\code{.visit.model},
		\code{.visit.models},
		\code{\link{coco.simple.double}},
		\code{\link{coco.simple.model}},
		\code{\link[CoCoOldData]{read.data}},
		\code{\link[CoCoOldData]{read.factors}},
		\code{\link[CoCoOldData]{read.list}},
		\code{\link[CoCoOldData]{read.names}},
		\code{\link[CoCoOldData]{read.observations}},
		\code{\link[CoCoOldData]{read.specification}},
		\code{\link[CoCoOldData]{read.table.coco}},
		\code{\link[CoCoOldData]{skip.missing}},
		\code{\link[CoCoRaw]{backward}},
		\code{\link[CoCoRaw]{cleanData}},
		\code{.base},
		\code{.clean.up},
		\code{.current},
		\code{.omit.test},
		\code{\link[CoCoRaw]{disposeOfEh}},
		\code{\link[CoCoRaw]{ehExtract}},
		\code{\link[CoCoRaw]{ehFindDual}},
		\code{\link[CoCoRaw]{ehFit}},
		\code{\link[CoCoRaw]{ehForceAddDualToClass}},
		\code{\link[CoCoRaw]{eh}},
		\code{\link[CoCoRaw]{ehSetBase}},
		\code{\link[CoCoRaw]{emOn}},
		\code{\link[CoCoRaw]{excludeMissing}},
		\code{\link[CoCoRaw]{forward}},
		\code{\link[CoCoRaw]{randomTableReplacement}},
		\code{\link[CoCoRaw]{setUseVariables}},
		\code{\link[CoCoRaw]{showOptions}},
		\code{\link[CoCoRaw]{showTests}}.

}

% \section{ToDo}{....}

\seealso{ \code{\link{call.coco}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
