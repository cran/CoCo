\name{call.coco.simple}
\alias{call.coco.simple}
\alias{coco.simple.command}

\title{ Call Coco with only code and subcode }

\description{ Internal CoCoCore function:
	      Call Coco with only code and subcode. }

\usage{
call.coco.simple(code, sub.code = FALSE, object = .currentCoCo())

coco.simple.command(code, sub.code = FALSE, object = .currentCoCo())
}

\arguments{
  \item{code}{ Numeric, between 1 and 255. }
  \item{sub.code}{ Numeric. }
  \item{object}{ See \code{\link{endCoCo}}. }
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
		\code{\link{optionsCoCo}}.

	\code{\link{coco.simple.command}} is called in
		\code{.visit.interval},
		\code{.visit.model},
		\code{.visit.models},
		\code{\link{coco.simple.double}},
		\code{\link{coco.simple.model}},
              % \code{\link[CoCoOldData]{read.data}},
              % \code{\link[CoCoOldData]{read.factors}},
              % \code{\link[CoCoOldData]{read.list}},
              % \code{\link[CoCoOldData]{read.names}},
              % \code{\link[CoCoOldData]{read.observations}},
              % \code{\link[CoCoOldData]{read.specification}},
              % \code{\link[CoCoOldData]{read.table.coco}},
              % \code{\link[CoCoOldData]{skip.missing}},
		\code{\link{backward}},
		\code{\link{cleanData}},
		\code{.base},
		\code{.clean.up},
		\code{.current},
		\code{.omit.test},
		\code{\link{disposeOfEh}},
		\code{\link{ehExtract}},
		\code{\link{ehFindDual}},
		\code{\link{ehFit}},
		\code{\link{ehForceAddDualToClass}},
		\code{\link{eh}},
		\code{\link{ehSetBase}},
		\code{\link{emOn}},
		\code{\link{excludeMissing}},
		\code{\link{forward}},
		\code{\link{randomTableReplacement}},
		\code{\link{setUseVariables}},
		\code{\link{showOptions}},
		\code{\link{showTests}}.

}

% \section{ToDo}{....}

\seealso{ \code{\link{call.coco}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
