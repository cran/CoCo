\name{call.coco.chars}
\alias{coco.enter.string}
\alias{call.coco.chars}
\alias{call.coco.reals}
\alias{call.coco.longs}

\title{ Call Coco with only code, subcode, and one argument }

\description{ Internal CoCoCore functions:
	      Call Coco with only code, subcode, and one argument. }

\usage{
coco.enter.string(code, argument = FALSE, sub.code = FALSE, length = 128,
                  no.warnings = NULL, object = .currentCoCo())

call.coco.chars(code, argument = FALSE, sub.code = FALSE, length = 128,
                no.warnings = NULL, object = .currentCoCo())

call.coco.longs(code, argument = FALSE, length = 0, sub.code = FALSE,
                no.warnings = NULL, object = .currentCoCo())

call.coco.reals(code, argument = FALSE, length = 0, sub.code = FALSE, 
                no.warnings = NULL, object = .currentCoCo())
}

\arguments{
  \item{code}{ Numeric, between 1 and 255. }
  \item{argument}{ The \code{argument} a charater string,
		   a vector of integers or a vector of reals. }
  \item{length}{ Numeric, the \code{length} of the argument. }
  \item{sub.code}{ Numeric. }
  \item{no.warnings}{ Logical, if \code{no.warnings} is TRUE then 
		      no warnings are given from \code{\link{ok.coco}}. }
  \item{object}{ See \code{\link{endCoCo}}. }
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

\note{ \code{\link{coco.enter.string}} calls \code{\link{call.coco.chars}}
       and returns same result.

	\code{\link{coco.enter.string}} is called in
		\code{.return.name.list.string},
		\code{\link{coco.string.double}},
		\code{\link{coco.string.model}},
              % \code{\link[CoCoOldData]{read.q.list}},
              % \code{\link[CoCoOldData]{read.q.table}},
		\code{.eh.enter.base.model},
		\code{.fix.edges},
		\code{\link{disposeOfQtable}},
		\code{\link{ehFit}},
		\code{\link{ehForceAccept}},
		\code{\link{ehForceFix}},
		\code{\link{ehForceReject}},
		\code{\link{ehSetBase}},
		\code{\link{ehSetMainEffects}},
		\code{\link{enterModel}},
		\code{\link{partialAssociations}},
		\code{\link{returnModel}},
		\code{\link{returnModelVariates}},
		\code{\link{returnSets}},
		\code{\link{showTable}}.

	\code{\link{call.coco.chars}}, is called in
		\code{\link{coco.enter.string}},
              % \code{\link[CoCoOldData]{set.data.file}},
              % \code{\link[CoCoOldData]{set.observations.file}},
              % \code{\link[CoCoOldData]{set.output}},
              % \code{\link[CoCoOldData]{set.specification.file}},
              % \code{\link[CoCoOldData]{source.coco}},
              % \code{\link[CoCoOldData]{excludeMissing}},
		\code{\link{exportCoCo}},
		\code{\link{importCoCo}},
		\code{\link{optionsCoCo}},
		\code{\link{setOrdinal}},
		\code{\link{setUseVariables}},
		\code{\link{showTable}},
		\code{\link{sinkCoCo}}.


	\code{\link{call.coco.longs}}, is called in
		\code{.return.factor.type.list},
		\code{.return.level.list},
		\code{.return.missing.list},
		\code{.set.em.initial},
		\code{.set.em.max.iterations},
		\code{.set.ips.max.iterations},
		\code{.set.list.of.number.of.tables},
		\code{.set.number.of.tables},
		\code{.set.page.formats},
		\code{.set.paging.length},
		\code{.set.print.formats},
		\code{.set.seed.coco},
		\code{.set.table.formats},
		\code{.set.test.formats},
		\code{\link{enterList}},
		\code{\link{enterTable}},
		\code{\link{makeBase}},
		\code{\link{makeCurrent}},
		\code{\link{numberVariates}},
		\code{\link{optionsCoCo}},
		\code{\link{returnModelNumber}},
		\code{\link{returnModelVariates}}.


	\code{\link{call.coco.reals}} is called in
		\code{.set.acceptance},
		\code{.set.rejection},
		\code{.set.components},
		\code{.set.separators},
		\code{.set.asymptotic},
		\code{.set.em.epsilon},
		\code{.set.exact.epsilon},
		\code{.set.ips.epsilon},
		\code{.set.power.lambda},
		\code{.set.ic},
		\code{\link{optionsCoCo}}.

       }

% \section{ToDo}{....}

\seealso{ \code{\link{call.coco}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
