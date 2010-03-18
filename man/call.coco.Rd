\name{call.coco}
\alias{call.coco}
\alias{call.coco.message}

\title{ Call CoCo }

\description{ Internal CoCoCore functions:
	      The single interface function to CoCo. }

\usage{
call.coco(code, sub.code = 0, 
          arg.char = "", arg.long = NULL, arg.double = NULL, 
          object = .currentCoCo(), char.ok = FALSE, type = NULL)

call.coco.message(code, sub.code = 0, 
                  arg.char = "", arg.long = NULL, arg.double = NULL, 
                  object = .currentCoCo())

}

\arguments{
  \item{code}{ Numeric, between 1 and 255. }
  \item{sub.code}{ Numeric. }
  \item{arg.char}{ A character string. The argument \code{arg.char}
		   should be long enough for returned values. }
  \item{arg.long}{ A vector of integers. The argument \code{arg.long}
		   should be long enough for returned values. }
  \item{arg.double}{ A vector of reals. The argument \code{arg.double}
		     should be long enough for returned values. }
  \item{object}{ See \code{\link{endCoCo}}. }
  \item{char.ok}{ Logical, if \code{char.ok} then R should be able
		  to call CoCo with strings. }
  \item{type}{ Integer, \code{type} is 1 for CoCo objects,
	       2 for CoCoCg objects. }
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

	\code{\link{call.coco}} is called in
		\code{\link{call.coco.chars}},
		\code{\link{call.coco.longs}},
		\code{\link{call.coco.message}},
		\code{\link{call.coco.reals}},
		\code{\link{call.coco.simple}},
		\code{\link{coco.enter.all}},
		\code{\link{coco.init}},
		\code{\link{coco.replace.all}},
		\code{\link{coco.resume}},
		\code{\link{coco.simple.command}},
		\code{\link{coco.string.double}},
		\code{\link{coco.string.model}},
		\code{\link{endCoCo}},
              % \code{\link[CoCoOldData]{cutpoints}},
		\code{.enter.double.list},
		\code{.plotCoCo},
		\code{.set.switch},
		\code{\link{editModel}},
		\code{\link{enterNames}},
		\code{\link{enterQlist}},
		\code{\link{enterQtable}},
		\code{\link{enterTwoLists}},
		\code{\link{isSubmodel}},
		\code{\link{propertyModel}},
		\code{\link{propertySet}},
		\code{\link{returnDeviance}},
		\code{\link{returnEdges}},
		\code{\link{returnNcells}},
		\code{\link{returnTable}},
		\code{\link{returnTest}},
		\code{\link{showTable}},
		and \code{\link{summaryTable}}.

	\code{\link{call.coco.message}} is called in
              % \code{\link[CoCoOldData]{cutpoints}},
              % \code{\link[CoCoOldData]{or.reject.cases}},
              % \code{\link[CoCoOldData]{or.select.cases}},
              % \code{\link[CoCoOldData]{redefine.factor}},
              % \code{\link[CoCoOldData]{reject.cases}},
              % \code{\link[CoCoOldData]{select.cases}},
		\code{.enter.n.interactions},
		and \code{\link{enterModel}}.

}

% \section{ToDo}{....}

\seealso{ \code{\link{coco.init}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
