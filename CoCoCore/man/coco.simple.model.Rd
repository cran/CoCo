\name{coco.simple.model}
\alias{coco.simple.model}
\alias{coco.simple.double}
\alias{coco.string.model}
\alias{coco.string.double}

\title{ Call CoCo with setting of models }

\description{ Internal CoCoCore functions:
	      
}

\usage{
coco.simple.model(code, sub.code = FALSE, model = FALSE, 
                  type = "unconditioned", data = NULL, 
                  object = .object.of.model(model, data = data, ...), ...)

coco.simple.double(code, sub.code = FALSE, 
                   model.1 = "current", model.2 = "base", 
                   type = "unconditioned", data = NULL, 
                   object = .object.of.models(model.1, model.2, 
                                              data = data, ...), ...)

coco.string.model(code, argument = FALSE, sub.code = FALSE, model = FALSE, 
                  make.model = FALSE, 
                  push.pop = FALSE, type = "unconditioned", data = NULL, 
                  object = .object.of.model(model, data = data, ...), ...)

coco.string.double(code, argument = FALSE, sub.code = FALSE, 
                   model.1 = "current", model.2 = "base", 
                   eliminate = TRUE, 
                   make.model = FALSE, return.test = FALSE, 
                   push.pop = FALSE, type = "unconditioned", data = NULL, 
                   object = .object.of.models(model.1, model.2, 
                                              data = data, ...), ...)
}

\arguments{
  \item{code}{ Numeric, between 1 and 255. }
  \item{sub.code}{ Numeric. }
  \item{argument}{ A charater string, \code{argument}. }
  \item{model}{ See \code{\link[CoCoRaw]{returnModel}}. }
  \item{model.1}{ See \code{\link[CoCoRaw]{isSubmodel}}. }
  \item{model.2}{ See \code{\link[CoCoRaw]{isSubmodel}}. }

  \item{eliminate}{ Logical, \code{eliminate} is TRUE
		    then \code{"last"} is set to \code{"current"},
		    else \code{"last"} is set to  \code{"base"}
		    before computing test when \code{return.test} is TRUE. }
  \item{make.model}{ Logical, if \code{make.model} is TRUE then
		     a model object of the model resulting
		     of the action is returned.
		     See \code{\link[CoCoRaw]{editmodel}}. }
  \item{return.test}{ Logical, if \code{return.test} is TRUE then
		      a character string with the model resulting
		      of the action is returned.
		      See \code{\link[CoCoRaw]{editmodel}}. }
  \item{push.pop}{ Logical, if \code{push.pop} is TRUE, both model arguments
		   are numeric and the object argument is a CoCoCg object
		   then model pointers are restored by less calls of CoCoCg. 
		   See also \code{\link[CoCoRaw]{returnModelNumber}}
		   and \code{\link[CoCoRaw]{makeCurrent}}. }
  \item{type}{ A charater string. The argument \code{type} should be
	       \code{"unconditioned"}, 
	       \code{"ok"}, 
	       \code{"long.true"}, 
	       \code{"double"}, 
	       or \code{"long.and.double"}.
 }

  \item{data}{ See \code{\link[CoCoRaw]{exportCoCo}}. }
  \item{object}{ See \code{\link[CoCoRaw]{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link[CoCoRaw]{propertyModel}}. }
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
	\code{\link{coco.simple.model}} is called in
		\code{.dispose.of.fitted.values},
		\code{\link[CoCoRaw]{editModel}},
		\code{\link[CoCoRaw]{showFormula}},
		\code{\link[CoCoRaw]{showVertexOrder}}.

	\code{\link{coco.simple.double}} is called in
		\code{.show.log.lik},
		\code{\link[CoCoRaw]{showDeviance}},
		\code{\link[CoCoRaw]{showTest}}.

	\code{\link{coco.string.model}} is called in
		\code{\link[CoCoRaw]{editModel}},
		\code{\link[CoCoRaw]{showTable}}.

	\code{\link{coco.string.double}} is called in
		\code{.decompose.models},
		\code{\link[CoCoRaw]{editModel}},
		\code{\link[CoCoRaw]{showTest}}.

}

% \section{ToDo}{....}

\seealso{ \code{\link{call.coco}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
