\name{propertySet}
\alias{propertySet}

\title{ Ask a query for a set of variables }

\description{ Test a query for a subset of the variables of a model. }

\usage{
propertySet(query = "in.one.clique", set = "", set.a = "", set.b = "", 
            model = FALSE, prior.action = NULL, 
            modification = NULL, data = NULL, 
            object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{query}{ A character string with the \code{query}:
		\code{"is.separator"}, 
		\code{"is.d-separator"}, 
		or \code{"in.one.clique"}. }
  \item{set}{ A character string: 
	      Test whether \code{set} is in one and only one clique
	      or a separator according to the argument \code{query}. }
  \item{set.a}{ A character string:
		If \code{query} is 
		\code{"is.separator"} or \code{"is.d-separator"}
		and \code{set.a} and \code{set.b} are given,
		then it is tested whether \code{set} separates
		\code{set.a} and \code{set.b} in the model. }
  \item{set.b}{ A character string:  See argument \code{set.a}. }

  \item{model}{ See \code{\link{returnModel}}. }

  \item{prior.action}{ A character string,
		       if \code{prior.action} is set, then
		       the model is modified by this action before
		       asking the query. See \code{\link{editModel}}. }
  \item{modification}{ See \code{\link{editModel}}. }

  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{
    \itemize{
	\item{ In one clique }{:} 
	\item{ Is separator }{: }
	\item{ Is d-separator }{: }
    }
 }

\value{ Logical }

% \references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
%             ( http://www.jstatsoft.org/v06/i04/ )
%             and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

\seealso{ \code{\link{propertyModel}}. }

\examples{
library(CoCo);

data(Fuchs82);

CoCoObject <- makeCoCo();
enterTable(Fuchs82, object = CoCoObject);

enterModel("mdp,da,am,dg,gs,sa", object = CoCoObject);

propertySet("in.one.clique", set = "d,m.", object = CoCoObject);

propertySet(query = "separator", set = "ds.",  object = CoCoObject);
propertySet(query = "separator", set = "a.",   object = CoCoObject);
propertySet(query = "separator", set = "ag.",  object = CoCoObject);
propertySet(query = "separator", set = "adm.", object = CoCoObject);
propertySet(query = "separator", set = "am.",  object = CoCoObject);
propertySet(query = "separator", set = "dm.",  object = CoCoObject);

returnSets(model = FALSE, type = "is.separator", set = "dm.",
           object = CoCoObject); 

propertySet(query = "separator", set = "d.", 
            set.a = "dpm", set.b = "gsa", object = CoCoObject);
propertySet(query = "separator", set = "da.",
            set.a = "dpm", set.b = "gs",  object = CoCoObject);
propertySet(query = "separator", set = "da.",
            set.a = "dpm", set.b = "gsa", object = CoCoObject);
propertySet(query = "separator", set = "dm.",
            set.a = "dpm", set.b = "gsa", object = CoCoObject);
propertySet(query = "separator", set = "ds.", set.a = "dpm", set.b = "ga");

enterModel("gasd,adm,dpm.;", object = CoCoObject);

propertySet("in.one.clique", set = "d,s.", object = CoCoObject);
propertySet("in.one.clique", set = "d,m.", object = CoCoObject);
propertySet("in.one.clique", set = "s,p.", object = CoCoObject);

endCoCo(CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
