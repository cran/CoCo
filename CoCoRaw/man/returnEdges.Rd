\name{returnEdges}
\alias{returnEdges}
\title{ Return the indices of vertices of edges }

\description{ Return a matrix with the indices of the vertices of the edges.
	      The edges can be the edges of the model,
	      the edges not in the model,
	      or the model can be ignored
	      (and thus the edges are returned from the
	      saturated model according to the fixing of edges).
	      And the edges can be edges not fixed in the model,
	      edges fixed in the model,
	      or the fixing of edges can be ignored.
}

\usage{
returnEdges(model = "current", edges = "in.model", fix = FALSE, data = NULL,
            object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
  \item{edges}{ Text string: \code{"in.model"}, 
		\code{"all"}, or \code{"not.in.model"}. 
		Default: \code{"in.model"}. }
  \item{fix}{ Text string:  \code{"fix.edges"}, 
	      \code{"ignore.fixing"} (ignore fixing of edges),
	      or \code{"not.fix.edges"}. 
	      Default: \code{"not.fix.edges"}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ A matrix of integers with the indices of the variables of the edges. }

\author{Jens Henrik Badsberg}

\note{ Fixing of edges is set by \code{\link{backward}},
       \code{\link{forward}}, and by \code{\link{ehForceFix}}.
}

\seealso{ \code{\link{backward}}, \code{\link{forward}},
	   \code{\link{eh}}, \code{\link{ehForceFix}},
           \code{\link{returnFix}} and \code{\link{showOptions}}. }

\examples{

library(CoCoCg);

# Without "data" require ending the CoCo object:

# returnEdges("ab,xy", data = "From.input", continuous = "xy");
# endCoCo();

# With CoCo object:

data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);

Base <- makeModel(enterModel("[[ab]] / [[ay][ax][bx]] / [[xy]]",
                              object = CoCoObject));

Current <- makeModel(enterModel("[[a][b]] / [[ay][ax][bx]] / [[xy]]",
                              object = CoCoObject));

showModel("all");

returnEdges(object = CoCoObject);

returnEdges("base",    object = CoCoObject); # Does not work ?!?
returnEdges(Base,      object = CoCoObject);
returnEdges(1,         object = CoCoObject);
returnEdges("current", object = CoCoObject);
returnEdges(Current,   object = CoCoObject);
returnEdges(4,         object = CoCoObject);
returnEdges("last",    object = CoCoObject);
returnEdges(8,         object = CoCoObject); # ?!?: Should return NULL!!!

returnEdges("ab",      object = CoCoObject);
returnEdges("xy",      object = CoCoObject);
returnEdges("axy",     object = CoCoObject);

backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, 
         fix.edges = "xy", object = CoCoObject);

# .fix.edges("xy", object = CoCoObject);

returnEdges(object = CoCoObject);

returnEdges(fix = "all",             object = CoCoObject);
returnEdges(fix = "fix.edges",       object = CoCoObject);
returnEdges(fix = "non.fix.edges",   object = CoCoObject);

returnEdges(edges = "all",           object = CoCoObject);
returnEdges(edges = "in.model",      object = CoCoObject);
returnEdges(edges = "not.in.model",  object = CoCoObject);

returnEdges(edges = "all",          fix = "all",           object = CoCoObject);
returnEdges(edges = "in.model",     fix = "all",           object = CoCoObject);
returnEdges(edges = "not.in.model", fix = "all",           object = CoCoObject);

returnEdges(edges = "all",          fix = "fix.edges",     object = CoCoObject);
returnEdges(edges = "in.model",     fix = "fix.edges",     object = CoCoObject);
returnEdges(edges = "not.in.model", fix = "fix.edges",     object = CoCoObject);

returnEdges(edges = "all",          fix = "non.fix.edges", object = CoCoObject);
returnEdges(edges = "in.model",     fix = "non.fix.edges", object = CoCoObject);
returnEdges(edges = "not.in.model", fix = "non.fix.edges", object = CoCoObject);

endCoCo(object = CoCoObject);

}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
