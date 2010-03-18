\name{returnModel}
\alias{returnModel}

\title{ Return the model }

\description{ Return the model or parts of it as a generating class. }

\usage{
returnModel(model = "current", type = "both", as.edges = FALSE, 
            split.string = FALSE, split.generators = FALSE, data = NULL, 
            object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ Numeric, model object, text string, or logical.
	 A numeric should give a valid model number in the \code{object},
         see \code{\link{makeCurrent}} about the model list.
         The argument can also be a CoCo model object, see
         \code{\link{makeModel}} about creating model objects.
         A text string can give the model as a generating class
	 (or generating classes in MIM-form for mixed models), see
         \code{\link{enterModel}} for the notation of models.
         The text strings \code{"base"}, \code{"current"}, or \code{"last"}
         refers to the three models.
         The default value \code{"current"} for the CURRENT model
         can also be given as the logical FALSE.
	 }
  \item{type}{ A character string, selecting
               \code{"gc"}: generating class(es),
	       \code{"cs"}: causal structure,
	       \code{"both"}: both the generating class(es) and
			      the causal structure,
	       \code{"discrete"}: discrete part of mixed models,
	       \code{"linear"}: linear part of mixed models,
	       and \code{"quadratic"}: quadratic part of mixed models.
	       }
  \item{as.edges}{ Logical, if \code{as.edges} is TRUE then the edges
		   of the model are returned. }
  \item{split.string}{ Logical, if \code{split.string} is TRUE
		       then the generating classes are split into 
		       character strings with the generators. }
  \item{split.generators}{ Logical, if \code{split.generators} is TRUE
		       then the generating classes are split into lists,
		       each item in the list a vector of character string
		       with the vertices of a generator. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{
	A character string, a vector of character strings,
	a list of vectors of character strings,
	or a matrix of integers.
}

\references{Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ Currently only the generating class can be returned by \code{as.edges}. }

\seealso{ \code{\link{returnEdges}}. }

\examples{
example.requiring.CoCoCg <- function() {
library(CoCoCg);

# Without "data" require ending the CoCo object:

# returnModel("abxy,bcyz", type = "linear", data = "From.input", 
#             continuous = "xyz");
# endCoCo();

# With CoCo a object:

data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

enterModel("[[b][a]] / [[by][bx][ay][ax]] / [[xy]]", object = CoCoObject);
enterModel("[[a][b]] / [[ay][ax][bx]] / [[xy]]", object = CoCoObject);

homogeneModel <- makeModel(enterModel("[[ab]] / [[aby][abx]] / [[x^][abxy][y^]]",
	                   object = CoCoObject));

# Show the model list:

showModel("all", object = CoCoObject);

returnModel(object = CoCoObject);

returnModel(type="discrete", object = CoCoObject);
returnModel(type="linear", object = CoCoObject);
returnModel(type="quadratic", object = CoCoObject);

returnModel(type="gc", object = CoCoObject);
returnModel(type="cs", object = CoCoObject);

returnModel(3, type="gc", object = CoCoObject);

returnModel(3, type="discrete", object = CoCoObject);
returnModel(3, type="linear", object = CoCoObject);
returnModel(3, type="quadratic", object = CoCoObject);

returnModel(3, object = CoCoObject, split.string = TRUE);
str(returnModel(3, object = CoCoObject, split.generators = TRUE));

returnModel(3, object = CoCoObject, as.edges = TRUE);
.fix.edges("xy")
returnModel(3, object = CoCoObject, as.edges = TRUE);
returnModel(3, object = CoCoObject, as.edges = TRUE, fix = "all");

returnModel("abx", type = "discrete", object = CoCoObject);
returnModel("a,bx", type = "discrete", object = CoCoObject);

endCoCo(object = CoCoObject);
}
}
\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
