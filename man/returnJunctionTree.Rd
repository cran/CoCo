\name{returnJunctionTree}
\alias{returnJunctionTree}

\title{ Return the junction tree }

\description{ Return the junction tree. }

\usage{
returnJunctionTree(model = "current", type = "junction.tree.components",
		   omit.prime.components = FALSE, omit.separators = FALSE,
		   omit.generators = FALSE, state.space = FALSE,
		   return.flags = FALSE, split.sets = FALSE,
		   split.models = TRUE, split.generators = TRUE,
		   eliminate.empty = TRUE, data = NULL,
		   object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
  \item{type}{ A character string: 
               \code{type} should be \code{"junction.tree.components"}. }
  \item{omit.prime.components}{ Logical, if \code{omit.prime.components} 
				is TRUE then prime components are omitted,
				only separators are returned. }
  \item{omit.separators}{ Logical, if \code{omit.separators} 
			  is TRUE then separators are omitted,
			  the prime components are returned. }
  \item{omit.generators}{ Logical, if \code{omit.generators} 
			  is TRUE then generators of non decomposable irreducible
			  components are omitted,
			  for non decomposable irreducible components
			  only the vertex sets are returned. }
  \item{state.space}{ Logical, if \code{state.space} 
		      is TRUE then variable sets of
		      irreducible components are returned. }
  \item{return.flags}{ Logical, if \code{return.flags} 
		       is TRUE then flags are return for each
		       irreducible component of mixed models. }
  \item{split.sets}{ Logical, if \code{split.sets} 
		     is TRUE then for mixed models the character strings for
		     variables in irreducible components and separators
		     are split into lists of variables. }
  \item{split.models}{ Logical, if \code{split.models} 
		       is TRUE then the character string with the mixed model
		       is split into 3 lists: discrete, linear and
		       quadratic generators. }
  \item{split.generators}{ Logical, if \code{split.generators} 
			   is TRUE then each generator of a mixed model
			   is split into a list of variables. }
  \item{eliminate.empty}{ Logical, if \code{eliminate.empty} 
			  is TRUE then empty sets are eliminated. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{ A tree structure with the junction tree. }


\author{Jens Henrik Badsberg}

\note{ The function is only available for CoCoCg objects. }

\seealso{ \code{\link{returnExpression}}. }

\examples{
example.requiring.CoCoCg <- function() {
  library(CoCoCg);
  data(Rats);
  CoCoObject <- makeCoCoCg();
  enterDataFrame(Rats, object = CoCoObject);
  fullModel <- makeModel(enterModel("*", object = CoCoObject));

  enterModel("[[a][b]] / [[ay][ax][bx]] / [[xy]]", object = CoCoObject);

  showModel("all", object = CoCoObject);

  makeCurrent("last", object = CoCoObject);

  str(returnJunctionTree(object = CoCoObject));

  str(returnJunctionTree(split.models = FALSE, object = CoCoObject));
  str(returnJunctionTree(split.generators = FALSE, object = CoCoObject));

  str(returnJunctionTree(omit.generators = TRUE, object = CoCoObject));
  str(returnJunctionTree(omit.generators = TRUE, split.sets = TRUE,
					  object = CoCoObject));

  str(returnJunctionTree(omit.separators = TRUE, split.models = FALSE, 
			 object = CoCoObject));

  str(returnJunctionTree(omit.separators = TRUE, split.models = FALSE, 
			 eliminate.empty = TRUE, object = CoCoObject)); # ?

  str(returnJunctionTree(omit.separators = TRUE, split.models = FALSE,
			 omit.generator = TRUE, object = CoCoObject));

  str(returnJunctionTree(omit.separators = TRUE, split.models = FALSE,
			 return.flags = TRUE,
			 omit.generator = TRUE, object = CoCoObject));

  str(returnJunctionTree(omit.separators = TRUE, split.models = FALSE,
			 return.flags = TRUE, state.space = TRUE,
			 omit.generator = TRUE, object = CoCoObject));

  endCoCo(CoCoObject);
}
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
