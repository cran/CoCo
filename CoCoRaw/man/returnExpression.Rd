\name{returnExpression}
\alias{returnExpression}

\title{ Return the expression of the estimates of parameters }

\description{ Return closed form expression for the parameter estimates. }

\usage{
returnExpression(model = "current", type = "expression", 
		 omit.prime.components = FALSE, omit.separators = FALSE, 
		 omit.generators = FALSE, state.space = FALSE,
		 return.flags = FALSE, split.sets = FALSE, 
		 split.models = TRUE, split.generators = TRUE, 
		 eliminate.empty = TRUE, data = NULL, 
		 object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
  \item{type}{ A character string:  \code{type} can be \code{"expression"}, 
	       \code{"prime.components"}, \code{"separators"}, 
               or \code{"junction.tree.components"}.
	       The type \code{"junction.-"}, \code{"tree.components"}
	       are only avaliable in CoCoCg objects.
    }
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
		       irreducible component of mixed models.
		       Only for CoCoCg objects. 
		       See also \code{\link{returnJunctionTree}}. }
  \item{split.sets}{ Logical, if \code{split.sets} 
		     is TRUE then for mixed models the character strings for
		     variables in irreducible components and separators
		     are split into lists of variables.
		     Only for CoCoCg objects. 
		     See also \code{\link{returnJunctionTree}}. }
  \item{split.models}{ Logical, if \code{split.models} 
		       is TRUE then the character string with the mixed model
		       is split into 3 lists: discrete, linear and
		       quadratic generators.
		       Only for CoCoCg objects. 
		       See also \code{\link{returnJunctionTree}}. }
  \item{split.generators}{ Logical, if \code{split.generators} 
			   is TRUE then each generator of a mixed model
			   is split into a list of variables.
			   Only for CoCoCg objects. 
			   See also \code{\link{returnJunctionTree}}. }
  \item{eliminate.empty}{ Logical, if \code{eliminate.empty} 
			  is TRUE then empty sets are eliminated.
			  Only for CoCoCg objects. 
			  See also \code{\link{returnJunctionTree}}. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{
  For discrete model in CoCo, a list with
  \item{[[1]]}{A character string with complete components, separators and
	       non-decomposable irreducible components with generators, }
  \item{[[2]]}{A numeric vector with the indices of
               the complete components and separators, }
  \item{[[3]]}{A constant for the model depending on
               the discrete variables not in the model. }

 In CoCoCg ...
}

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

\seealso{ \code{\link{returnJunctionTree}}. }

\examples{
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);

# Enter the saturated model:	
enterModel("*", object = CoCoObject);

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE);

# Show the model list:
showModel("all", object = CoCoObject);

makeCurrent("last", object = CoCoObject);

returnExpression(object = CoCoObject);

str(returnExpression(type = "expression", object = CoCoObject));

str(returnExpression(omit.prime.components = TRUE, object = CoCoObject));
str(returnExpression(omit.separators = TRUE, object = CoCoObject));
str(returnExpression(omit.generators = TRUE, object = CoCoObject));

str(returnExpression(type = "separators", object = CoCoObject));
str(returnExpression(type = "prime.components", object = CoCoObject));

str(returnExpression(state.space = TRUE, object = CoCoObject)); # ?

endCoCo(CoCoObject);

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

enterModel("[[a][b]] / [[ay][ax][bx]] / [[xy]]", object = CoCoObject);

showModel("all", object = CoCoObject);

makeCurrent("last", object = CoCoObject);

returnExpression(object = CoCoObject);

str(returnExpression(type = "expression", object = CoCoObject));

str(returnExpression(omit.prime.components = TRUE, object = CoCoObject));
str(returnExpression(omit.separators = TRUE, object = CoCoObject));
str(returnExpression(omit.generators = TRUE, object = CoCoObject));

str(returnExpression(type = "separators", object = CoCoObject));
str(returnExpression(type = "prime.components", object = CoCoObject));

str(returnExpression(state.space = TRUE, object = CoCoObject)); # ?

str(returnExpression(return.flags = TRUE, object = CoCoObject));

str(returnExpression(split.sets = TRUE, object = CoCoObject)); # ?
str(returnExpression(eliminate.empty = FALSE, object = CoCoObject)); # ?

str(returnExpression(type = "junction.tree.components",
                     split.models = FALSE, object = CoCoObject));

str(returnExpression(type = "junction.tree.components",
                     split.generators = FALSE, object = CoCoObject));

endCoCo(CoCoObject);

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data(Fuchs82);

CoCoObject <- makeCoCo();
enterTable(Fuchs82, object = CoCoObject);

enterModel("mdp,da,am,dg,gs,sa", object = CoCoObject);


# State-space, with generators:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = FALSE, omit.separators = FALSE,
                 omit.generators = FALSE, state.space = TRUE, object = CoCoObject);

# State-space, without generators:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = FALSE, omit.separators = FALSE,
                 omit.generators = TRUE, state.space = TRUE, object = CoCoObject);

showFormula(object = CoCoObject);

# State-space, with generators:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = FALSE, omit.separators = FALSE,
                 omit.generators = FALSE, state.space = TRUE, object = CoCoObject);

# State-space, without generators:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = FALSE, omit.separators = FALSE,
                 omit.generators = TRUE, state.space = TRUE, object = CoCoObject);

# Expression:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = FALSE, omit.separators = FALSE,
                 omit.generators = FALSE, state.space = FALSE, object = CoCoObject);

# Expression, without generators:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = FALSE, omit.separators = FALSE,
                 omit.generators = TRUE, state.space = FALSE, object = CoCoObject);

# Prime components:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = FALSE, omit.separators = TRUE,
                 omit.generators = TRUE, state.space = FALSE, object = CoCoObject);
returnExpression(model = FALSE, type = "prime.components", object = CoCoObject);

# Separators:
returnExpression(model = FALSE, type = "expression", 
                 omit.prime.components = TRUE, omit.separators = FALSE,
                 omit.generators = TRUE, state.space = FALSE, object = CoCoObject);
returnExpression(model = FALSE, type = "separators", object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
