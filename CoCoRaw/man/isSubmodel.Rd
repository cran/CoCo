\name{isSubmodel}
\alias{isSubmodel}

\title{ Is the CURRENT model a sub model of the BASE model ? }

\description{
  Return TRUE if the model of the first argument model is
  a sub model of model of the second argument.
}

\usage{
isSubmodel(model.1 = "current", model.2 = "base", data = NULL, 
           object = .object.of.models(model.1, model.2, 
                                      data = data, ...), ...)
}

\arguments{
  \item{model.1}{ Numeric, model object, text string, or logical.
	 A numeric should give a valid model number in the \code{object},
         see \code{\link{makeCurrent}} about the model list.
         The argument can also be a CoCo model object, see
         \code{\link[CoCoObjects]{makeModel}} about creating model objects.
         A text string can give the model as a generating class
	 (or generating classes in MIM-form for mixed models), see
         \code{\link{enterModel}} for the notation of models.
         The text strings \code{"base"}, \code{"current"}, or \code{"last"}
         refers to the three models.
         The default value \code{"current"} for the CURRENT model
         can also be given as the logical FALSE. }
  \item{model.2}{ As the argument \code{model.1}.
         The default value \code{"base"} can here 
	 also be given as the logical FALSE. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{
  Tests if the generating class (classes for mixed models) for the
  first model is a sub class of the generating class of the second model.
  The causal structures of the two models should be the same.
}

\value{
  Logical, TRUE if the first argument is a sub model of the second argument.

  The value NULL is returned (after warnings) if the two models
  is not available.
}

\references{Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{

%      The function aborted with an error, if the models not are available:
%      Now the value NULL is returned after warnings (and extra line return).

       NULL can be used for FALSE.
%      NULL can now be used for FALSE ("current").

%      The function aborted with an error, 
%      if CURRENT and BASE models not were available.
%      Now the missing pointers will be set as specified by the arguments.

       If CURRENT (BASE) is not available the pointer in the CoCo object
       will after the call be as specified by the \code{model.1}
       (\code{model.2}) argument.

       Model arguments should not be \code{"previous"} or \code{"next"}.

%      If BASE if fully specified (with \code{"\^"}) and CURRENT is not
%      the result might currently not be correct in CoCoCg (interchanged).

%      The two first arguments were interchanged !!!

%      Data from model arguments is not implemented.

     }

\seealso{\code{\link{enterModel}}, \code{\link[CoCoObjects]{makeModel}},
         \code{\link{showModel}},
         \code{\link{returnTest}}, \code{\link{propertyModel}}, 
         \code{\link{makeCurrent}}, \code{\link{returnModel}},
         and \code{\link{returnModelNumber}}. }

\examples{
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Generate some models, here by "backward":
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, 
         object = CoCoObject);

# Show the model list:
showModel("all", object = CoCoObject);

.set.switch(26921, "on");
.set.switch(26931, "on");
.set.switch(11081, "on");
.set.switch(11071, "on");
.set.switch(11061, "on");
.set.switch(6301, "on");

isSubmodel(object = CoCoObject); # TRUE
isSubmodel(1, 4, object = CoCoObject); # FALSE
isSubmodel(4, 1, object = CoCoObject); # TRUE

homogeneModel <- makeModel(enterModel("[[ab]] / [[aby][abx]] / [[x^][abxy][y^]]",
	                   object = CoCoObject));

# Numbers:

n6 <- returnModelNumber(homogeneModel, object = CoCoObject);

isSubmodel(object = CoCoObject); # TRUE
isSubmodel(1, n6, object = CoCoObject); # FALSE
isSubmodel(n6, 1, object = CoCoObject); # TRUE

# Text string with tags:

isSubmodel("base", "current", object = CoCoObject); # FALSE
isSubmodel("current", "base", object = CoCoObject); # TRUE
isSubmodel("last", "base", object = CoCoObject); # TRUE

# Objects:

isSubmodel(fullModel, homogeneModel, object = CoCoObject); # FALSE
isSubmodel(homogeneModel, fullModel, object = CoCoObject); # TRUE

# Non existing models:

isSubmodel("current", 7, object = CoCoObject); # NULL
isSubmodel(7, "base", object = CoCoObject); # NULL

makeCurrent(7, object = CoCoObject);
isSubmodel(1, 4, object = CoCoObject); # FALSE
isSubmodel(4, 1, object = CoCoObject); # TRUE

showModel("all", object = CoCoObject);

makeCurrent(4, object = CoCoObject);
isSubmodel("next", "base", object = CoCoObject); # Using current!
isSubmodel("previous", "base", object = CoCoObject); #  Using current!

# Text string with generating class:

isSubmodel("aby", object = CoCoObject); # TRUE
isSubmodel("ab", "a,b", object = CoCoObject); # FALSE

enterModel("ab", object = CoCoObject);
isSubmodel("aby", object = CoCoObject); # TRUE: Sub model of base!!!!
makeBase(, object = CoCoObject);
isSubmodel("aby", object = CoCoObject); # FALSE

endCoCo(object = CoCoObject);
}
\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
