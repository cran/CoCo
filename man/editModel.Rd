\name{editModel}
\alias{editModel}

\title{ Edit model }

\description{ Edit or transform models. }

\usage{
editModel(action = NULL, modification = NULL, model = FALSE, 
	  result.form = "maximal-interaction-terms", omit.test = TRUE, 
	  make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
	  edges = TRUE, dispose = FALSE, data = NULL, 
	  object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{action}{ A character string. 
		 The argument \code{action} should be
		 \code{"marginal.model"},  
		 \code{"collaps.model"},  
		 \code{"generate.decomposable"},  
		 \code{"generate.graphical"}, \   
		 \code{"generate.skeleton"}, \   
		 \code{"generate.moral"},  
		 \code{"dual.to.normal"},  
		 \code{"nor-} \code{mal.to.dual"},  
		 \code{"drop.factor"},  
		 \code{"drop.edges"},  
		 \code{"add.edges"},  
		 \code{"drop.-} \code{interactions"},  
		 \code{"add.interactions"},  
		 \code{"reduce.generator"},  
		 \code{"remove.-} \code{generator"}, \ 
		 \code{"remove.total.interaction"}, \ 
		 \code{"meet.of.models"}, \ (or \code{"intersection"}), 
		 \code{"join.of.models"}, (or \code{"union"}), 
		 \code{"difference.of.-} \code{models"},  
		 or \code{"decompose.models"}.
		 See the section "Details" below. }
  \item{modification}{ A character string or a model as \code{model}.
		       The argument \code{modification}
		       should be a set, generating class,
		       or a model. }
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
         can also be given as the logical FALSE. }
  \item{result.form}{ A character string.
		      Currently only \code{"maximal-interaction-terms"}
		      is implemented for \code{result.form}. }
  \item{omit.test}{ Logical, if \code{omit.test} is TRUE then
		    the resulting model is not tested against the
		    argument model. }
  \item{make.model}{ Logical, if \code{make.model} is TRUE then
		     a model object of the model resulting
		     of the action is returned. }
  \item{return.test}{ Logical, if \code{return.test} is TRUE then
		      a character string with the model resulting
		      of the action is returned. }
  \item{push.pop}{ Logical, if \code{push.pop} is TRUE, the model argument
		   is numeric and the object argument is a CoCoCg object
		   then model pointers are restored by less calls of CoCoCg. 
		   See also \code{\link{returnModelNumber}}
		   and \code{\link{makeCurrent}}. }
  \item{edges}{ A logical for difference between models.
		If \code{edges} is TRUE then the edges not in both models
		are returned. }
  \item{dispose}{ Logical, if \code{dispose} is TRUE then the model
		  resulting of the action is in the CoCo object,
		  and a character string with the model is returned. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{
	\itemize{
	\item{Marginal model }{:}   
	\item{Collaps model }{:}   
	\item{Generate decomposable }{:}   
	\item{Generate graphical }{:}   
	\item{Generate skeleton }{:}   
	\item{Generate moral }{:}   
	\item{Dual to normal }{:}   
	\item{Normal to dual }{:}   
	\item{Drop factor }{:}   
	\item{Drop edges }{:}   
	\item{Add edges }{:}   
	\item{Drop interactions }{:}   
	\item{Add interactions }{:}   
	\item{Reduce generator }{:}   
	\item{Remove generator }{:}   
	\item{Remove total interaction }{:}   
	\item{Meet of models }{: (or \code{"intersection"}) }
	\item{Join of models }{: (or \code{"union"}) }
	\item{Difference of models }{:}   
	\item{Decompose models }{:}   
	}
 }

\value{ The logical TRUE,
	a charaterstring with a generating class of a model,
	a model object,
	or a vector of numerics (as \code{\link{returnTest}}) for a test.
	}

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{enterModel}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
