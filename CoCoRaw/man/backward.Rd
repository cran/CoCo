\name{backward}

\alias{backward}
\alias{forward}

\title{ Stepwise model selection by backward elimination and
        forward selection of edges or interaction terms }

\description{ This function perform (depth first) search for a single model
	      with all edges or interaction terms significant (backward),
	      or all terms not in the model not significant (forward).
	      The function can also be used to compute tests for
	      all terms in the model or all terms not in the model.
	      }

\usage{
backward(sorted = FALSE, reversed = FALSE, only = FALSE, short = FALSE, 
	 p.accepted = FALSE, p.rejected = FALSE, decomposable.mode = NULL, 
	 coherent = FALSE, headlong = FALSE, recursive = FALSE, 
         follow = FALSE, 
         least.significant = TRUE, 
	 components = NULL, p.components = FALSE, 
	 separators = FALSE, p.separators = FALSE, 
         edges = TRUE, model = FALSE, fix.edges = NULL, data = NULL, 
	 object = .object.of.model(model, data = data, ...), ...)

forward(sorted = FALSE, reversed = FALSE, only = FALSE, short = FALSE, 
        p.accepted = FALSE, p.rejected = FALSE, decomposable.mode = NULL, 
	coherent = FALSE, headlong = FALSE, recursive = FALSE, 
	all.significant = TRUE, 
	components = NULL, p.components = FALSE, 
	separators = FALSE, p.separators = FALSE, 
	edges = TRUE, model = FALSE, fix.edges = NULL, data = NULL, 
	object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{sorted}{ Logical, if \code{sorted} is TRUE
		 then a list of tests is printed
		 sorted according to the selected
                 p-value (statistic, if \code{IC} is selected).
		 For backward the p-values are ordered in
                 increasing order (\code{IC} decreasing),
		 and for forward the p-values are ordered in
                 decreasing order (\code{IC} increasing).
		 The least significant edge is then last for backward,
		 and the most significant edge is then last for forward.
                 }
  \item{reversed}{ Logical, if \code{reversed} is TRUE then
	       the sorted list is printed in reverse order. }
  \item{only}{ Logical, if \code{only} is TRUE then only
	       the sorted list is printed. }
  \item{short}{ Logical, if \code{short} is TRUE then only a short report
                is printed: The test of the edge removed (entered) at
                each cycle. 

		If \code{short} is TRUE and \code{dump}
		(\code{\link{optionsCoCo}}) also is TRUE
		then for each completed set of backward (e.i. each removal
		of an edge or interaction terms), the dump file is rewinded,
		and a report of rejected, accepted, eligible terms
		and the model resulting of the step is printed on the file.
		In the following step of the backward elimination each tested
		term and the selected test statistic for the test of the term
		is printed on the dump file. Similar for forward.
		}

  \item{p.accepted}{ Numeric, edges and interaction terms with tests with
		     p-value (or minus the \code{IC} value)
		     greater than \code{p.accepted} are accepted
		     for removal in backward. 

		     If \code{coherent} is TRUE for
		     forward then terms with 
		     p-value (or minus the \code{IC} value)
		     greater than \code{p.accepted} are not
		     eligible for entering in sub sequential
		     cycles of the forward selection.
		     }

  \item{p.rejected}{ Numeric, and interaction terms with tests with
		     p-value (or minus the \code{IC} value)
		     smaller than \code{p.rejected} are rejected,
		     and thus entered into the model in forward
		     (according to \code{all.significant}). 

		     If \code{coherent} is TRUE for
		     backward then terms with 
		     p-value (or minus the \code{IC} value)
		     smaller than \code{p.rejected} are not
		     eligible for removal in sub sequential
		     cycles of the backward elimination.
		     }

  \item{decomposable.mode}{ Logical, if \code{decomposable.mode} is TRUE
			    then only decomposable models are visited. }

  \item{coherent}{ Logical, if \code{coherent} then the principle of
		   coherence is applied: Once an edge in backward (forward)
		   is rejected (accepted) it is no more tested for
		   removal (entering).
		   }
  \item{headlong}{ Logical, if \code{headlong} is TRUE then in each cycle,
		   e.i. visiting of the edges or interaction terms of the
		   model, terms are only visited until a term is found
		   to remove or enter. 
		   In backward edges are visited until an edge with
		   a p-value (or minus the \code{IC} value)
		   greater than \code{p.accepted} is found,
		   and in forward edges are visited until an edge with
		   a p-value (or minus the \code{IC} value)
		   smaller than \code{p.rejected} is found.
		   }
  \item{recursive}{ Logical, if \code{recursive} is TRUE then cycles
		    of visiting edges or interaction terms are repeated
		    until no more edges are removed in backward
		    and until no more edges are added in forward. }
  \item{follow}{ Logical, if \code{follow} is TRUE then tests in backward
		 are performed against the model selected in the previous
		 cycle, else tests are performed against the BASE model.
		 The option is not available for forward. }
  \item{least.significant}{ Logical, if \code{least.significant} is TRUE then 
			    the least significant edge is removed in backward,
			    else all non significant edges are removed.
		 The option is not available for forward. }
  \item{all.significant}{ Logical, if \code{all.significant} is TRUE then
			  all significant edges are entered in forward,
			  else only the most significant edge is entered.
			  The option is not available for backward. }
  \item{components}{ Logical, if \code{components} is TRUE,
		     then common decompositions of the two models
		     of each tests are found.
		     If any p-value (or minus the \code{IC} value) of
		     the tests on the resulting components is less than 
		     \code{p.components} then the test is rejected. }
  \item{p.components}{ Numeric, edges
		       with a p-value (or minus the \code{IC} value)
		       less than the value \code{p.components}
		       for any component
		       is rejected, if \code{components} is TRUE. }
  \item{separators}{ Logical, if \code{separators} is TRUE, then for each
		     edge all separators of the two vertices of the edge
		     is found, and the edge is rejected, if for any of
		     these separators the two variables not are conditional
		     independent, determined by \code{p.separators}. }
  \item{p.separators}{ Numeric, edges 
		       with a p-value (or minus the \code{IC} value) 
		       less than the value \code{p.separators}
		       for any separator
		       is rejected, if \code{separators} is TRUE. }
  \item{edges}{ Logical, if \code{edges} is TRUE, then in each cycle
		edges of the model are visited, else the maximal interaction
		terms are visited. }
  \item{model}{ See \code{\link{returnModel}}. }
  \item{fix.edges}{ A character string with a generating class.
		    Edges in \code{fix.edges} are not eligible for removal
		    in backward, and are not eligible for entering in forward.
		    if \code{fix.edges} is NULL, default, the fixing of the
		    previous backward or forward is retained. 
		    Use \code{fix.edges = ""} to clear fixing of edges.
		    }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ Note also the options of the sections \code{"tests"}
       (\code{\link{optionsCoCo}}).

       \code{p.accepted}, \code{p.rejected}, \code{p.components},
       \code{p.separators} when set to a value not FALSE
       will set these options for the CoCo object.
       If the arguments are not given to backward and forward
       the the values of the CoCo objects will be used.

       The arguments \code{decomposable.mode} and \code{separators}  
       when different from NULL sets respectively the
       options \code{decomposable.mode} and \code{partitioning}.

       Note that to use \code{BIC} both \code{IC} and \code{BIC}
       has to be set to TRUE, since \code{IC} controls
       whether to use p-values or IC-values,
       and \code{BIC} controls which IC-value to use.
       }

\seealso{ \code{\link{editModel}} and \code{\link{eh}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
