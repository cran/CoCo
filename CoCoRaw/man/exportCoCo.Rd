\name{exportCoCo}
\alias{exportCoCo}
\alias{importCoCo}

\title{ Write and read the CoCo data and the models to a binary file }

\description{
        Export and import a binary file with the CoCo data and the models. }

\usage{
exportCoCo(file.name, data = NULL, 
           object = .object.of.thing(data = data, ...), ...)

importCoCo(file.name, setslot = TRUE, data = NULL, 
           object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{file.name}{ A character string naming the file to write to. }
  \item{setslot}{ Logical, if \code{setslot} is TRUE then the entered
                  value of \code{file.name} is added to the CoCo \code{object},
		  such that the \code{object} can be recovered
		  after ending the CoCo \code{object} by,
		  e.g. \code{\link[CoCoRaw]{endCoCo}},
		  or after terminating the R session and restarting R. }
  \item{data}{ An optional argument with the "data", only used if
	       the argument \code{object} is not given.
	       If the class of \code{data} is \code{"table"} or \code{"array"},
	       then the table is entered into a pure discrete CoCo object.
	       If the class of \code{data} is \code{"data.frame"}
	       (or \code{"matrix"}),
	       then the data frame is entered into a CoCoCg object. 
	       Columns of class \code{"factor"}
	       will for \code{data} of class \code{"data.frame"}
	       be considered as discrete variables,
  	       and columns of class \code{"numeric"}
	       will be considered as continuous variables.
    	       Columns of class \code{"numeric"} can be converted to
	       discrete variables by the argument \code{to.factor},
	       see \code{\link{enterDataFrame}}.
	       }
  \item{object}{ The CoCo object with the data and models to use.
		 If neither \code{object} or \code{data} is given,
		 then the object \code{CoCoCore::.currentCoCo()} of
		 \code{.GlobalEnv} is used as default object,
		 see \code{\link[CoCoObjects]{makeCurrentCoCo}}.
		 }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{ Options, the values of \code{\link{optionsCoCo}} is not exported,
	  as neither is the setting of \code{\link{setUseVariables}},
          \code{\link{excludeMissing}}, and \code{\link{emOn}}.
	  \code{\link{enterQtable}} and \code{\link{enterQlist}}
	  are exported. }

\value{ A character string with the file name \code{file.name}. }

\author{Jens Henrik Badsberg}

\note{ \code{\link{exportCoCo}} and \code{\link{importCoCo}} is 
       intended to facilitate using the same data in R+CoCo, Xlisp+CoCo and 
       standalone CoCo by only entering the data into one of the programs.
       Once the data has been entered into CoCo in one of the three systems,
       the data can be exported, and then imported to a standalone CoCo
       or CoCo loaded under the other system. This will work with the same
       version number of CoCo, and probably also between different platforms
       (Unix, Linux, Windows, etc.). In future versions of CoCo the format
       of the binary file might change, and thus the binary files from
       \code{\link{exportCoCo}} should not be used for storing data.
       }

\seealso{ \code{\link{importCoCo}}. }

\examples{

library(MASS)
data(anorexia)
library(CoCoCg);

# Without "data" require ending the CoCo object:

# exportCoCo("Anorexia.xpt", data = anorexia);
# endCoCo();

# With CoCo object:

library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);

exportCoCo("Rats.xpt", object = CoCoObject);

endCoCo(object = CoCoObject);

# Import the first example:

Anorexia <- makeCoCo();
importCoCo("Anorexia.xpt", object = Anorexia);
showOptions("specification");
endCoCo(object = Anorexia);

# Import the second example:

Rats <- makeCoCoCg();
importCoCo("Rats.xpt", object = Rats);
showOptions("specification");
endCoCo(object = Rats);

}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
