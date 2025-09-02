\name{Res.mhda.1}
\Rdversion{2.0}
\docType{class}
\alias{Res.mhda.1-class}

\title{ Class to contain the MHDA results for a single unit. }

\description{
The function MHDA returns object of class Res.mhda.1 when the Unit argument is not NULL.
}

\section{Objects from the Class}{
new("Res.mhda.1",Obj.a.unit=new("list"),type=new("character"),is.binary=new("character"))
}
\section{Slots}{
  \describe{
    \item{\code{Obj.a.unit}:}{ A list. Each element of the list is a marix, corresponding to one slot. Each row of the matrix 
                 corresponds to one data cell. The first element of the row is sum of the observations in the data cell. 
                 The second element of the row is the number of the observations in the data cell. }
    \item{\code{is.binary}:}{ Logical. TRUE for binary "positive" observations. FALSE for all observations.}
    }
}


\examples{
showClass("Res.mhda.1")
}
\keyword{classes}
