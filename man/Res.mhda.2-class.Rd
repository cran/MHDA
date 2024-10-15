\name{Res.mhda.2}
\Rdversion{1.4}
\docType{class}
\alias{Res.mhda.2-class}

\title{ Class to contain the MHDA results for Category.I and Category.II. }

\description{
The function MHDA returns object of class Res.mhda.2 when the Unit argument is NULL.
}

\section{Objects from the Class}{
new("Res.mhda.2",Obj.all.units=new("matrix"),Obj.category=new("list"),type=new("character"),is.binary=new("character"))
}
\section{Slots}{
  \describe{
    \item{\code{Obj.all.units}:}{ Matrix. Each row of the matrix corresponds to one unit. Odd elements of the row are 
                sum of the observations in each slot. Even elements of the row are the number of observations in each slot. 
                in one slot for one unit. Each row corresponds to one unit. Each column corresponds to one slot. }
    \item{\code{Obj.category}:}{ List.  Each element of the list is a matrix corresponding to one slot.
                The (i,j) element of a matrix is sum of the observations in the i-th level of Category.I 
                and the j-th level of Category.II. When Category.I or Category.II is NULL in the arguments, it is meant that 
                that category is ignored to be whole levels together.} 
    \item{\code{type}:}{ Character. "Value" for the value number of the observations. "Count" for counting the number of 
                observations. }  
    \item{\code{is.binary}:}{ Logical. TRUE for binary "positive" observations. FALSE for all observations.}
    }
}

\examples{
showClass("Res.mhda.2")
}
\keyword{classes}
