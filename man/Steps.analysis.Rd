\name{Steps.analysis}
\alias{Steps.analysis}
\title{Stepwise analysis of a series of numeric observations on multiple levels of rhythms}
\description{
    It's a function to conduct stepwise analysis on a series of numeric observations,
 specifically stepwisely computing the mean of observations on each rhythm. Missing observations are allowed. Maximally available for seven Rhythms.
}  
\usage{
Steps.analysis(ID, Tag, S, Rhythm1, Rhythm2, Rhythm3, Rhythm4, Rhythm5, 
Rhythm6, Rhythm7, Start, plot)
}
\arguments{
  \item{ID}{ Character String. Label for the data. }
  \item{Tag}{ Character String. Label for the data. }
  \item{S}{ Numeric. A series of numeric numbers. Missing observations are allowed in the series.  }
  \item{Rhythm1}{ Integer. The number of steps on the first rhythm. }
  \item{Rhythm2}{ Integer. The number of steps on the second rhythm. }
  \item{Rhythm3}{ Integer. The number of steps on the third rhythm. }
  \item{Rhythm4}{ Integer. The number of steps on the fourth rhythm. }
  \item{Rhythm5}{ Integer. The number of steps on the fifth rhythm. }
  \item{Rhythm6}{ Integer. The number of steps on the sixth rhythm. }
  \item{Rhythm7}{ Integer. The number of steps on the seventh rhythm. }
  \item{Start}{ Character String. Description for the initial number with format "a+b", where a and b are integers. 
                For example, "2+3" means that the initial observation in S is the third step in Rhythm2.   }
  \item{plot}{Logical. TRUE for plotting. FALSE for not plotting.}
}
 
\value{A list. The first element of the list is the series of the mean value along the rhythms. 
The second element of the list shows the total mean on each rhythm. }

\author{Yi Ya}

\examples{

data1<-abs(rnorm(100,0,1))
data2<-rep(NA,100)
data<-sample(c(data1,data2),200,replace=FALSE)
Res<-Steps.analysis(ID="200 Abs Normal with Missing",Tag="Rhythm1=7, Rhythm2=5, Rhythm3=3, 
Rhythm4=2",S=data,Rhythm1=7,Rhythm2=5,Rhythm3=3,Rhythm4=2,Rhythm5=NULL,Rhythm6=NULL,
Rhythm7=NULL,Start="3+1",plot=TRUE) 
 
}

