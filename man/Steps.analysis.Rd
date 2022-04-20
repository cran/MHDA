\name{Steps.analysis}
\alias{Steps.analysis}
\title{Stepwise analysis of a series of numeric observations on multiple levels of rhythms}
\description{
    It's a function to conduct stepwise analysis on a series of numeric observations,
 specifically stepwisely computing the mean of observations on each rhythm. Missing observations are allowed. Maximally available for seven Rhythms.
}  
\usage{
Steps.analysis(ID, Tag, S, Rhythms, Start, plot, pick.plot)
}
\arguments{
  \item{ID}{ Character String. Label for the data. }
  \item{Tag}{ Character String. Label for the data. }
  \item{S}{ Numeric. A series of numeric numbers. Missing observations are allowed in the series.  }
  \item{Rhythms}{ Integer vector. Each element of the vector is the number of steps on the corresponding rhythm.
                  For example, Rhythms=c(7,5,3,2,4) means there are five rhythms with the number of steps on each being 7,5,3,2,4, respectively.  }
  \item{Start}{ Character String. Description for the initial number with format "a+b", where a and b are integers. 
                For example, Start="2+3" means that the initial observation in S is the third step in the second rhythm.   }
  \item{plot}{Logical. TRUE for stepwise plotting of the whole periodic rhythms. FALSE for not plotting.}
  \item{pick.plot}{ Integer vector. Each element of the vector is the index of a rhythms to be plotted isolatedly. 
                    For example, pick.plot=1 is meant to be plotting only the first rhythm and 
                    pick.plot=c(4,2) is meant to be plotting the fourth rhythm and then plotting the second rhythm. }
}
 
\value{A list. The first element of the list is the series of the mean values along the whole periodic rhythms. 
The second element of the list shows the total mean on each rhythm. }

\author{Yi Ya}

\examples{

data1<-abs(rnorm(150,0,1))
data2<-rep(NA,150)
data<-sample(c(data1,data2),300,replace=FALSE)
T<-paste("Rhythm",1:5,"=",c(7,5,3,2,4),sep="")
tag<-NULL
for(i in 1:length(T)) tag<-paste(tag,T[i])
Res<-Steps.analysis(ID="300 Abs Normal with Missing",Tag=tag,S=data,Rhythms=c(7,5,3,2,4),
Start="3+1",plot=TRUE,pick.plot=NULL) 
Res<-Steps.analysis(ID="300 Abs Normal with Missing",Tag=tag,S=data,Rhythms=c(7,5,3,2,4),
Start="3+1",plot=FALSE,pick.plot=c(3,5,2)) 

 
}

