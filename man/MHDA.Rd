\name{MHDA}
\alias{MHDA}
\title{Massive Hierarchically Data Analysis}
\description{
    This function conducts Massive Hierarchically Data Analysis.
} 
\usage{
MHDA(Data,data.infor,type,is.binary,Unit,Category.I,Category.II,Slot)
}
\arguments{
  \item{Data}{ List. Each element of the list keeps data observations in one slot. Each unit in each slot has a series of data cells with
              each data cell keeping a vector of observations. When this argument is not NULL, argument data.infor is ignored. }
  \item{data.infor}{ Character String. When argument Data is NULL, a .rdata file name is assigned to this argument. The content of the 
                     .rdata file is a data.frame with three columns. The first column is the name vector of units. The second column shows the
                     levels of the units in Category I. The third column shows the levels of the units in Category II.}
  \item{type}{Character. "Value" for the value number of the observations. "Count" for counting the number of observations.}
  \item{is.binary}{ Logical. TRUE for binary "positive" observations. FALSE for all the observations. Binary identification is labeled as 
                    the name of an observation.}
  \item{Unit}{ Character String. ID of a unit in the first column of the data information matrix. 
               When Unit is not NULL, Category.I and Category.II are ignored.}
  \item{Category.I}{ Character Strings. Partial or full levels of the categories in the second column of the data informaion matrix. NULL to ignore this argument. }
  \item{Category.II}{ Character Strings. Partial or full levels of the categories in the third column of the data information matrix. NULL to ignore this argument.}
  \item{Slot}{ Character Strings. Names of the folders with each folder keeping data under a specific slot. For example, "2021-01" means 
              that the folder "2021-01" keeps data observations in slot "2021-01". Every unit has a .rdata data file in every slot. In each of these .rdata file,
              there are a list of data cells. In each data cell, there is a series of observations. }
}
 
\value{A list.}

\examples{

##generating a small data for example###
Slots<-c("2021-01","2021-02")
Units<-c("Store-1","Store-2","Store-3","Store-4")
Class.I<-c("Mall_1","Mall_2","Mall_3","Mall_a","Mall_b","Mall_c")
Class.II<-c("B&H","F&B","HOM","KID","LEI&ENT","RET-SHO-ACC","SPM&SER")
Infor.1<-c("Mall_2","HOM")
Infor.2<-c("Mall_c","B&H")
Infor.3<-c("Mall_2","KID")
Infor.4<-c("Mall_c","F&B")
Store.sales<-list()
Store.sales[[1]]<-Store.sales[[2]]<-list()
names(Store.sales)<-Slots
for(i in 1:2) {
    for(j in 1:4) {
        Store.sales[[i]][[j]]<-list()
        n<-sample(1:30,1)
        for(k in 1:n) {
            t<-Store.sales[[i]][[j]][[k]]<-abs(rnorm(sample(1:50,1),0,1))
            names(Store.sales[[i]][[j]][[k]])<-sample(c(0,1),length(t),replace=TRUE)
        }
        names(Store.sales[[i]][[j]])<-paste("s",1:n,sep="")
    }
    Store.sales[[i]][[4+1]]<-c(Infor.1[1],Infor.2[1],Infor.3[1],Infor.4[1])
    Store.sales[[i]][[4+2]]<-c(Infor.1[2],Infor.2[2],Infor.3[2],Infor.4[2])
    names(Store.sales[[i]])<-c(Units,"Level.I","Level.II")
}

Res<-MHDA(Data=Store.sales,data.infor=NULL,type="Value",is.binary=TRUE,
Unit="Store-1",Category.I="Mall_2",Category.II=Class.II,Slot="2021-01")

}

