\name{MHDA.plot}
\alias{MHDA.plot}
\title{Plotting Results from Massive Hierarchically Data Analysis}
\description{
    This function plots results from Massive Hierarchically Data Analysis.
} 
\usage{
MHDA.plot(data,plot.type,Class,ID,Category.I,Category.II,Slot)
}
\arguments{
  \item{data}{ List. Result object from MHDA function. }
  \item{plot.type}{ Character. "line" for line plot for Unit. "pie" for pie plot for Category I and Category II. }
  \item{Class}{ Character. "Unit", "Category.I", or "Category.II".}
  \item{ID}{Character. A level of the category of argument Class.}
  \item{Category.I}{ Character Strings. Partial or full levels of Category I according to the object assigned to arugument "data". }
  \item{Category.II}{ Character Strings. Partial or full levels of Category II according to the object assigned to argument "data". }
  \item{Slot}{ Character Strings. Names of slots. Line plot only shows results for the first single slot. }
}

\author{Yarong Yang and Jacob Zhang}

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
Unit="Store-1",Category.I="Mall_c",Category.II=Class.II,Slot=c("2021-01","2021-02"))

MHDA.plot(data=Res,plot.type="line",Class="Unit",ID="Store-1",Category.I=Class.I,
Category.II=Class.II,Slot="2021-01")

Res.2<-MHDA(Data=Store.sales,data.infor=NULL,type="Count",is.binary=FALSE,Unit=NULL,
Category.I="Mall_c",Category.II=Class.II,Slot=c("2021-01","2021-02"))

MHDA.plot(data=Res.2,plot.type="pie",Class="Category.I",ID="Mall_c",
Category.I=Class.I,Category.II=Class.II,Slot="2021-02")

}

