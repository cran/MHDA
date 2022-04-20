Steps.analysis<-function(ID,Tag,S,Rhythms,Start,plot,pick.plot) {

    IDS<-paste("Rhythm",1:length(Rhythms),sep="")
    IDS.new<-vector()
    which.Rhythm<-which(1:length(Rhythms) %in% substr(Start,start=1,stop=1))
    IDS.new[1]<-IDS[which.Rhythm]
    which.step<-which(1:max(Rhythms) %in% substr(Start,start=3,stop=3))

    LL<-vector()
    Steps.at<-1
    steps<-Rhythms[which.Rhythm]-which.step+1
    k<-0
    while(Steps.at<(length(S)+1)&(Steps.at+steps)<(length(S)+1)){
         k<-k+1
         temp<-S[Steps.at:(Steps.at+steps-1)]
         LL[k]<-mean(temp[!is.na(temp)])
         which.Rhythm<-ifelse((which.Rhythm+1)%%length(Rhythms)==0,length(Rhythms),(which.Rhythm+1)%%length(Rhythms))
         IDS.new[k+1]<-IDS[which.Rhythm]
         Steps.at<-Steps.at+steps
         steps<-Rhythms[which.Rhythm]
    }
    LL<-c(LL,mean(S[Steps.at:length(S)]))
    names(LL)<-IDS.new
    Rhythms.mean<-vector()
    for(i in 1:length(IDS)) {
         ix<-which(IDS.new==IDS[i])
         temp<-LL[ix]
         Rhythms.mean[i]<-mean(temp[!is.na(temp)])
    }
    names(Rhythms.mean)<-IDS
    if(plot==TRUE) {
         Cols<-rep(1:length(Rhythms),length.out=length(LL))    
         plot(LL,col=Cols,xlab=Tag,ylab="",type="n",main=ID)      
         legend("topleft",legend=IDS.new[1:length(Rhythms)],col=levels(factor(Cols)),lwd=5)
         for(i in 1:length(LL)) lines(x=c(i,i),y=c(0,LL[i]),col=Cols[i],lwd=5)
         for(j in 1:length(Rhythms)) {
               ix<-which(names(Rhythms.mean)==IDS.new[j])
               abline(h=Rhythms.mean[ix],lty=2,lwd=2,col=levels(factor(Cols))[j])
         }
    }
    if(!is.null(pick.plot)) {
         LL.pick<-NULL
         Cols.pick<-NULL
         Cols.t<-vector()
         P<-vector()
         for(i in 1:length(pick.plot)) {
               LL.t<-LL[which(IDS.new==paste("Rhythm",pick.plot[i],sep=""))]
               LL.pick<-c(LL.pick,LL.t)
               col.t<-which(IDS.new==paste("Rhythm",pick.plot[i],sep=""))[1]
               Cols.pick<-c(Cols.pick,rep(col.t,length(LL.t)))
               P[i]<-length(LL.t)
               Cols.t[i]<-col.t
         }    
         plot(LL.pick,col=Cols.pick,xlab=Tag,ylab="",xaxt="n",type="n",main="")
         legend("topleft",legend=paste("Rhythm",pick.plot,sep=""),col=Cols.t,lwd=5)
         for(i in 1:length(LL.pick)) lines(x=c(i,i),y=c(0,LL.pick[i]),col=Cols.pick[i],lwd=5)
         for(j in 1:length(pick.plot)) {
               lines(x=c(sum(P[1:j])+0.5,sum(P[1:j])+0.5),y=c(0,max(LL.pick[!is.na(LL.pick)])),col=1,lwd=1,lty=2)
               axis(1,at=sum(P[1:j]),labels=P[j])
               ix<-which(names(Rhythms.mean)==paste("Rhythm",pick.plot[j],sep=""))
               lines(x=c(sum(P[1:j])+0.5-P[j],sum(P[1:j])+0.5),y=c(Rhythms.mean[ix],Rhythms.mean[ix]),col=Cols.t[j],lwd=2,lty=2)
               if(j!=1) text(x=sum(P[1:j])+0.5-P[j]/2,y=max(LL.pick[!is.na(LL.pick)]),labels=round(Rhythms.mean[ix],2))
               if(j==1) text(x=0,y=max(LL.pick[!is.na(LL.pick)])/2,labels=round(Rhythms.mean[ix],2))
         }
    }

    return(list(Steps=LL,Rhythms.mean=Rhythms.mean))
}




