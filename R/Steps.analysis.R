Steps.analysis<-function(ID,Tag,S,Rhythm1,Rhythm2,Rhythm3,Rhythm4,Rhythm5,Rhythm6,Rhythm7,Start,plot) {

    Rhythms<-c(Rhythm1, Rhythm2, Rhythm3, Rhythm4, Rhythm5, Rhythm6, Rhythm7)
    IDS<-c("Rhythm1","Rhythm2","Rhythm3","Rhythm4","Rhythm5","Rhythm6","Rhythm7")[1:length(Rhythms)]
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
    return(list(Steps=LL,Rhythms.mean=Rhythms.mean))
}




