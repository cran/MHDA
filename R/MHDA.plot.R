MHDA.plot<-function(data,plot.type,Class,ID,Category.I,Category.II,Slot) {

     plot.line<-function(S,AT,ID,Tag) {
          plot(S,ylim=c(min(S)-IQR(S),max(S)+IQR(S)),main=paste(ID,":",round(sum(S),2),sep=""),xaxt="n",ylab=Tag,
               xlab=paste("Max:",round(max(S),2),"--","Min:",round(min(S),2),sep=""),type="l",lwd=2)
          points(1:length(S),S,col="red",pch=19)
          ix<-which(S==max(S))
          ixx<-which(S==min(S))
          points(c(ix,ixx),c(S[ix],S[ixx]),col="blue",pch=19)
          axis(1,at=1:length(S),labels=AT,las=2)
     }

     plot.pie<-function(S,AT,ID,Tag) {
          Colors<-c("purple","violetred1","green3","red3","cyan","blue1","orange","deepskyblue","yellow","red1","blue3","green1") 
          percent<-round(S/sum(S),4)  
          AT<-paste(AT," ",percent*100,"%",sep="")
          names(S)<-AT
          ix<-which(S!=0)
          pie(S[ix],col=Colors[1:length(S)][ix],main=paste(ID,": ",round(sum(S),2)),xlab=Tag)
     }

     if(plot.type=="line") {
          par.new<-par(mfrow=c(2,1))
          on.exit(par(par.new))
          pt<-ifelse(data@is.binary==TRUE,"+","")
          Slot<-Slot[1]
          plot.line(S=data@Obj.a.unit[[Slot]][,1],AT=rownames(data@Obj.a.unit[[Slot]]),ID=ID,Tag=paste(Slot," ",pt,sep=""))
          plot.line(S=data@Obj.a.unit[[Slot]][,2],AT=rownames(data@Obj.a.unit[[Slot]]),ID=ID,Tag=paste(Slot," ",pt,sep=""))
     }

     if(plot.type=="pie") {
          pt<-ifelse(data@is.binary==TRUE,"+","")
          if(is(data,"Res.mhda.2")) pt<-paste(pt,",",data@type,sep="")
          dd<-0
          ps<-Slot[1]
          for(i in 1:length(Slot)) {
               data.temp<-data@Obj.category[[Slot[i]]]
               if(Class=="Category.I") {
                      at<-Category.II
                      if(ID=="whole.I") dat<-matrix(colSums(data.temp[,at]),nrow=1)
                      if(ID!="whole.I") dat<-matrix(data.temp[ID,at],nrow=1)
                      rownames(dat)<-ID
                      colnames(dat)<-colnames(data.temp[,at])
               }
               if(Class=="Category.II") {
                      at<-Category.I
                      if(ID=="whole.II") dat<-matrix(rowSums(data.temp[at,]),ncol=1)
                      if(ID!="whole.II") dat<-matrix(data.temp[at,ID],ncol=1)
                      colnames(dat)<-ID
                      rownames(dat)<-rownames(data.temp[at,])
               } 
               dd<-dd+dat
               if(i<length(Slot)) ps<-paste(ps,"&",Slot[i+1],sep="")
          }
          plot.pie(S=dd,AT=at,ID=ID,Tag=paste(ps," ",pt,sep=""))
     }
}

