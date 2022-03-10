MHDA<-function(Data,data.infor,type,is.binary,Unit,Category.I,Category.II,Slot){
   
    if(is.null(Data)!=TRUE) {
        DAT<-Data
        SLOTS<-names(DAT)
        DATA<-data.frame(C1=names(DAT[[1]])[1:(length(DAT[[1]])-2)],C2=DAT[[1]][[length(DAT[[1]])-1]],C3=DAT[[1]][[length(DAT[[1]])]])
    }

    if(is.null(data.infor)!=TRUE) DATA<-get(load(data.infor))

    if(is.null(Unit)!=TRUE) {
        data<-list(length(Slot))
        for(i in 1:length(Slot)) {
            if(is.null(Data)!=TRUE) {
                SALES.temp<-DAT[[which(SLOTS==Slot[i])]]
                SALES<-SALES.temp[[Unit]]
            }
            if(is.null(data.infor)!=TRUE) {
                if(file.exists(Slot[i])!=TRUE) {
                    stop("The Slot folder is not under the current working directory.")
                } else {
                    SALES<-get(load(paste(Slot[i],"/",Unit,"@",Slot[i],".rdata",sep="")))
                }
            }
            sales<-matrix(0,nrow=length(SALES),ncol=2)
            for(j in 1:length(SALES)) {
                sales.temp<-SALES[[j]]
                sales.temp<-sales.temp[!is.na(sales.temp)]
                ix<-1:length(sales.temp)
                if(is.binary==TRUE) ix<-which(names(sales.temp)==1)
                sales[j,]<-c(sum(sales.temp[ix]),length(ix))
            }
            rownames(sales)<-names(SALES)
            colnames(sales)<-c("Amount","Count")
            data[[i]]<-sales
        }
        names(data)<-Slot
        return(new("Res.mhda.1",Obj.a.unit=data,is.binary=is.binary))
    } else {
        data<-matrix(ncol=length(Slot),nrow=nrow(DATA))
        for(i in 1:nrow(DATA)) {
            for(j in 1:length(Slot)) {
                if(is.null(Data)!=TRUE) {
                     SALES.temp<-DAT[[which(SLOTS==Slot[j])]]
                     SALES<-SALES.temp[[DATA[i,1]]]
                }
                if(is.null(data.infor)!=TRUE) {
                     if(file.exists(Slot[j])!=TRUE) {
                          stop("The Slot folder is not under the current working directory.")
                     } else {
                          SALES<-get(load(paste(Slot[j],"/",DATA[i,1],"@",Slot[j],".rdata",sep="")))
                     }
                }
                dat<-SALES
                sales<-0
                for(k in 1:length(dat)) {
                     dat.temp<-dat[[k]]
                     dat.temp<-dat.temp[!is.na(dat.temp)]
                     ix<-1:length(dat.temp)
                     if(is.binary==TRUE) ix<-which(names(dat.temp)==1)
                     if(type=="Value") t<-sum(dat.temp[ix]) else t<-length(ix)
                     sales<-sales+t
                }
                data[i,j]<-sales
            }
            if(i%%100==0) cat("# of units:",i,"\n")
        }
        colnames(data)<-Slot
        rownames(data)<-DATA[,1]

        if(is.null(Category.I)!=TRUE&is.null(Category.II)!=TRUE) {
            data.category.I.II<-list(length(Slot))
            for(i in 1:length(Slot)) {
                dat<-matrix(0,ncol=length(Category.II),nrow=length(Category.I))
                colnames(dat)<-Category.II
                rownames(dat)<-Category.I
                for(j in 1:length(Category.I)){
                     for(k in 1:length(Category.II)) {
                          ix<-which(DATA[,2]==Category.I[j]&DATA[,3]==Category.II[k])
                          if(length(ix)!=0) dat[j,k]<-sum(data[ix,i])
                     }
                } 
                data.category.I.II[[i]]<-dat            
            }
            names(data.category.I.II)<-Slot
            data.category<-data.category.I.II
        }
        if(is.null(Category.I)!=TRUE&is.null(Category.II)==TRUE) {
            CL<-Category.I
            data.category.I<-list(length(Slot))
            for(i in 1:length(Slot)) {
                data.category.I[[i]]<-matrix(ncol=1,nrow=length(CL)) 
                rownames(data.category.I[[i]])<-CL
                colnames(data.category.I[[i]])<-"whole.II"
            }
            names(data.category.I)<-Slot              
            for(j in 1:length(Slot)) {
                dat<-matrix(data[,Slot[j]],ncol=1)
                for(i in 1:length(CL)) {
                    ix<-which(DATA[,2]==CL[i])
                    datt<-matrix(dat[ix,],ncol=1)
                    data.category.I[[j]][i,]<-colSums(datt)
                }
            }
            data.category<-data.category.I
        }
        if(is.null(Category.II)!=TRUE&is.null(Category.I)==TRUE) {
            CL<-Category.II
            data.category.II<-list(length(Slot))
            for(i in 1:length(Slot)) {
                data.category.II[[i]]<-matrix(nrow=1,ncol=length(CL))
                colnames(data.category.II[[i]])<-CL
                rownames(data.category.II[[i]])<-"whole.I"
            }
            names(data.category.II)<-Slot
            for(j in 1:length(Slot)) {
                dat<-matrix(data[,Slot[j]],ncol=1)
                for(i in 1:length(CL)) {
                    ix<-which(DATA[,3]==CL[i])
                    datt<-matrix(dat[ix,],ncol=1)
                    data.category.II[[j]][,i]<-colSums(datt)
                }    
            }
            data.category<-data.category.II
        }

        return(new("Res.mhda.2",Obj.all.units=data,Obj.category=data.category,type=type,is.binary=is.binary))
    }
}



