mcPrt<-function(asset,sub="::",pstvRtn=FALSE, pr="daily", Rf=0.0){
  z<-list()
  #  graphics.off()

  mRetn<-SD<-Sharp<-array(0)
  
  tryCatch({
    asset<-asset[!duplicated(asset)]
    if(length(asset)>1){
      m.close<-get(asset[1])$Close[sub]
      #m.return <-(diff(log(m.close),lag=L))
      ifelse(pr=="daily", m.return <-dailyReturn(m.close),ifelse(pr=="weekly", m.return <-weeklyReturn(m.close), 
                                                                 ifelse(pr=="monthly", m.return <-monthlyReturn(m.close))))
      
      m.max<-get(asset[1])$High[sub]
      m.min<-get(asset[1])$Low[sub]
      for(i in 2:length(asset)){
        tem.m.close<-get(asset[i])$Close[sub]
        #  tem.m.return <-(diff(log(tem.m.close),lag=L))
        ifelse(pr=="daily", tem.m.return <-dailyReturn(tem.m.close),ifelse(pr=="weekly", tem.m.return <-weeklyReturn(tem.m.close),
                                                                           ifelse(pr=="monthly", tem.m.return <-monthlyReturn(tem.m.close))))
        
        
        m.close<-merge(m.close,tem.m.close)
        m.return<-merge(m.return,tem.m.return)
        tem.m.max<-get(asset[i])$High[sub]
        m.max<-merge(m.max,tem.m.max)
        tem.m.min<-get(asset[i])$Low[sub]
        m.min<-merge(m.min,tem.m.min)
      }
    }
    else {
      m.close<-get(asset[1])$Close[sub]
      ifelse(pr=="daily", m.return <-dailyReturn(m.close), ifelse(pr=="weekly", m.return <-weeklyReturn(m.close),
                                                                  ifelse(pr=="monthly", m.return <-monthlyReturn(m.close))))
      #m.return <-(diff(log(m.close),lag=L))
      m.max<-get(asset[1])$High[sub]
      m.min<-get(asset[1])$Low[sub]
    }
    z$close<-m.close
    z$return<-m.return
    z$max<-m.max
    z$min<-m.min
    names(z$close)<-asset
    names(z$return)<-asset
    names(z$max)<-asset
    names(z$min)<-asset
    mRetn<-apply(z$return,2,function(x) mean(x,na.rm=TRUE))
    SD<-apply(z$return,2,function(x) sd(x,na.rm=TRUE))
    names(mRetn)<-asset
    SD<-SD
    names(SD)<-asset
    Sharp<-(mRetn-Rf)/SD
    if(length(asset)>1){
      if(isTRUE(pstvRtn)){
        SD<-SD[mRetn>0]
        Sharp<-Sharp[mRetn>0]
        mRetn<-mRetn[mRetn>0]
        s<-names(mRetn)
        S0<-names(z$return)
        
        no<-match(s, S0)
        z$return<-z$return[,no]
        asset<-s
      }
    }
    z$out<-data.frame(mRetn,SD,Sharp)
    row.names(z$out)<-asset
    colnames(z$out)<-c("meanReturn","volatility","sharpRatio" )
    cat("Values in % base ",pr,":\n")
    print(round(z$out*100,2))
    cat("\n")
    if(length(asset)>1){
      barplot(z$out[,3], names.arg=rownames(z$out),
              las=2, main="Sharpe Ratio",
              col ="steelblue", cex.lab = 1, cex.names=0.6,beside=TRUE)
      plot(z$out[,2]*100,z$out[,1]*100,
           xlab= "volatility",
           ylab= "return",ylim = c(min(z$out[,1]*100)-.5,max(z$out[,1]*100)+0.5))
      text(z$out[,2]*100,z$out[,1]*100,labels=rownames(z$out),cex=0.5,pos=3)
      
    }
    z$assets<-asset
},error=function(e){
  cat("The security prices do not exist! \n")
}
)
  
  class(z)<-"mcPrt"
  invisible(z)
}
