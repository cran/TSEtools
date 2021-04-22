capm<-function (x, Rf = 0.2/270, sh = FALSE, eRtn = NULL){
  z<-list()
  
  n.assets<-ncol(x)
  mRtn<-colMeans(x,na.rm = T)
  rtn_rf<-(mRtn-Rf)
  
  if(is.null(eRtn)){
    eRtn<-mean(mRtn)
  }
  rcov<-as.matrix(cov(x,use="pairwise"))
  cov.inv <- solve(rcov)
  
  one<-rep(1,n.assets)
  
  if(sh){
    
    dvec <- rep(0,ncol(t(mRtn)))
    Am <- cbind(as.numeric(t(mRtn)-Rf))
    
    bvec.mp <- eRtn-Rf
    
    wCAPM<-solve.QP(2*(rcov), dvec=dvec, Amat=Am,
                    bvec=bvec.mp, meq=0,factorized=F)$solution
    
    wmp<-prtf(x, sh= T, Rf=Rf, eRtn=eRtn)
    names(wCAPM)<-rownames(wmp$w[2])
    
    vmp<-(wmp$vol[2])
    rtnmp<-wmp$rtn[2]
    
    wrf<-1-sum(wCAPM)
    
  }else{
    #####
    
    dvec <- rep(0,ncol(t(mRtn)))
    Am <- cbind(diag(1,ncol(t(mRtn))),as.numeric(t(mRtn)-Rf))
    
    bvec.mp <- c(rep(0,ncol(t(mRtn))),eRtn-Rf)
    
    wCAPM<-solve.QP(2*(rcov), dvec=dvec, Amat=Am,
                    bvec=bvec.mp, meq=0,factorized=F)$solution
    
    wmp<-prtf(x, sh = F, Rf=Rf, eRtn=eRtn)
    names(wCAPM)<-rownames(wmp$w[2])
    
    vmp<-(wmp$vol[2])
    rtnmp<-wmp$rtn[2]
    
    wrf<-1-sum(wCAPM)
    
  }
  #------
  
  z$wCAPM<-round(wCAPM,5)

  z$wrF<-round(wrf,5)
  z$sd.capm<-as.numeric(sqrt(t(wCAPM)%*% rcov %*%(wCAPM)))
  z$rtn.capm<-as.numeric((wCAPM)%*%mRtn+Rf*wrf)
  
#  z$w.mp<-round(t(wmp$w[2]),5)
#  rownames(z$w.mp)<-NULL
  
#  z$sd.mp<-as.numeric(vmp)
#  z$rtn.mp<-as.numeric(rtnmp)
  
  z$beta<-as.numeric((eRtn-Rf)/(vmp^2*(sum(cov.inv%*% (rtn_rf)))))
  
  return(z)
  class(z)<-"capm"
  invisible(z)
}
