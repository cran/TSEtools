prtf<-function( x, Rf=0.0, sh = FALSE, eRtn=NULL ){
  z<-list()
  z$prt<-colnames(x)
  n.assets<-length(z$prt)
  mean.ret<- colMeans(x,na.rm=T)
  
  if(is.null(eRtn)){eRtn<-mean(mean.ret)}
  
  rcov<-as.matrix(cov(x,use="pairwise"))
  cov.inv <- solve(rcov)
  
  maxret<-max(mean.ret)
  minret<-min(mean.ret)
  rets<-seq(minret,maxret,len=500)
  
  if(sh==TRUE){
    
    sd.p<-0
    ret.p<-0
    dvec <- rep(0,ncol(t(mean.ret)))
    Am <- cbind(rep(1,ncol(t(mean.ret))),as.numeric(t(mean.ret)))
    for(k in 1:1000){
      bvec <- c(1,rets[k])
      tryCatch(
        wp <- solve.QP(2*(rcov), dvec=dvec, Amat=Am,
                       bvec=bvec, meq=1,factorized = F)$solution
        , error=function(e){
          wp <- NA
        })
      wp<-as.vector(wp)
      sd.p[k]<-sqrt(t(wp)%*%(rcov) %*%(wp))
      ret.p[k]<-t(wp)%*%as.matrix(mean.ret)
    }
    rv.p_sh<-data.frame(sd.p,ret.p)
    
    # 
    bvec <- 1
    Am.min <- cbind(rep(1,ncol(t(mean.ret))))
    tryCatch(
      w.min<- solve.QP(2*(rcov), dvec=dvec, Amat=Am.min, bvec=bvec,meq=1,factorized=F)$solution
      , error=function(e){
        w.min<-NA
      })
    
    names(w.min)<-z$prt
    ret.min <- w.min%*%as.matrix(mean.ret)
    sd.min <-sqrt(w.min%*%rcov%*%w.min)
    
    bvec.mp <- c(1,eRtn)
    
    tryCatch(
      w.mp<-solve.QP(2*(rcov), dvec=dvec, Amat=Am,
                     bvec=bvec.mp, meq=1,factorized=F)$solution
      , error=function(e){
        w.mp<-NA
      })
    names(w.mp)<-z$prt
    ret.mp <- w.mp%*%as.matrix(mean.ret)
    sd.mp <-sqrt(w.mp%*%rcov%*%w.mp)
    #####
  }else{
    sd.p<-0
    ret.p<-0
    dvec <- rep(0,ncol(t(mean.ret)))
    Am <- cbind(rep(1,ncol(t(mean.ret)))
                ,as.numeric(t(mean.ret)),diag(1,nrow = ncol(t(mean.ret))))
    
    for(k in 1:1000){
      bvec <- c(1,rets[k],rep(0,ncol(t(mean.ret))))
      tryCatch(
        wp <- solve.QP(2*(rcov), dvec=dvec, Amat=Am,
                       bvec=bvec, meq=1,factorized = F)$solution
        , error=function(e){
          wp <- NA
        })
      wp<-as.vector(wp)
      sd.p[k]<-sqrt(t(wp)%*%(rcov) %*%(wp))
      ret.p[k]<-t(wp)%*%as.matrix(mean.ret)
    }
    rv.p_sh<-data.frame(sd.p,ret.p)
    
  
    bvec.min <- c(1,rep(0,ncol(t(mean.ret))))
    Am.min <- cbind(rep(1,ncol(t(mean.ret)))
                    ,diag(1,nrow = ncol(t(mean.ret))))
    tryCatch(
      w.min<-solve.QP(2*(rcov), dvec=dvec, Amat=Am.min, bvec=bvec.min,meq=1,factorized=F)$solution
      , error=function(e){
        w.min<-NA
      })
    
    
    names(w.min)<-z$prt
    ret.min <- w.min%*%as.matrix(mean.ret)
    sd.min <-sqrt(w.min%*%rcov%*%w.min)
    ###
    bvec.mp <- c(1,eRtn,rep(0,ncol(t(mean.ret))))
    
    tryCatch(
      w.mp<-solve.QP(2*(rcov), dvec=dvec, Amat=Am,
                     bvec=bvec.mp, meq=1,factorized=F)$solution
      , error=function(e){
        w.mp<-NA
      })
    names(w.mp)<-z$prt
    ret.mp <- w.mp%*%as.matrix(mean.ret)
    sd.mp <-sqrt(w.mp%*%rcov%*%w.mp)
  }
  
  z$obs.p<-rv.p_sh
  z$vol<-round(data.frame(sd.min,sd.mp),4)
  names(z$vol)<-c("MIN","MP")
  z$rtn<-round(data.frame(ret.min, ret.mp),4)
  names(z$rtn)<-c("MIN","MP")
  z$w<-round(data.frame(w.min, w.mp),4)
  names(z$w)<-c("MIN","MP")
  
  return(z)
  class(z)<-"prtf"
  invisible(z)
}
