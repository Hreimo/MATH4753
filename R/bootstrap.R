#' @title Bootstrap estimation
#'
#' @param iter the number of samples created from the data set
#' @param x the data set of interest
#' @param fun The parameter/function of interest. Mean by default
#' @param alpha The parameter used to find the confidence interval of interest
#' @param cx The size of the plotting text relative the base value of 1
#' @param ... Any additional comments to be added to the title
#'
#' @return A histogram of Bootstrap sample statistics
#' @export
#' @importFrom graphics 'abline' 'segments'
#' @importFrom stats 'quantile'
#'
#' @examples \dontrun{myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...)}
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){

  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para = hist(xstat,freq=FALSE,las=1,
              main = paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""), ...)

  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))
}
