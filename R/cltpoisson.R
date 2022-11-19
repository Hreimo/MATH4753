#' @title Central Limit Theorem for Poisson
#'
#' @param n The number of observation in a sample
#' @param iter The number of random samples
#' @param lambda The parameter of the poisson
#' @param ... A dynamic parameter that can take the value of any specified parameter.
#' @importFrom stats 'dpois' 'rpois'
#' @return
#' A histogram showing the density of sample means taken
#' from iter number of samples with n number of observations
#' per sample. A barplot with the relative frequncies of y
#' values  vs the value A plot of probability of the y value
#' vs the y value.
#' @export
#'
#' @examples \dontrun{mycltp(n=10,iter=10000, lambda = 10)}
mycltp=function(n=10,iter=10000,lambda=10,...){

  ## r-random sample from the Poisson
  y=rpois(n*iter,lambda=lambda)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax

  ## Make a suitable layout for graphing
  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))

  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)
  ## add a density curve made from the sample distribution
  #lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

  # Now make a new plot
  # Since y is discrete we should use a barplot
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x=0:max(y)
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}

mycltp(n=10,iter=10000)
