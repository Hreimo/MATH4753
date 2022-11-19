
#' @title Overbooking
#'
#' @param N The number of seats on the plane
#' @param gamma The probability of overbooking
#' @param p The probability of someone showing up
#' @importFrom stats 'pbinom' 'qbinom' 'qnorm'
#' @importFrom graphics 'layout.show'
#' @return
#' The number of tickets that should be sold
#' for both the discrete and continuous case given p
#' and gamma. Returns a discrete and continuous,
#' normal approximated plot of objective function vs
#' the number of tickets sold, where the objective
#' function describes how overbook the plane is. If
#' the objective function is equal to 0, then the
#' number of tickets at that value is the number of
#' tickets that minimizes overbooking.
#'
#' @export
#'
#' @examples \dontrun{ntickets(200,0.02,0.95)}
#'

ntickets=function(N=400,gamma=0.02,p=0.95){
  n=N
  while(N>qbinom(1-gamma,n,p)){
    n=n+1

  }
  nd=n
  ndstart=N
  ndend=N
  while(1-gamma-pbinom(N,ndend,p)<(1-gamma-0.001)){
    ndend=ndend+1
  }




  n=N

  while(N+0.5>qnorm(1-gamma,n*p,sqrt(n*p*(1-p)))){
    n=n+0.000001
  }
  nc = n
  nd = round(nc+0.5,0)

  ncstart = N
  ncend = N
  while((1-gamma-pnorm(ncstart+0.5,ncend*p,sqrt(ncend*p*(1-p)))<(1-gamma-0.001))){
    ncend=ncend+0.0001
  }




  layout(matrix(c(1,2),ncol=1))
  plot(c(ndstart:nd,(nd),(nd+1):ndend),c((1-gamma-pbinom(N,c(ndstart:nd),p)),1-gamma-pbinom(N,nd,p),1-gamma-pbinom(N,c((nd+1):ndend),p)),type ="b"
  ,main = paste("Objective vs n, the optimal number of tickets to be sold", "\n(",nd,")", "gamma =", gamma,"N=",N, "discrete")
  , xlab = "n", ylab = "Objective") + polygon(c(nd,nd),c(-0.5,1)) + polygon(c(ndstart-5,ndend+5),c(0,0))
  plot(c(ncstart:ncend),1-gamma-pnorm(N+0.5,c(ncstart:ncend)*p,sqrt(c(ncstart:ncend)*p*(1-p))),type = "l",
  main = paste("Objective vs n, the optimal number of tickets to be sold", "\n(",nc,")","gamma =", gamma,"N=",N, "cont")
  , xlab = "n", ylab = "Objective") + polygon(c(nc,nc,nc),c(-0.5,(1-gamma-pnorm(N+0.5,nc*p,sqrt(nc*p*(1-p)))),1)
  ) + polygon(c(ncstart-5,ncend+5),c(0,0))






 list(nd=nd,nc=nc, N=N,p=p, gamma=gamma)
}



