#' @title Normal plot
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a x value
#' @importFrom stats 'pnorm' 'dnorm'
#' @importFrom graphics 'curve' 'polygon' 'text'
#'
#' @return A normal distribution with shaded area
#' @export
#'
#' @examples \dontrun{myncurve(10,5,6)}
myncurve = function(mu=10, sigma=5,a=6){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  curvex = seq(mu-3*sigma, a , length = 1000000)
  curvey = dnorm(curvex, mu, sigma)
  polygon(c(mu-3*sigma,curvex,a),c(
    dnorm(mu-3*sigma,mu,sigma),curvey,0), col =
  "Light Blue",)
  area =pnorm(a,mu,sigma)
  area = round(area,4)
  text(mu + 1*sigma,dnorm(a,mu,sigma), paste0("Area =", area))
}

utils::globalVariables('x')
