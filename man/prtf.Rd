\name{prtf}
\alias{prtf}
\title{
Design the Portfolio of assets
}
\description{
Compute the efficient frontier function for some selected risk functionals in a  portfolio optimization setting.
}
\usage{
prtf (x, Rf = 0.0, sh = FALSE ,eRtn = NULL)
}
\arguments{
  \item{x }{a numeric matrix of random returns per unit of price within some holding
period.
}
\item{sh }{a logical indicating whether shortsales on the risky securities are allowed. Default is FALSE.}

\item{Rf }{the return of the risk free, i.e. has variance 0.} 

\item{eRtn }{a value of expected returen of portofilo. The mean of whole data defualt.}
}
\details{
Let \eqn{\xi_1 , \ldots,\xi_n} be random asset returns and \eqn{w_1 , \ldots, w_n} the portfolio
weights. The expected returns are \eqn{r_m = E\xi_m , m = 1, \ldots, n.} In addition
to these risky investments, there is a risk-free asset (a bond or bank account)
available, which has return \eqn{r_0}. Denoting the weights of \eqn{w_0} for the risk-free
asset. The return of portfolio given by \deqn{ R_p = w^t r } where, \eqn{r = ( r_1, \ldots, r_n)^t}.

Risk is measure by a deviation functional \eqn{\Sigma}. It is a variance-covariance of asset returns. The risk-free component \eqn{w_0} ignore in the objective. So, the standard deviation of portfolio is given by \eqn{\sigma_p = w^t \Sigma w.} 

To obtain the optimum value of \eqn{w_i, i=1,\ldots, n,} we solve the following model:
\deqn{\min w^t  \Sigma  w\;\; s.t:\;\;w^t  r  > \mu \;\; and\;\; \sum w_i = 1.}
where, \eqn{ \mu } is a constant value.
Note that, the portfolio weights may be negative (selling short is allowed).
}
\value{
The minimum weights show with \code{MIN} which is the portfolio with the minimum volatility. Market portfolio is given by \code{MP} where, the risk free weight \code{w_0} is zero. \code{MP} is the tangency point between the market line and efficient frountier curve. A list containing the following components:
\item{prt}{list the name of assests in the portfolio}
\item{obs.p }{return and volatiliy of overall portfolio}
\item{vol }{volatility of portfolio}
\item{rtn }{return of portfolio}
\item{w }{weigths of portfolio}
}
\references{
Pflug and Romisch (2007, ISBN: 9789812707406)
}

\seealso{
\href{https://CRAN.R-project.org/package=portfolio.optimization}{portfolio.optimization},
\href{ https://CRAN.R-project.org/package=tseries}{portfolio.optim}
}
\examples{
\dontrun{
x <- rnorm(500,0.05,0.02)
y <- rnorm(500,0.01,0.03)
z<-cbind(x, y)
colnames(z) <- c("prt1","prt2")

prtf (z, sh = FALSE)
}
}
