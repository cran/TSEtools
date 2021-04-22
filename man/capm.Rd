\name{capm}
\alias{capm}

\title{
Capital assets pricing model including a risk-free asset.
}
\description{
Compute the capital assets pricing model including a risk-free asset.
}
\usage{
capm(x, Rf = 0.2/270, sh = FALSE, eRtn = NULL)
}
\arguments{
  \item{x }{a numeric matrix of random returns per unit of price within some holding
period.

}
  \item{Rf }{the return of the risk free, i.e. has variance 0. 
}
  \item{sh }{a logical indicating whether shortsales on the risky securities are allowed. Default is FALSE.
}
  \item{eRtn}{a value of expected returen of portofilo. The mean of whole data defualt.}
}
\details{
Let \eqn{\xi_1 , \ldots,\xi_n} be random asset returns and \eqn{w_1 , \ldots, w_n} the portfolio weights. The expected returns are \eqn{r_m = E\xi_m , m = 1, \ldots, n.} In addition
to these risky investments, there is a risk-free asset (a bond or bank account)
available, which has return \eqn{r_0}. Denoting the weights of \eqn{w_0} for the risk-free
asset. The return of portfolio given by \deqn{R_p = w^t r} where, \eqn{r = (r_1, \ldots, r_n)^t}.

Risk is measure by a deviation functional \eqn{\Sigma}. It is a variance-covariance of asset returns. The risk-free component \eqn{w_0} ignore in the objective. So, the standard deviation of portfolio is given by \eqn{\sigma_p = w^t \Sigma w.} 

To obtain the optimum value of \eqn{w_i, i = 1,\ldots, n,} we solve the following model:
\deqn{\min w^t \Sigma w,\;\;s.t:\;\; w^t r + w_0 r_0 > \mu \;\; and \;\;\sum w_i + w_0 = 1}
Note that, the portfolio weights may be negative (selling short is allowed). Market portfolio is named \code{MP} where, the risk free weight \code{w_0} is zero (see, the function of \code{ prtf()}).

For any portfolio \eqn{p},
\deqn{E(R_p) = r_0 + \beta(p) (r_{MP} - r_0)}
where, \eqn{r_{MP}} is return of market portfolio and \eqn{\beta(p)} is the beta coefficient of the portfolio \eqn{p}. It is given by \eqn{\beta(p) = Cov( r_{MP}, r_p )/ SD(r_{MP}).} 
}
\value{
\item{wCAPM }{ weight of CAPM assets}
\item{wrF }{ weight of risk free assets}
\item{sd.capm }{ volatility of CAPM portfolio}
\item{rtn.capm }{ return of CAPM portfolio}
\item{beta }{ beta coefficient of portfolio}
}
\references{
Pflug and Romisch (2007, ISBN: 9789812707406)
}

\examples{
\dontrun{
x <- rnorm(500,0.05,0.02)
y <- rnorm(500,0.01,0.03)
z<-cbind(x, y)
colnames(z) <- c("prt1","prt2")

capm( z, sh = FALSE, Rf= 0.2/270, eRtn=0.02 )
}
}