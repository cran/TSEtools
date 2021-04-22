\name{mcPrt}
\alias{mcPrt}
\title{
Matrix Conversion in Portfolio
}
\description{
mcPrt function compound the vector of assets in matrix form to using the multivariate data analysis. The assets must be included the items of \code{Open}, \code{Close}, \code{High}, \code{Low} and \code{Volumn}. The function also calculate the rate of return and Sharpe ratio for portfolio selection.
}
\usage{
mcPrt(asset, sub = "::", pstvRtn = FALSE, pr = "daily", Rf = 0.0)
}
\arguments{
  \item{asset}{
a character vector of symbols.
}
\item{sub}{
The character string in the form of "start date :: end date". The date format is "\%Y-\%M-\%D". Since, the data is time series format, the rules of \CRANpkg{xts} satisfied.
}
\item{pstvRtn}{
logical flag to determine, if the symbols with mean of return (see, details) should be positive/negative. Missing value is taken as false. By setting \code{pstvRtn=TRUE}, only a check the symbols that the mean of return is not negative and symbols with negative returns will be vanished from the list of calculation.
}
\item{pr}{
a character specifying the desired period time of return. Default value is daily. The values get "daily", "weekly" and "monthly".
}
\item{Rf}{
an integer value to give risk free.
}
}
\details{
The values of function contain Sharpe ratio and rate of return. Sharpe ratio is defined as:
\deqn{SR=\frac{\bar{R}-R_f}{\sigma_R}}
where \eqn{\bar{R}} is the mean asset return, \eqn{R_f} is the risk free rate of return and \eqn{\sigma_R} is the standard deviation (volatility) of the asset.

Let \eqn{X_t} is the closing price of the asset at time \eqn{t} and its value after a period of length \eqn{\Delta t}, say \eqn{X_{t+\Delta t}}. The return over that period is defined as:
\deqn{R_t=\frac{X_{t+\Delta t}}{X_t}-1}
Note that the return equation is equivalent to log return asset when the ratio value of the asset is close to 1 (see, Carmona 2003). To check the goodness of fit test on return value, maximum and minimum of prices, suggested to use the package of \CRANpkg{gnFit}.
}
\value{
The values of function involve \code{close}, \code{return}, \code{max}  and \code{min} as a matrix and xts format. The columns are a value of assets which is ordered by date. The mean return, volatility and Sharpe ratio are assigned in the \code{out}. The plot of return vs volatility and also the graph of Sharpe ratio are outputs of function.
}
\references{
Carmona (2003, ISBN:0387202862)
}
\examples{
\dontrun{
fpath<-system.file("extdata", "sample.dat", package="TSEtools")
getTSE(fpath)
rtn<-mcPrt(asset0, sub="2016::", pstvRtn=TRUE, pr="weekly")$return
}
}