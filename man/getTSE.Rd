\name{getTSE}
\alias{getTSE}

\title{
	Download Historical Dataset from Tehran Stock Exchange (TSE)
}
\description{
	getTSE function is an easy way to download and organize the historical dataset from websites of \href{http://new.tse.ir/en/}{TSE} and \href{http://www.tsetmc.com/Loader.aspx?ParTree=15}{www.tsetmc.com}. Since, the raw data are not recorded in a standard format, we provide the function for organize the data in user friendly way. Daily data updated after 13:30 UTC. The market is closed on Friday and Wensday and national holidays.
}
\usage{
	getTSE(file, symbols=NA)
}
\arguments{
	\item{file}{
		the path and name of external file which include the name of symbol and company and a specific code. The sample file (\code{sample.csv}) is available on packages (see, example). Given that randomly selected the companies. To get main file, please contact with maintainer.
	}
	\item{symbols}{
		a vector of character given the name of assets to download. The name of assets has to be included in the first column of \code{file}. The default value is NA which consider all assets at source file to download.
	}
}
\details{
	The list of symbols or assets can be named, and the list names will be used as names for the variables. Component data is an array with \CRANpkg{xts} time series object. The name for dimensions are: Open, High, Low, Close, Volume, Last. The value of symbols are appear in \code{Global Environment}.

}
\value{
	The output is an object of the \dQuote{assets} name which get all the name of assets list to download.
}
\examples{
	fpath<-system.file("extdata", "sample.csv", package="TSEtools")
	assets0<-getTSE(fpath)$assets
}
