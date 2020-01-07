getTSE<-function(file,symbols=NA){
  z <- list()
asset <- function(s, file1) {
  if (file.exists(file1)) {
    temp <- NULL
    temp <- read.csv(file1, sep = ",", header = TRUE)
    temp <- temp[-c(1, 7, 9, 10, 11)]
    colnames(temp) <- c("Date", "Open", "High", "Low", 
                        "Close", "Volume", "Last")
    assign(x = s, value = temp)
    date <- as.Date(as.character(get(s)$Date), "%Y%m%d")
    assign(x = s, value = xts(get(s)[-1], order.by = date))
    return(get(s))
  }
  else {
    cat(s, "dataset does not exist!", 
        "\n")
  }
}
url <- as.character("http://www.tsetmc.com/tsev2/data/Export-txt.aspx?t=i&a=1&b=")
sName <- read.csv(file, sep = ",", header = TRUE)
ifelse(is.na(symbols), S <- as.vector(sName$symbol), S <- symbols)
S <- S[!duplicated(S)]
folder <- tempdir()
folder0 <- paste0(folder, "/temp/")
if (file.exists(folder0)) 
  unlink(folder0, recursive = TRUE)
dir.create(folder0)
temp <- temp1 <- file0 <- NULL
for (s in S) {
  cat("proceeding...\n")
  file0 <- tempfile(pattern = "", tmpdir = folder0)
  file1 <- paste0(file0, ".csv")
  temp <- paste0(url, as.character(sName$code[sName$symbol == 
                                                s]))
  if (!identical(temp, character(0)) && temp != url) {
    tryCatch({
      download.file(temp, file1, mode = "wb", quiet = TRUE)
      cat(s, "compeleted! \n")
      temp3 <- asset(s, file1)
      assign(as.character(s), temp3, parent.frame())
      cat("Last observations:\n")
      print(last(get(s)))
      cat("\n")
    }, warning = function(w) {
      print("URL is not responding! Try it after sometimes.")
    }, error = function(e) {
      print("URL is not responding! Try it after sometimes.")
    })
  }
  else {
    cat("** ASSET of", s, "DOES NOT EXIST!**", 
        "\n")
    no <- match(s, S)
    S <- S[-no]
  }
}
z$assets <- S
unlink(folder0, recursive = TRUE)
class(z) <- "getTSE"
invisible(z)
}