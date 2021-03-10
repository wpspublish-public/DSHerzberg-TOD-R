urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/TOD-R/master/INPUT-FILES/"
fileName_path   <- "TODC-TODS2.23.21forceilingrecodes.csv"

input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path),
  "rb"
)))

con <- url("http://www.imdb.com/title/tt1490017/", "rb") 

"https://raw.githubusercontent.com/DSHerzberg/TOD-R/master/INPUT-FILES/TODC-TODS2.23.21forceilingrecodes.csv"


input <- suppressMessages(read_csv(url(
  "https://raw.githubusercontent.com/DSHerzberg/TOD-R/master/INPUT-FILES/TODC-TODS2.23.21forceilingrecodes.csv"
)))
