urlRemote_path  <- "https://raw.githubusercontent.com/"
github_path <- "DSHerzberg/TOD-R/master/INPUT-FILES/"
fileName_path   <- "test1.csv"

input <- suppressMessages(read_csv(url(
  str_c(urlRemote_path, github_path, fileName_path)
)))

