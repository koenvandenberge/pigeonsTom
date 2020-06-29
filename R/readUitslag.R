
readUitslag <- function(file){
  #res <- readLines(file)
  res <- read.fwf(file, skip=7, widths=c(4, 20, 10, 4, 3, 8, 3, 10,
                                         9, 10, 16),
                  stringsAsFactors=FALSE)
  colnames(res) <- c("Nr", "Naam", "Gemeente", "AD", "IG",
                     "Afstand", "LD", "Ring", "Constat",
                     "Snelheid", "Nr2")
  return(res)
}
