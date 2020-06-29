source("R/readAll.R")

## calculate speed
addSpeed <- function(res){
  afstand <- res$afstandIk
  minutenGevlogen <- (res$tijdIk - res$lostijd)
  ## seconden naar fractie
  snelheid <- afstand / as.numeric(minutenGevlogen)
  res$snelheidIk <- snelheid
  return(res)
}
dfSnel <- addSpeed(res)

dfTom <- dfSnel[grep(x=dfSnel$Naam, pattern="Van Den Berge Tom"),]
dfTom$duif <- as.factor(dfTom$Ring)
dfTom$coef <- (dfTom$Nr / dfTom$aantalDuiven)*100