source("R/readAll.R")

## get all his pigeons
tomID <- grep(x=res$Naam, pattern = "Van Den Berge Tom")
uniqPigeonsTom <- res$Ring[tomID]

## calculate speed
# calcSpeed <- function(res){
#   tomID <- grep(x=res$Naam, pattern = "Van Den Berge Tom")
#   afstandTom <- res$Afstand[tomID[1]]
#   minutenGevlogen <- (res$Constat[tomID] - 8) * 100
#   ## seconden naar fractie
#   minutenGevlogen <- round(minutenGevlogen) + ((minutenGevlogen - round(minutenGevlogen)) / 60 * 100)
#   snelheid <- afstandTom / minutenGevlogen
#   dfSpeed <- data.frame(duif=factor(res$Ring[tomID]),
#                         snelheid=snelheid)
#   return(dfSpeed)
# }
# dfSnel <- calcSpeed(res)
# dfSnel$race <- factor("Quievrain 14 jun 2020")

## calculate speed
addSpeed <- function(res){
  afstand <- res$Afstand
  minutenGevlogen <- (res$Constat - 8) * 100
  ## seconden naar fractie
  minutenGevlogen <- round(minutenGevlogen) + ((minutenGevlogen - round(minutenGevlogen)) / 60 * 100)
  snelheid <- afstand / minutenGevlogen
  res$snelheidIk <- snelheid
  return(res)
}
dfSnel <- addSpeed(res)


