source("R/readUitslag.R")
library(lubridate)

files <- c("Quievrain 14 jun 2020"="data/duivenTom/2020/20200614_quievrain_tollembeek_jonge.txt",
          "Noyon 21 jun 2020"="data/duivenTom/2020/20200621_noyon_tollembeek_jonge.txt",
          "Noyon 28 jun 2020"="data/duivenTom/2020/20200628_noyon_tollembeek_jonge.txt")
lostijd <- c(as.POSIXct("14062020 08:00:00", format = "%d%m%Y %H:%M:%S", tz="GMT"), 
             as.POSIXct("21062020 07:30:00", format = "%d%m%Y %H:%M:%S", tz="GMT"),
             as.POSIXct("28062020 10:30:00", format = "%d%m%Y %H:%M:%S", tz="GMT"))
aantalDuiven <- c("Quievrain 14 jun 2020"=418,
                  "Noyon 21 jun 2020"=444,
                  "Noyon 28 jun 2020"=669)

# create one big df
datList <- list()
for(nn in 1:length(files)){
  curRes <- readUitslag(files[nn])
  curRes$race <- names(files)[nn]
  curRes$lostijd <- lostijd[nn]
  curRes$aantalDuiven <- aantalDuiven[nn]
  # get time in right format
  curTime <- format(curRes$Constat)
  if(max(nchar(curTime)) == 8){
    curTime <- paste0(substr(curTime,1,2),":",
                      substr(curTime,4,5),":",
                      substr(curTime,6,8))
  } else if(max(nchar(curTime)) == 7){
    curTime <- paste0(substr(curTime,1,1),":",
                      substr(curTime,3,4),":",
                      substr(curTime,5,7))
  }
  curTime <- paste(date(lostijd[nn]), curTime, sep=" ")
  curTime <- as.POSIXct(curTime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
  curRes$tijdIk <- curTime
  # get distance right: needs to happen within each race.
  uniqNaam <- unique(curRes$Naam)
  afstandDf <- data.frame(Naam = unique(curRes$Naam),
                          afstand = sapply(uniqNaam, function(name) max(curRes$Afstand[curRes$Naam == name])))
  curRes$afstandIk <- afstandDf$afstand[match(curRes$Naam, afstandDf$Naam)]
  datList[[nn]] <- curRes
}
res <- do.call(rbind, datList)
res$race <- factor(res$race, levels=names(files))


