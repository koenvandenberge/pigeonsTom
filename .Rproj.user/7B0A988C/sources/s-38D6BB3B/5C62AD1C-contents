source("R/readUitslag.R")

files <- c("Quievrain 14 jun 2020"="data/duivenTom/2020/20200614_quievrain_tollembeek_jonge.txt",
          "Noyon 21 jun 2020"="data/duivenTom/2020/20200621_noyon_tollembeek_jonge.txt")

datList <- list()
for(nn in 1:length(files)){
  curRes <- readUitslag(files[nn])
  curRes$race <- names(files)[nn]
  datList[[nn]] <- curRes
}
res <- do.call(rbind, datList)

#res <- readUitslag(file)
