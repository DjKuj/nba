library(rvest)
library(lubridate)
library(stringr)

main <- function(){
  writeOdd(importOdd())
}

importOdd <- function(){
  
  setwd("~/Documents/GitHub/nba_R")
  html <- read_html("https://www.enligne.parionssport.fdj.fr/paris-basketball/usa/nba")
  
  table <- html_node(html, "#tab_gameevent")
  table <- gsub("\t", "",html_text(table))
  table <- strsplit(table,split="\n")[[1]]
  
  games <- table[str_detect(table, " - ") == TRUE][-1]
  games <- strsplit(games,split=" - ")
  odd <- table[str_detect(table, ",") == TRUE]
  nbGames <- length(games)
  
  finalObject <- matrix(rep("", nbGames), nrow = nbGames, ncol = 7)
  
  for(i in seq(1,nbGames,1)){
      finalObject[i,3] <- odd[3*i-2]
      finalObject[i,4] <- odd[3*i-1]
      finalObject[i,5] <- odd[3*i]
    for(j in seq(1,2,1)){
      finalObject[i,j] <- games[[i]][j]
    }
  }
  
  for(k in seq(1,nbGames,1)){
    url <- paste0("https://www.enligne.parionssport.fdj.fr/paris-basketball/usa/nba/",
                  gsub(" ", "-",tolower(finalObject[k,1])),
                  "-vs-",
                  gsub(" ", "-",tolower(finalObject[k,2])))
    html <- read_html(url)
    html <- gsub("\t", "",html_text(html))
    html <- strsplit(html,split="\n")[[1]]
    ligne <- which(html == "    FACE à FACE - MATCH")[1]
    odd1 <- str_sub(html[ligne+14], -4)
    odd2 <- str_sub(html[ligne+18], -4)
    finalObject[k,6] <- odd1
    finalObject[k,7] <- odd2
  }
  
  finalObjectCSV <- as.data.frame(finalObject)
  names(finalObjectCSV) <- c("Home", "Away", "1N2H", "1N2N", "1N2A", "12H", "12A")
  write.csv2(finalObjectCSV, paste0('odds/', gsub("-", "", today()), ".csv"))
  return(finalObject)
}

writeOdd <- function(odd){
  corresp <-  matrix(c("LA LAKERS", "GOLDENSTATE", "CLEVELAND", "SAN ANTONIO", "NEW YORK", "CHICAGO", "OKLAHOMA", "LA CLIPPERS", "MIAMI", "ATLANTA", "BOSTON", "HOUSTON", "PHILADELPHIE", "NEW ORLEANS","WASHINGTON", "DETROIT", "DALLAS", "TORONTO", "BROOKLYN", "MILWAUKEE", "PORTLAND", "PHOENIX", "INDIANA", "MEMPHIS", "UTAH", "CHARLOTTE", "DENVER", "SACRAMENTO", "ORLANDO","MINNESOTA","Los Angeles Lakers", "Golden State Warriors", "Cleveland Cavaliers", "San Antonio Spurs", "New York Knicks", "Chicago Bulls", "Oklahoma City Thunder", "Los Angeles Clippers", "Miami Heat", "Atlanta Hawks", "Boston Celtics", "Houston Rockets", "Philadelphia 76ers", "New Orleans Pelicans", "Washington Wizards", "Detroit Pistons", "Dallas Mavericks", "Toronto Raptors", "Brooklyn Nets", "Milwaukee Bucks", "Portland Trail blazers", "Phoenix Suns", "Indiana Pacers", "Memphis Grizzlies", "Utah Jazz", "Charlotte Hornets", "Denver Nuggets", "Sacramento Kings", "Orlando Magic", "Minnesota Timberwolves"), 30,2)
  
  today <- gsub("-", "", today())
  dataset <- read.csv2("data/datasetInit2.csv", stringsAsFactors = FALSE)
  nbGames <- dim(odd)[1]
  
  for(i in seq(1, nbGames, 1)){
    correspHomeName <- corresp[which(corresp == as.character(odd[i,1])),2]
    correspAwayName <- corresp[which(corresp == as.character(odd[i,2])),2]
    
    dataset$Odd1N2[dataset$Team == correspHomeName & dataset$Date == as.numeric(today)] <- odd[i,3]
    dataset$Odd12[dataset$Team == correspHomeName & dataset$Date == as.numeric(today)] <- odd[i,6]
    dataset$Odd1N2[dataset$Team == correspAwayName & dataset$Date == as.numeric(today)] <- odd[i,5]
    dataset$Odd12[dataset$Team == correspAwayName & dataset$Date == as.numeric(today)] <- odd[i,7]
  }
  
  write.csv2(dataset, 'data/datasetInit2.csv')
  return(dataset)
}