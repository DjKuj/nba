#Libraries
library(lubridate)
library(caret)
library(reshape)
library(optparse)
library(doParallel)
library(foreach)

################  MAIN FUNCTION  #################
oddPrediction <- function(dateStart, dateEnd, everyDay){
  
  trainingFreq <- 10
  
  dataset <- importDataset('data/datasetInit2.csv')
  
  elo <- NULL
  homeAway <- NULL
  rest <- NULL
  predictions <- NULL
  count <- 0
  
  for(date in as.integer(gsub("-", "", as.character(seq(ymd(dateStart), ymd(dateEnd), 1))))){
    print(date)
    if(dim(dataset[dataset$Date == date,])[1] != 0){
    eloDay <- eloProbability(dataset, date)
    homeAwayDay <- HWProbability(dataset, date)
    restDay <- restProbability(dataset, date)
    
    dataset <- updateFile1(dataset, date, eloDay, homeAwayDay, restDay)
    
    if(everyDay == TRUE){
    neuralNetworkDay <- trainDay(dataset, date)
    }
    else{
      if((date - dateStart) %% trainingFreq == 0){
        neuralNetworkDay <- trainDay(dataset, date)
      }
    }
    
    predictionsDay <- predictDay(dataset, date, neuralNetworkDay)
    predictionsDay[predictionsDay > 1] <- 1
    predictionsDay[predictionsDay < 0] <- 0

    dataset <- updateFile2(dataset, date, predictionsDay)

    elo <- c(eloDay, elo)
    homeAway <- c(homeAwayDay, homeAway)
    rest <- c(restDay, rest)
    predictions <- c(predictionsDay, predictions)
    
    count <- count + 1
    if(count == 10){
      write.csv2(dataset, 'autosave.csv')
      print("Fichier enregistre ")
      count = 0
    }
    else{}
    }
    else{}
  
  }
  
  write.csv2(dataset, 'autosave.csv')
  print("Fichier enregistre ")
  return(dataset)
}

############### IMPORT DATASET  #################
importDataset <- function(dataset){
  #!!!!!!!FACTORS
  output <- read.csv2(dataset, stringsAsFactors = FALSE)
  output$eloRanking <- as.numeric(output$eloRanking)
  output$prevEloRanking <- as.numeric(output$prevEloRanking)
  output$Odd12 <- as.numeric(output$Odd12)
  output$ProbaCD <- as.numeric(output$ProbaCD)
  output$ProbaDE <- as.numeric(output$ProbaDE)
  output$ProbaRest <- as.numeric(output$ProbaRest)
  output$VD <- as.numeric(output$VD)
  
  return(output)
}

############ ELO PROBABILITY ####################
eloProbability <- function(dataset, date){
  
  alpha <- c(2.202, 2.085, 1.995, 1.889, 1.800, 1.023, 1, 1, 1, 1)
  
  currentDataset <- dataset[dataset$Date == date,]
  pastDataset <- dataset[dataset$Date < date,]
  lengthCDS <- dim(currentDataset)[1]
  output <- NULL
  for(i in seq(lengthCDS, 2, -2)){
    currentTeamHome <- currentDataset$Team[i]
    currentTeamAway <- currentDataset$Team[i-1]
    bufferHome <- as.data.frame(matrix(nrow = 10, ncol = 4))
    names(bufferHome) <- c("indexTeam","indexOpponent", "eloOpponent", "percScore")
    bufferAway <- bufferHome
    
    bufferHome$indexTeam <- which(pastDataset$Team == currentTeamHome)[1:10]
    bufferHome$indexOpponent <- bufferHome$indexTeam - ((bufferHome$indexTeam +1) %% 2) + (bufferHome$indexTeam %% 2)
    bufferHome$eloOpponent <- pastDataset$prevEloRanking[bufferHome$indexOpponent]
    bufferHome$percScore <- pastDataset$Score[bufferHome$indexTeam]/(pastDataset$Score[bufferHome$indexTeam]+pastDataset$Score[bufferHome$indexOpponent])
    indEloHome <- sum(alpha*bufferHome$eloOpponent*bufferHome$percScore)/(pastDataset$eloRanking[which(pastDataset$Team == currentTeamAway)[1]]*sum(alpha))
    
    bufferAway$indexTeam <- which(pastDataset$Team == currentTeamAway)[1:10]
    bufferAway$indexOpponent <- bufferAway$indexTeam - ((bufferAway$indexTeam +1) %% 2) + (bufferAway$indexTeam %% 2)
    bufferAway$eloOpponent <- pastDataset$prevEloRanking[bufferAway$indexOpponent]
    bufferAway$percScore <- pastDataset$Score[bufferAway$indexTeam]/(pastDataset$Score[bufferAway$indexTeam]+pastDataset$Score[bufferAway$indexOpponent])
    indEloAway <- sum(alpha*bufferAway$eloOpponent*bufferAway$percScore)/(pastDataset$eloRanking[which(pastDataset$Team == currentTeamHome)[1]]*sum(alpha))
    
    diffCD <- indEloHome - indEloAway
    
    if(diffCD >= 0){
      probEloHome <- eloFunction(diffCD)
      probEloAway <- 1 - probEloHome}
    else{
      probEloAway <- eloFunction(-diffCD)
      probEloHome <- 1 - probEloAway
    }
  output <- c(probEloAway, probEloHome, output)
  }
  return(output)
}

################# ELO FUNCTION ####################
eloFunction <- function(x){
  output <- 0.004*x^6 + 1618.7*x^5 - 0.0007*x^4 - 98.115*x^3 - 10^(-8)*x^2 + 4.3177*x + 0.5
  return(output)
}

################ HOME AWAY PROBABILITY ############
HWProbability <- function(dataset, date){
  
  betas <- c(8.66, 6.55)
  season <- dataset$Saison[dataset$Date == date][1]
  
  currentDataset <- dataset[dataset$Date == date,]
  pastDataset <- dataset[dataset$Date < date,]
  pastDatasetHome <- pastDataset[-seq(1,dim(pastDataset)[1],2),]
  pastDatasetAway <- pastDataset[-seq(2,dim(pastDataset)[1],2),]
  lengthCDS <- dim(currentDataset)[1]
  output <- NULL
  
  for(i in seq(lengthCDS, 2, -2)){
    currentTeamHome <- currentDataset$Team[i]
    currentTeamAway <- currentDataset$Team[i-1]
    
    Home_nbGamesHome <- length(which(pastDatasetHome$Team == currentTeamHome & pastDatasetHome$Saison == season))
    Away_nbGamesAway <- length(which(pastDatasetAway$Team == currentTeamAway & pastDatasetAway$Saison == season))
    
    if(Home_nbGamesHome >= 5){
      winningPercHome <- sum(pastDatasetHome$VD[pastDatasetHome$Team == currentTeamHome & pastDatasetHome$Saison == season])/Home_nbGamesHome
    }
    else{
      Home_nbGamesHome <- length(which(pastDatasetHome$Team == currentTeamHome & pastDatasetHome$Saison == season - 1))
      winningPercHome <- sum(pastDatasetHome$VD[pastDatasetHome$Team == currentTeamHome & pastDatasetHome$Saison == season - 1])/Home_nbGamesHome
    }
    
    if(Away_nbGamesAway >= 5){
      winningPercAway <- sum(pastDatasetAway$VD[pastDatasetAway$Team == currentTeamAway & pastDatasetAway$Saison == season])/Away_nbGamesAway
    }
    else{
      Away_nbGamesAway <- length(which(pastDatasetAway$Team == currentTeamAway & pastDatasetAway$Saison == season - 1))
      winningPercAway <- sum(pastDatasetAway$VD[pastDatasetAway$Team == currentTeamAway & pastDatasetAway$Saison == season - 1])/Away_nbGamesAway
    }
    
    probaCDHome_num <- betas[1]*winningPercHome + betas[2]*(1-winningPercAway)
    probaCDAway_num <- betas[1]*(1-winningPercHome) + betas[2]*winningPercAway
    
    probaCDHome <- probaCDHome_num/(probaCDHome_num+probaCDAway_num)
    probaCDAway <- probaCDAway_num/(probaCDHome_num+probaCDAway_num)
    
    output <- c(probaCDAway, probaCDHome, output)
  }
  return(output)
}

##############  REST PROBABILITY  #######################
restProbability <- function(dataset, date){
  
  gammas <- c(2.54, 1.65, 3.24)
  
  currentDataset <- dataset[dataset$Date == date,]
  pastDataset <- dataset[dataset$Date < date,]
  lengthCDS <- dim(currentDataset)[1]
  dateYMD <- ymd(date)
  output <- NULL
  
  matrixCons <- matrix(c(0.5,0.41,0.35,0.59,0.5,0.41,0.65,0.59,0.5), nrow = 3, ncol = 3)
  matrix7days <- matrix(c(0.5,0.4219,0.4048,0.5781,0.5,0.4018,0.5952,0.5982,0.5), nrow = 3, ncol = 3)
  matrixRestDays <- matrix(c(0.5,0.5956,0.5819,0.5254,0.4044,0.5,0.5188,0.5475,0.4181,0.4812,0.5,0.4804,0.4746,0.4525,0.5196,0.5), nrow = 4, ncol = 4)

  for(i in seq(lengthCDS, 2, -2)){
    currentTeamHome <- currentDataset$Team[i]
    currentTeamAway <- currentDataset$Team[i-1]

    consecutiveGamesHome <- dim(pastDataset[pastDataset$Team == currentTeamHome & pastDataset$Date == as.integer(gsub("-", "", as.character(dateYMD - 1))),])[1]+
      dim(pastDataset[pastDataset$Team == currentTeamHome & pastDataset$Date == as.integer(gsub("-", "", as.character(dateYMD - 1))),])[1]*
      dim(pastDataset[pastDataset$Team == currentTeamHome & pastDataset$Date == as.integer(gsub("-", "", as.character(dateYMD - 2))),])[1]
    
    consecutiveGamesAway <- dim(pastDataset[pastDataset$Team == currentTeamAway & pastDataset$Date == as.integer(gsub("-", "", as.character(dateYMD - 1))),])[1]+
                            dim(pastDataset[pastDataset$Team == currentTeamAway & pastDataset$Date == as.integer(gsub("-", "", as.character(dateYMD - 1))),])[1]*
                            dim(pastDataset[pastDataset$Team == currentTeamAway & pastDataset$Date == as.integer(gsub("-", "", as.character(dateYMD - 2))),])[1]
    
    restingDaysHome <- dateYMD - 1 - ymd(pastDataset$Date[which(pastDataset$Team == currentTeamHome)[1]])
    restingDaysAway <- dateYMD - 1 - ymd(pastDataset$Date[which(pastDataset$Team == currentTeamAway)[1]])
    
    nbGames7Home <- dim(pastDataset[pastDataset$Team == currentTeamHome & pastDataset$Date >= as.integer(gsub("-", "", as.character(dateYMD - 7))),])[1]
    nbGames7Away <- dim(pastDataset[pastDataset$Team == currentTeamAway & pastDataset$Date >= as.integer(gsub("-", "", as.character(dateYMD - 7))),])[1]
    
    pourcConsHome <- matrixCons[consecutiveGamesHome + 1, consecutiveGamesAway + 1]
    pourcConsAway <- matrixCons[consecutiveGamesAway + 1, consecutiveGamesHome + 1]
    
    if(restingDaysHome > 3 || restingDaysAway > 3){
      pourcRestHome <- 0.5
      pourcRestAway <- 0.5
      }
    else{
      pourcRestHome <- matrixRestDays[restingDaysHome + 1, restingDaysAway + 1]
      pourcRestAway <- matrixRestDays[restingDaysAway + 1, restingDaysHome + 1]
    }
    
    if(nbGames7Home > 4 || nbGames7Home < 2 || nbGames7Away > 4 || nbGames7Away < 2){
      pourc7Home <- 0.5
      pourc7Away <- 0.5
    }
    else{
      pourc7Home <- matrix7days[nbGames7Home - 1, nbGames7Away - 1]
      pourc7Away <- matrix7days[nbGames7Away - 1, nbGames7Home - 1]
    }
    
    restProbabilityHomeNum <- gammas[1]*pourcRestHome + gammas[2]*pourcConsHome + gammas[3]*pourc7Home
    restProbabilityAwayNum <- gammas[1]*pourcRestAway + gammas[2]*pourcConsAway + gammas[3]*pourc7Away
    
    restProbabilityHome <- restProbabilityHomeNum/(restProbabilityHomeNum+restProbabilityAwayNum)
    restProbabilityAway <- restProbabilityAwayNum/(restProbabilityHomeNum+restProbabilityAwayNum)
    
    output <- c(restProbabilityAway, restProbabilityHome, output)
  }

  return(output)
}

#################  UPDATE FILE 1 #####################
updateFile1 <- function(dataset, date, eloDay, homeAwayDay, restDay){
  dateFirst <- which(dataset$Date == date)[1]
  dateLast <- which(dataset$Date == date)[length(which(dataset$Date == date))]
  
  dataset$ProbaCD[dateFirst:dateLast] <- eloDay
  dataset$ProbaDE[dateFirst:dateLast] <- homeAwayDay
  dataset$ProbaRest[dateFirst:dateLast] <- restDay
  
  return(dataset)
}

#################  UPDATE FILE 2 #####################
updateFile2 <- function(dataset, date, predictionsDay){
  dateFirst <- which(dataset$Date == date)[1]
  dateLast <- which(dataset$Date == date)[length(which(dataset$Date == date))]
  
  l = 1
  for(j in seq(dateFirst, dateLast)){
    if(is.na(dataset$Odd12[j])){
      l <-  l + 1
    }
    else{
      dataset$winningPerc[j] <- predictionsDay[l]
      l <- l + 1}
  }
  
  return(dataset)
}

############ NEURAL NETWORK TRAINING #################
trainDay <- function(dataset, date){

  size <- 3
  decay <- 0.01
  maxit <- 10000
  repeats <- 50
  nbGenNN <- 5
  
  dataset$Odd12 <- as.numeric(dataset$Odd12)
  dataset$ProbaCD <- as.numeric(dataset$ProbaCD)
  dataset$ProbaDE <- as.numeric(dataset$ProbaDE)
  dataset$ProbaRest <- as.numeric(dataset$ProbaRest)
  dataset$VD <- as.numeric(dataset$VD)
  
  dataset <- dataset[!is.na(dataset$ProbaCD),]
  dataset <- dataset[!is.na(dataset$ProbaDE),]
  dataset <- dataset[!is.na(dataset$ProbaRest),]
  dataset <- dataset[!is.na(dataset$Odd12),]
  dataset <- dataset[dataset$Odd12 != "",]
    
  dataset_train <- dataset[dataset$Periode == dataset$Periode[dataset$Date == date][1] & dataset$Date < date,c("Odd12", "ProbaCD", "ProbaDE", "ProbaRest")]
  output <- dataset[dataset$Periode == dataset$Periode[dataset$Date == date][1] & dataset$Date < date, "VD"]
  

  
  neuralnets <- list(0)
  for(i in seq(1, nbGenNN, 1)){
    neuralnets[[i]] <- avNNet(dataset_train, output,
                              size = size,
                              decay = decay,
                              maxit = maxit,
                              repeats = repeats,
                              trace = FALSE)
  }

  return(neuralnets)
}

###############  PREDICTION  ########################
predictDay <- function(dataset, date, NN){
  
  iter <- length(NN)
  datatest <- dataset[dataset$Date == date, c("Odd12", "ProbaCD", "ProbaDE", "ProbaRest")]
  
  datatest$Odd12 <- as.numeric(datatest$Odd12)
  datatest$ProbaCD <- as.numeric(datatest$ProbaCD)
  datatest$ProbaDE <- as.numeric(datatest$ProbaDE)
  datatest$ProbaRest <- as.numeric(datatest$ProbaRest)
  
  datatest <- datatest[!is.na(datatest$ProbaCD),]
  datatest <- datatest[!is.na(datatest$ProbaDE),]
  datatest <- datatest[!is.na(datatest$ProbaRest),]
  datatest <- datatest[!is.na(datatest$Odd12),]
  
  predictions <- NULL
  for(i in seq(1, iter, 1)){
    predictions <- cbind(predictions, predict(NN[[i]], datatest))
  }
  
  predictions <- apply(predictions, 1, mean)
  
  return(predictions)
}
