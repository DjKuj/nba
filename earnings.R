earnings <- function(dataset){

percMin <- 0
percMax <- 0.161
ajust <- 0.025

data <- dataset[complete.cases(dataset),]

WL <- data$VD
predictions <- as.numeric(data$winningPerc)
oddPercentage <-1/as.numeric(data$Odd12)

data$bet <- rep(0, length(WL))
data$investment <- rep(0, length(WL))
data$earnings <- rep(0, length(WL))

data$bet[(predictions + ajust) > oddPercentage + percMin & (predictions + ajust) < oddPercentage + percMax] <- 1
data$investment <- kelly(1/(predictions + ajust), 1/oddPercentage)*data$bet*100
data$earnings <- data$investment*(1/oddPercentage*WL - 1)

results <- list("earnings" = sum(data$earnings),
                "investment" = sum(data$investment),
                "ROI" = sum(data$earnings)/sum(data$investment),
                "PercBet" = 2*sum(data$bet)/(length(data$bet)))
print(results)
return(data)
}

kelly <- function(a,b){
  (b/a - 1)/(b-1)
}

earnings2 <- function(dataset){
  
  percMin <- 0
  percMax <- 0.161
  
  data <- dataset[complete.cases(dataset),]
  
  results <- NULL
  WL <- data$VD
  predictions <- as.numeric(data$winningPerc)
  oddPercentage <-1/as.numeric(data$Odd12)
  
  for(i in seq(0,0.1,0.0025)){
  data$bet <- rep(0, length(WL))
  data$investment <- rep(0, length(WL))
  data$earnings <- rep(0, length(WL))
  
  data$bet[(predictions + i) > oddPercentage + percMin & (predictions + i) < oddPercentage + percMax] <- 1
  data$investment <- kelly(1/(predictions + i), 1/oddPercentage)*data$bet*100
  data$earnings <- data$investment*(1/oddPercentage*WL - 1)
  
  earnings <- sum(data$earnings)
  investment <- sum(data$investment)
  ROI <- sum(data$earnings)/sum(data$investment)
  nbBet <- sum(data$bet)
  percBet <- 2*sum(data$bet)/(length(data$bet))
  indicator <- ROI*percBet

  
  results <- rbind(results,c(i,
              earnings,
              investment,
              ROI,
              nbBet,
              percBet,
              indicator))
  results <- as.data.frame(results)
  }
  plot(x = results[,1], y = results[,7], type = "l", col = "red", xlab = "+Pourcent", ylab = "Indice", sub = paste(data$Saison, "-", data$Periode))
  return(results)
}