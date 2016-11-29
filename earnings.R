earnings <- function(dataset, ajust){

percMin <- 0
percMax <- 0.161

data <- dataset[complete.cases(dataset),]

WL <- data$VD
predictions <- as.numeric(data$winningPerc)
oddPercentage <-1/as.numeric(data$Odd12)

data$bet <- rep(0, length(WL))
data$investment <- rep(0, length(WL))
data$earnings <- rep(0, length(WL))

data$bet[(predictions + ajust) > oddPercentage + percMin & (predictions + ajust) < oddPercentage + percMax] <- 1
data$investment <- kelly(1/(predictions + ajust), 1/oddPercentage,0,0.5)*data$bet*100
data$earnings <- data$investment*(1/oddPercentage*WL - 1)

results <- list("earnings" = sum(data$earnings),
                "investment" = sum(data$investment),
                "ROI" = sum(data$earnings)/sum(data$investment),
                "PercBet" = 2*sum(data$bet)/(length(data$bet)))
print(results)
return(data)
}

kelly <- function(a,b,c,d){
  (b/a - 1)/(b-1)*c+d
}

earnings2 <- function(dataset){
  
  percMin <- 0
  percMax <- 0.161
  
  data <- dataset[complete.cases(dataset),]
  
  results <- NULL
  WL <- data$VD
  predictions <- as.numeric(data$winningPerc)
  oddPercentage <-1/as.numeric(data$Odd12)
  
  for(i in seq(0,0.05,0.0001)){
  data$bet <- rep(0, length(WL))
  data$investment <- rep(0, length(WL))
  data$earnings <- rep(0, length(WL))
  
  data$bet[(predictions + i) > oddPercentage + percMin & (predictions + i) < oddPercentage + percMax] <- 1
  data$investment <- kelly(1/(predictions + i), 1/oddPercentage,0.5,0.5)*data$bet*100
  data$earnings <- data$investment*(1/oddPercentage*WL - 1)
  
  
  earnings <- sum(data$earnings)
  investment <- sum(data$investment)
  ROI <- sum(data$earnings)/sum(data$investment)
  nbBet <- sum(data$bet)
  percBet <- 2*sum(data$bet)/(length(data$bet))
  precisionDenom <- length(which(data$earnings > 0))/nbBet
  precisionNum <- (sum(predictions[which(data$earnings > 0)]+i))/length(which(data$earnings > 0))
  precision <- precisionNum/precisionDenom
  indicatorPerc <- ROI*(percBet)^2*(1-abs(1-precision))
  indicatorROI <- sign(ROI)*ROI^2*percBet*(1-abs(1-precision))
  indicatorPrecision <- ROI*percBet/(abs(1-precision)^(1/10))

  
  results <- rbind(results,c(i,
              earnings,
              investment,
              ROI,
              nbBet,
              percBet,
              indicatorPerc,
              indicatorROI,
              indicatorPrecision,
              precision))
  results <- as.data.frame(results)
  names(results) <- c("+Pourcent", "earnings", "investment", "ROI", "nbBet", "percBet", "indicatorPerc", "indicatorROI", "indicatorPrecision", "precision")
  }
  results$indicatorPerc <- results$indicatorPerc/max(results$indicatorPerc)*sign(max(results$indicatorPerc))
  results$indicatorROI <- results$indicatorROI/max(results$indicatorROI)*sign(max(results$indicatorROI))
  results$indicatorPrecision <- results$indicatorPrecision/max(results$indicatorPrecision)*sign(max(results$indicatorPrecision))
  
  plot(x = results[,1], y = results[,7] + results[,8] + results[,9], type ="l", col = "red", xlab = "+Pourcent", ylab = "Indice")
  lines(x = results[,1], y = results[,7], lty = 2, col = "pink", xlab = "+Pourcent", ylab = "Indice", sub = paste(data$Saison, "-", data$Periode))
  lines(x = results[,1], y = results[,8], lty = 2, col = "blue")
  lines(x = results[,1], y = results[,9], lty = 2, col = "green")
  lines(x = results[,1], y = results[,7] + results[,8] + results[,9], type ="l", col = "red")
  lines(x = seq(0,10,10), y = rep(0,2), type ="l", col = "grey")
  legend(0, -0.5, legend=c("%bet", "ROI", "Precision", "Somme"),
         col=c("pink", "blue", "green", "red"), lty=c(2,2,2,1), cex=0.5)

  print(paste0("max indicator ROI = ", 100*results$'+Pourcent'[which(results$indicatorROI == max(results$indicatorROI))], "%"))
  print(paste0("max indicator Precision = ", 100*results$'+Pourcent'[which(results$indicatorPrecision == max(results$indicatorPrecision))], "%"))
  print(paste0("max indicator Perc = ", 100*results$'+Pourcent'[which(results$indicatorPerc == max(results$indicatorPerc))], "%"))
  return(results)
}

earnings3 <- function(dataset, ajust){
  
  percMin <- 0
  percMax <- 0.161
  
  data <- dataset[complete.cases(dataset),]
  
  WL <- data$VD
  predictions <- as.numeric(data$winningPerc)
  oddPercentage <-1/as.numeric(data$Odd12)
  
  data$bet <- rep(0, length(WL))
  data$investment <- rep(0, length(WL))
  data$earnings <- rep(0, length(WL))
  results <- NULL
  
  for(c in seq(0,1,0.05)){
    for(d in seq(0,0.5, 0.01)){
  data$bet[(predictions + ajust) > oddPercentage + percMin & (predictions + ajust) < oddPercentage + percMax] <- 1
  data$investment <- kelly(1/(predictions + ajust), 1/oddPercentage,c,d)*data$bet*100
  data$earnings <- data$investment*(1/oddPercentage*WL - 1)
  ROI <- sum(data$earnings)/sum(data$investment)
  results <- rbind(results,c(ROI,c,d, sum(data$investment),sum(data$earnings)))
    }
  }
  return(results)
}
