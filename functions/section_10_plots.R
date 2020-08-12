##############################################################################
# Purpose: POINT Dashboard -> SECTION 10: MEDICATION DIARY
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################
percent_ci <- function(df, name, var, outcome="Yes", time_ind=0){
  df <- subset(df, time == time_ind)
  df <- df[!is.na(df$totalopioiddose), ]
  # pci <- percent_ci(df, "Male", "T1_sex", outcome="Male")
  df <- as.data.frame(df)
  tmp <- df[,var]
  
  # Calculate the p and se
  observed <- sum(df[, var] == outcome, na.rm=T)
  n <- length(tmp)
  
  # calculate the proportion
  p <- ( observed / n) * 100
  
  # standard error = proportion of outcome 1 * proportion of outcome 2 / n
  se <- sqrt(p * (100 - p) / n)
  
  ## 2. Create % (95% CI)
  # return result
  estimate <- round(p, 2)
  lower <- round(p - (1.96*se), 2)
  upper <- round(p + (1.96*se), 2)
  
  # conditionals for small numbers
  if (lower < 0){ lower <- 0 }
  if (upper > 100){ upper <- 100 }
  # print results
  result <- c(estimate, lower, upper, time_ind, observed, n)
  #result <- paste0(estimate, " (", lower, "-", upper, ")")
  
  # return results
  return(result)
}

proportion_make <- function(df, variable, outcome = "yes"){
  ##############################################################################
  t0 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=0)
  t1 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=1)
  t2 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=2)
  t3 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=3)
  t4 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=4)
  t5 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=5)
  
  tall <- as.data.frame(rbind(t0,t1,t2,t3,t4,t5))
  names(tall)[names(tall) == "V1"] <- 'Proportion'
  names(tall)[names(tall) == "V2"] <- 'Lower CI'
  names(tall)[names(tall) == "V3"] <- 'Upper CI'
  names(tall)[names(tall) == "V4"] <- 'Time'
  names(tall)[names(tall) == "V5"] <- 'Users'
  names(tall)[names(tall) == "V6"] <- 'Diaries'
  
  # reorder so time is at the front
  tall <- tall[,c(4,1,2,3,5,6)]
  ##############################################################################
  return(tall)
}

proportion_plot <- function(df, variable, outcome = "yes"){
  ##############################################################################
  t0 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=0)
  t1 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=1)
  t2 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=2)
  t3 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=3)
  t4 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=4)
  t5 <- percent_ci(df, variable, variable, outcome=outcome, time_ind=5)
  
  tall <- as.data.frame(rbind(t0,t1,t2,t3,t4,t5))
  names(tall)[names(tall) == "V1"] <- 'estimate'
  names(tall)[names(tall) == "V2"] <- 'lower'
  names(tall)[names(tall) == "V3"] <- 'upper'
  names(tall)[names(tall) == "V4"] <- 'time'
  ##############################################################################
  y_label <- paste0("Proportion of ", variable," Users")
  p <- ggplot(data=tall, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=1))+
    #geom_line(size = 8, colour="black",) +
    #geom_df(size=1, color='blue') +
    geom_ribbon(alpha=0.43, fill='blue') +
    geom_line(size=2, color='blue') +
    labs(title="",
         x = "Time",
         y = y_label) + theme_bw()
return(p)
}


ome_plot <- function(df, variable){
  require(dplyr)
  tmp <- df %>% select(time, totalopioiddose, all_of(variable))
  names(tmp)[names(tmp) == variable] <- 'drug'
  tmp[,3] <- as.integer(tmp[,3])-1
  # subset for only those drug users 
  tmp2 <- subset(tmp, drug == 1)
  tmp2 <- tmp2[complete.cases(tmp2),]
  
  # calculate the mean ome and plot it 
  t0 <- mean(tmp2$totalopioiddose[which(tmp2$time == 0)], na.rm=T)
  t1 <- mean(tmp2$totalopioiddose[which(tmp2$time == 1)], na.rm=T)
  t2 <- mean(tmp2$totalopioiddose[which(tmp2$time == 2)], na.rm=T)
  t3 <- mean(tmp2$totalopioiddose[which(tmp2$time == 3)], na.rm=T)
  t4 <- mean(tmp2$totalopioiddose[which(tmp2$time == 4)], na.rm=T)
  t5 <- mean(tmp2$totalopioiddose[which(tmp2$time == 5)], na.rm=T)
  
  time <- c(0:5)
  means <- c(t0,t1,t2,t3,t4,t5)
  
  results <- data.frame(Time = time, OME = means)
  
  # subset by drug
  ##############################################################################
  y_label <- paste0("Mean oral morphine equivalent for \n", variable," users across time")
  p <- ggplot(data=results, aes(x=Time, y=OME))+
    geom_line(size=2, color='red') +
    labs(title="",
         x = "Time",
         y = y_label) + theme_bw()
  return(p)
}

ome_summary <- function(df, variable){
  require(dplyr)
  tmp <- df %>% select(time, totalopioiddose, all_of(variable))
  names(tmp)[names(tmp) == variable] <- 'drug'
  
  tmp[,3] <- as.integer(tmp[,3])-1
  # subset for only those drug users 
  tmp2 <- subset(tmp, drug == 1)
  tmp2 <- tmp2[complete.cases(tmp2),]
  
  # calculate the mean ome and plot it 
  t0 <- mean(tmp2$totalopioiddose[which(tmp2$time == 0)], na.rm=T)
  t1 <- mean(tmp2$totalopioiddose[which(tmp2$time == 1)], na.rm=T)
  t2 <- mean(tmp2$totalopioiddose[which(tmp2$time == 2)], na.rm=T)
  t3 <- mean(tmp2$totalopioiddose[which(tmp2$time == 3)], na.rm=T)
  t4 <- mean(tmp2$totalopioiddose[which(tmp2$time == 4)], na.rm=T)
  t5 <- mean(tmp2$totalopioiddose[which(tmp2$time == 5)], na.rm=T)
  
  time <- c(0:5)
  means <- c(t0,t1,t2,t3,t4,t5)
  
  results <- data.frame(Time = time, OME = means)
  return(results)
}
##############################################################################
#################################### END #####################################
##############################################################################