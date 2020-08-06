##############################################################################
# Purpose: POINT Dashboard -> SECTION 2: DEMOGRAPHICS
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
# PLOT 1: Density plot
density_plot <- function(df, variable){

  # create label
  y_label <- paste0("Density plot for ", variable)
  
  # get baseline data
  tmp <- subset(df, time == 0)
  
  # create plot
  p <- ggplot(data = tmp, aes(x = tmp[, variable])) + 
    geom_density(kernel = "gaussian", color="darkblue", fill="lightblue") +
    geom_vline(aes(xintercept=mean(tmp[, variable], na.rm=T)),
                 color="blue", linetype="dashed", size=1) +
    labs(title="",
         x = variable,
         y = y_label)
  
  return(p)
}
#=============================================================================
# PLOT 2: Donut
donut_plot <- function(df, variable){
  
  # create label
  y_label <- paste0("Donut plot for ", variable,".")
  
  # get baseline data
  tmp <- subset(df, time == 0)
  
  #tmp[,variable] <- as.integer(tmp[,variable])-1
  
  tmp2 <- tmp %>% 
    group_by_at(variable) %>% 
    summarise(n = n()) %>% 
    mutate(prop = n / sum(n))
  
  tmp2 <- as.data.frame(tmp2)
  
  tmp2[,3] <- round(tmp2[,3], 2)
  
  # Create donut plot
  p <- ggplot(tmp2, aes(x = nrow(tmp2), y = prop, fill = tmp2[, variable], color = tmp2[, variable])) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar(theta = "y", start = 0) +
    #geom_text(aes(y = lab.ypos, label = prop), color = "white", size=0)+
    guides(fill=guide_legend(title="")) +
    theme_void()  # to make donut ->  + xlim(0.5, 2.5)
  return(p)
}

#=============================================================================
# PLOT 3: Histogram
histogram_plot <- function(df, variable){
  # create label
  y_label <- paste0("Histogram of ", variable)
  
  # get baseline data
  tmp <- subset(df, time == 0)
  
  # create plot
  p <- ggplot(data = tmp, aes(x = tmp[, variable])) + 
    geom_histogram(color="darkblue", fill="lightblue") +
    labs(title="",
         x = variable,
         y = y_label)
  
  return(p)
}
#=============================================================================
# PLOT 4: Box plot
donut_summary <- function(df, variable){
  
  # create label
  y_label <- paste0("Donut plot for ", variable,".")
  
  # get baseline data
  tmp <- subset(df, time == 0)
  
  tmp2 <- tmp %>% 
    group_by_at(variable) %>% 
    summarise(n = n()) %>% 
    mutate(prop = n / sum(n))
  
  tmp2 <- as.data.frame(tmp2)
  
  tmp2[,3] <- round(tmp2[,3], 2)
  
  return(tmp2)
}
##############################################################################
##############################################################################
##############################################################################
proportion_plot <- function(df, variable){
  ##############################################################################
  t0 <- percent_ci(df, variable, variable, outcome=1, time_ind=0)
  t1 <- percent_ci(df, variable, variable, outcome=1, time_ind=1)
  t2 <- percent_ci(df, variable, variable, outcome=1, time_ind=2)
  t3 <- percent_ci(df, variable, variable, outcome=1, time_ind=3)
  t4 <- percent_ci(df, variable, variable, outcome=1, time_ind=4)
  t5 <- percent_ci(df, variable, variable, outcome=1, time_ind=5)
  
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
