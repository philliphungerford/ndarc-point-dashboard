##############################################################################
# Purpose: POINT Dashboard -> SECTION 7: Quality of Life
# Author: Phillip Hungerford
# Date: 2020-08-13
##############################################################################
qol_q1_plot <- function(df){
  
  df <- get_complete_case(df, variable='WHO_QOL_q1', yrs=6) # 4920
  t <- table(df$time, df$WHO_QOL_q1)
  t <- as.data.frame(t)
  t$prop <- NA
  names(t)[names(t)=="Var2"] <- "Quality of Life"
  for (i in 0:5){
    t$prop[which(t$Var1==i)] <- (round(t$Freq[which(t$Var1==i)] / length(!is.na(df$WHO_QOL_q1[which(df$time==i)])),2))*100
  }
  
  y_label <- paste0("Proportion of participants (%) (N=", table(df$time)[1], ")")
  p <- 
  ggplot(t, aes(x=Var1, y = prop, group = `Quality of Life`, color=`Quality of Life`)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = y_label)
  p
  return(p)
}

qol_q2_plot <- function(df){
  df <- get_complete_case(df, variable='WHO_QOL_q2', yrs=6) # 4920
  t <- table(df$time, df$WHO_QOL_q2)
  t <- as.data.frame(t)
  t$prop <- NA
  names(t)[names(t)=="Var2"] <- "Health Satisfaction"
  for (i in 0:5){
    t$prop[which(t$Var1==i)] <- (round(t$Freq[which(t$Var1==i)] / length(!is.na(df$WHO_QOL_q2[which(df$time==i)])),2))*100
  }
  
  t <- t[which(t$`Health Satisfaction` != 'Not Applicable'), ]
  y_label <- paste0("Proportion of participants (%) (N=", table(df$time)[1], ")")
  p <- 
    ggplot(t, aes(x=Var1, y = prop, group = `Health Satisfaction`, color= `Health Satisfaction`)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = y_label)
  p
  return(p)
}
##############################################################################
#################################### END #####################################
##############################################################################