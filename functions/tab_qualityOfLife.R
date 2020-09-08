##############################################################################
# Purpose: POINT Dashboard -> SECTION 7: Quality of Life
# Author: Phillip Hungerford
# Date: 2020-08-13
##############################################################################
# PLOTS
qol_q1_plot <- function(qol_q1_data, qol_q1_n){
  qol_q1_data <- qol_q1_data[which(qol_q1_data$`Quality of Life` != 'Not Applicable'), ]
  y_label <- paste0("Proportion of participants (%) (N=", qol_q1_n, ")")
  p <- 
  ggplot(qol_q1_data, aes(x=Var1, y = prop, group = `Quality of Life`, color=`Quality of Life`)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = y_label)
  return(p)
}

qol_q2_plot <- function(qol_q2_data, qol_q2_n){
  y_label <- paste0("Proportion of participants (%) (N=", qol_q2_n, ")")
  p <- 
    ggplot(qol_q2_data, aes(x=Var1, y = prop, group = `Health Satisfaction`, color= `Health Satisfaction`)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = y_label)
  return(p)
}

##############################################################################
#################################### END #####################################
##############################################################################