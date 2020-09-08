##############################################################################
# Purpose: POINT Dashboard -> SECTION 4: PAIN
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
# PLOT 1: Bar chart of baseline pain
# This is a bar plot of baseline presentation of chronic pain problems. 

pain_baseline_plot <- function(pain_baseline_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates bar plot of baseline reported chronic conditions
  #
  # REQUIREMENTS: 
  #     summary_table function
  #
  # INPUTS:
  #     df: dataframe with variableiables
  #
  # Returns: 
  #     p: bar plot
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  p <- 
    ggplot(pain_baseline_data, aes(x=Condition, weight=proportion, fill=Condition)) + 
    geom_bar() + 
    ylab("Proportion of POINT participants (%)") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + ylim(0,100)
  return(p)
}

#-------------------------------------------------------------------------------
# PLOT 2: Line plot of past 12m pain over time 
# This plot shows past 12m reporting of pain conditions over time. 
pain_past12m_plot <- function(pain_past12m_data, data_prepared=TRUE){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates line plot of past 12m reported chronic conditions
  #
  # REQUIREMENTS: 
  #     summary_table function
  #
  # INPUTS:
  #     df: dataframe with variableiables
  #
  # Returns: 
  #     p: line plot
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

  # Create plot from summary table data
  y_label <- paste0("Percentage of Participants (%) (N=", pain_past12m_data$n[1], ")")
  p <- ggplot(data=pain_past12m_data, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=Condition))+
    geom_ribbon(alpha = 0.3, aes(fill=Condition), show.legend = T) +
    geom_line(aes(color=Condition)) +
    geom_point(aes(color=Condition)) +
    labs(title="",
         x = "Time",
         y = y_label) + 
    ylim(0,100)
  p
  return(p)
}

# PLOT 3: Line graph of BPI interference and severity
pain_bpi_plot <- function(pain_bpi_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates line plot of BPI severity and interference over time
  #
  # REQUIREMENTS: 
  #     pain_bpi_tbl: creates a summary of bpi scores
  #
  # INPUTS:
  #     df: summary table of bpi trends over time
  #
  # Returns: 
  #     p: line plot
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  y_label <- paste0("Mean score (out of 10) (N=", pain_bpi_data$count[1], ")")
  p <- 
    ggplot(pain_bpi_data, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci, group=Pain)) +
    geom_ribbon(aes(fill=Pain), alpha=0.4) +
    geom_line(aes(color=Pain), size=1) + 
    geom_point(aes(color=Pain), size=2) + 
    ylim(0,10) + 
    labs(title ="",
         x = "Years",
         y = y_label)
  p
  return(p)
}

# PLOT 4: Summary of BPI scores 
pain_bpi_tbl <- function(pain_bpi_data){
  pain_bpi_data <- pain_bpi_data[, c(8,1,2,3,4,5,6,7)]
  return(pain_bpi_data)
}

##############################################################################
#################################### END #####################################
##############################################################################
