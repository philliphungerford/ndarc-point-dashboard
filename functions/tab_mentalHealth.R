##############################################################################
# Purpose: POINT Dashboard -> SECTION 09: Substance Use
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################

# ROW 1:
mh_ever_plot <- function(mh_ever_plot_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #    XX
  #
  # INPUTS:
  #     df: XX
  #     variable: XX
  #
  # Returns: 
  #     estimate: XX
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  # 
  p <- 
    ggplot(mh_ever_plot_data, aes(x=Drug, y = prop, fill=Drug)) +
    geom_bar(stat='identity') +
    ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = "Proportion of Participants (%)")+ 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())
  
  return(p)
}

mh_drug_trend_plot <- function(mh_drug_trend_data, mh_drug_trend_n){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #    XX
  #
  # INPUTS:
  #     df: XX
  #     variable: XX
  #
  # Returns: 
  #     estimate: XX
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

  # Create plot from summary table data
  
  y_label <- paste0("Proportion of participants (%) (N=", mh_drug_trend_n, ")")
  
  p <- ggplot(data=mh_drug_trend_data, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=Condition))+
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

##############################################################################
#################################### END #####################################
##############################################################################
