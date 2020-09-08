##############################################################################
# Purpose: POINT Dashboard -> SECTION 4: PAIN
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
# PLOT 1: Bar chart of orbit trends over time
# This is a bar plot of baseline presentation of chronic pain problems. 

tmt_orbit_plot <- function(tmt_orbit_data, exclude_never, variables){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates line plot of days utilised during the week
  #
  # REQUIREMENTS: 
  #     pain_bpi_tbl: creates a summary of bpi scores
  #
  # INPUTS:
  #     df: summary table of bpi trends over time
  #     variables: 'orbit_items' is a list with the variable names within them
  #
  # Returns: 
  #     p: line plot
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  t <- tmt_orbit_data[which(tmt_orbit_data$question == variables), ]
  colours <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e')
  
  if(exclude_never == TRUE){
    t <- t[which(t$Item != 'Never'), ]
    colours <- c('#d95f02','#7570b3','#e7298a','#66a61e')
  }
  
  y_label <- paste0("Percentage of Participants (%) (N=", t$n[1], ")")
  
  p <- 
    ggplot(t, aes(x=Var1, y = prop, group = Item, color= Item)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    scale_color_manual(values = colours) + 
    #ylim(0,100) +
    labs(title="",
         x = "Years",
         y = y_label) 
  return(p)
}

##############################################################################
#################################### END #####################################
##############################################################################
