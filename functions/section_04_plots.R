##############################################################################
# Purpose: POINT Dashboard -> SECTION 4: PAIN
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
# PLOT 1: Bar chart of baseline pain
# This is a bar plot of baseline presentation of chronic pain problems. 

pain_baseline_plot <- function(){
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

  return()
}

#-------------------------------------------------------------------------------
# PLOT 2: Line plot of past 12m pain over time 
# This plot shows past 12m reporting of pain conditions over time. 
pain_past12m_plot <- function(df){
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
  chronic_variables <- c(
    'Arth_12m',
    'Back_12m',
    'Head_12m',
    'Visc_12m',
    'Fibro_12m',
    'Cmplx_12m',
    'Shing_12m',
    'Gen_Pain_12m'
  )
  
  chronic_names <- c(
    'Arthritis',
    'Back or neck',
    'Severe headaches',
    'Visceral',
    'Fibromyalgia',
    'Complex regional',
    'Shingles',
    'Generalised'
  )
  
  # Creates summary table and plots
  t <- lapply(chronic_variables, util_percent_ci_all, times=0:5, df=df, outcome="Yes")
  t2 <- bind_rows(t, .id = "column_label")
  
  t2$Condition <- ""
  for (i in 1:length(chronic_names)){
    t2$Condition[t2$variable == chronic_variables[i]] <- chronic_names[i]
  }

  
  # Create plot from summary table data
  y_label <- paste0("Proportion of POINT Users")
  p <- ggplot(data=t2, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=Condition))+
    geom_ribbon(alpha = 0.3, aes(fill=Condition), show.legend = T) +
    geom_line(aes(color=Condition)) +
    geom_point(aes(color=Condition)) +
    labs(title="",
         x = "Time",
         y = y_label)
  p
  return(p)
}

# PLOT 3: Line graph of BPI interference and severity
pain_bpi_plot <- function(){
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
}

# PLOT 4: Summary of BPI scores 
pain_bpi_tbl <- function(){
  
}

##############################################################################
#################################### END #####################################
##############################################################################
