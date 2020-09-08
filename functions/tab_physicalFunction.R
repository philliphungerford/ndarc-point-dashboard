##############################################################################
# Purpose: POINT Dashboard -> SECTION 05: Physical Function
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################
## this section has 3 line plots over time and 2 bar charts 

## EXERCISE 
# NOTE: Exercise variables not collected at year 1

# DEPENDENCE
pf_ex_days <- function(pf_ex_days_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates line plot of days utilised during the week
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

  y_label <- paste0("Percentage of Participants (%) (N=", table(pf_ex_days_data$n)[1], ")")
  p <- 
    ggplot(pf_ex_days_data, aes(x=time, y = prop, group = days, color= days)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    #ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = y_label)
  p
  return(p)
}

## Exercise intensity
pf_ex_in <- function(pf_ex_in_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates line plot of days utilised during the week
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

  y_label <- paste0("Percentage of Participants (%) (N=", table(pf_ex_in_data$n)[1], ")")
  
  p <- 
    ggplot(pf_ex_in_data, aes(x=Var1, y = prop, group = Intensity, color= Intensity)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    #ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = y_label) 
  p
  return(p)
}

## Exercise intensity
pf_ex_tp <- function(pf_ex_tp_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates line plot of days utilised during the week
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
  
  y_label <- paste0("Percentage of Participants (%) (N=", table(pf_ex_tp_data$n)[1], ")")
  
   p <- 
    ggplot(pf_ex_tp_data, aes(x=Var1, y = prop, group = Type, color= Type)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    #ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = y_label) 
   p
  return(p)
}

## PSEQ
pf_pseq <- function(pf_pseq_data){
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

  y_label <- paste0("Mean PSEQ Score (N=", table(pf_pseq_data$n)[1], ")")
  p <- 
    ggplot(pf_pseq_data, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci)) +
    geom_ribbon(alpha=0.4, fill = 'red') +
    geom_line(size=1, color = 'red') + 
    geom_point(size=2, color = 'red') + 
    labs(title ="",
         x = "Years",
         y = y_label)
  p
  return(p)
}

## SLEEP
pf_slp <- function(pf_slp_data){
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
  
  y_label <- paste0("Mean Sleep Score (N=", table(pf_slp_data$n)[1], ")")
  p <- 
    ggplot(pf_slp_data, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci)) +
    geom_ribbon(alpha=0.4, fill = 'purple') +
    geom_line(size=1, color = 'purple') + 
    geom_point(size=2, color = 'purple') + 
    labs(title ="",
         x = "Years",
         y = y_label)
  p
  return(p)
}
##############################################################################
#################################### END #####################################
##############################################################################