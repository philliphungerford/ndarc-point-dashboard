##############################################################################
# Purpose: POINT Dashboard -> SECTION 2: DEMOGRAPHICS
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
# PLOT 1: Density plot
density_plot <- function(df, variable){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Create's a density plot for continuous variables
  #
  # INPUTS:
  #     df: dataframe with variables
  #     variable: variable of which to calculate density plot
  #
  # Returns: 
  #     p: plot 
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
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
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Create's a donut plot for categorical variables
  #
  # INPUTS:
  #     df: dataframe with variables
  #     variable: variable of which to calculate density plot
  #
  # Returns: 
  #     p: plot 
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # create label
  y_label <- paste0("Donut plot for ", variable,".")
  
  # get baseline data
  tmp <- subset(df, time == 0)
  
  #tmp[,variable] <- as.integer(tmp[,variable])-1
  
  tmp2 <- tmp %>% 
    dplyr::group_by_at(variable) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::mutate(prop = n / sum(n))
  
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
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Create's a histogram plot for continuous variables
  #
  # INPUTS:
  #     df: dataframe with variables
  #     variable: variable of which to calculate density plot
  #
  # Returns: 
  #     p: plot 
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
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
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Create's summary data which is used for the donut plot
  #
  # INPUTS:
  #     df: dataframe with variables
  #     variable: variable of which to calculate density plot
  #
  # Returns: 
  #     tmp2: dataframe/table of summarised results 
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # create label
  y_label <- paste0("Donut plot for ", variable,".")
  
  # get baseline data
  tmp <- subset(df, time == 0)
  
  tmp2 <- tmp %>% 
    dplyr::group_by_at(variable) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::mutate(prop = n / sum(n))
  
  tmp2 <- as.data.frame(tmp2)
  
  tmp2[,3] <- round(tmp2[,3], 2)
  
  return(tmp2)
}

##############################################################################
#################################### END #####################################
##############################################################################