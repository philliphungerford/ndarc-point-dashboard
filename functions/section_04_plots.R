##############################################################################
# Purpose: POINT Dashboard -> SECTION 4: PAIN
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
# Functions
percent_ci <- function(df, variable){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Calculate the percent and 95% confidence intervals for a given variable
  #
  # INPUTS:
  #     df: dataframe with variableiables
  #     variable: variable of which to calculate density plot
  #
  # Returns: 
  #     estimate: the proportion calculated
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # Make sure df is a dataframe
  df <- as.data.frame(df)
  
  # Extract vector of desired variable
  tmp <- df[,variable]
  
  # Calculate the p and se
  observed <- sum(df[, variable] == 'Yes', na.rm=T)
  n <- length(tmp)
  
  # Calculate the proportion
  p <- ( observed / n) * 100
  
  # standard error = proportion of outcome 1 * proportion of outcome 2 / n
  se <- sqrt(p * (100 - p) / n)
  
  ## 2. Create % (95% CI)
  estimate <- round(p, 2)
  lower <- round(p - (1.96*se), 2)
  upper <- round(p + (1.96*se), 2)
  
  # Conditionals for small numbers
  if (lower < 0){ lower <- 0 }
  if (upper > 100){ upper <- 100 }

  # return results
  return(estimate)
}

summary_table <- function(df){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Creates summary table of values defined within (chronic variables)
  #
  # INPUTS:
  #     df: dataframe with variableiables
  #     variable: variable of which to calculate density plot
  #
  # Returns: 
  #     estimate: the proportion calculated
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
  
  b <- subset(tmp, time == 0)
  t0 <- as.data.frame(sapply(chronic_variables, FUN = percent_ci, df=b))
  names(t0)[1] <- "t0"
  
  b <- subset(tmp, time == 1)
  t1 <- as.data.frame(sapply(chronic_variables, FUN = percent_ci, df=b))
  names(t1)[1] <- "t1"
  
  b <- subset(tmp, time == 2)
  t2 <- as.data.frame(sapply(chronic_variables, FUN = percent_ci, df=b))
  names(t2)[1] <- "t2"
  
  b <- subset(tmp, time == 3)
  t3 <- as.data.frame(sapply(chronic_variables, FUN = percent_ci, df=b))
  names(t3)[1] <- "t3"
  
  b <- subset(tmp, time == 4)
  t4 <- as.data.frame(sapply(chronic_variables, FUN = percent_ci, df=b))
  names(t4)[1] <- "t4"
  
  b <- subset(tmp, time == 5)
  t5 <- as.data.frame(sapply(chronic_variables, FUN = percent_ci, df=b))
  names(t5)[1] <- "t5"
  
  tall <- cbind(t0,t1,t2,t3,t4,t5)
  tall$Condition <- chronic_names
  return(tall)
}



##############################################################################
# PLOT 1: Bar chart of baseline pain
# This is a bar plot of baseline presentation of chronic pain problems. 

pain_baseline_plot <- function(df){
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
  
  # Creates summary table and plots
  tmp <- summary_table(df)
  
  # Create plot from summary table data
  p <- ggplot(data = tmp, aes(x = Condition, y = t0, fill = Condition)) + 
    geom_col() + 
    ylab(" Proportion (%)") + 
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  return(p)
  }
#-------------------------------------------------------------------------------
# PLOT 2: Line plot of past 12m pain over time 
# This plot shows past 12m reporting of pain conditions over time. 
pain_past12m <- function(){
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
