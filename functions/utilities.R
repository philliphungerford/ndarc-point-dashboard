##############################################################################
# Purpose: Utilities script for functions that are frequently used
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
util_percent_ci <- function(df, variable, outcome="Yes", time_ind=0){
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
  # Example:
  #      percent_ci(df, variable = "Arth_12m", outcome = "Yes", time_ind=0)
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  df <- subset(df, time == time_ind)
  #df <- df[!is.na(df$totalopioiddose), ]
  
  df <- as.data.frame(df)
  tmp <- df[,variable]
  
  # Calculate the p and se
  observed <- sum(df[, variable] == outcome, na.rm=T)
  n <- length(tmp)
  
  # calculate the proportion
  p <- ( observed / n) * 100
  
  # standard error = proportion of outcome 1 * proportion of outcome 2 / n
  se <- sqrt(p * (100 - p) / n)
  
  ## 2. Create % (95% CI)
  # return result
  estimate <- round(p, 2)
  lower <- round(p - (1.96*se), 2)
  upper <- round(p + (1.96*se), 2)
  
  # conditionals for small numbers
  if (lower < 0){ lower <- 0 }
  if (upper > 100){ upper <- 100 }
  # print results
  result <- c(estimate, lower, upper, time_ind, observed, n, variable)
  # return results
  return(result)
}

util_percent_ci_all <- function(times=0:5, df=df, variable='Back_12m', outcome="Yes"){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #     Calculate the percent and 95% confidence intervals for multiple time points
  #
  # INPUTS:
  #     times: vector of times keep it 0:5 for baseline to year 5
  #     df: dataframe with variableiables
  #     variable: variable of which to calculate density plot
  #     outcome: positive value for variable
  #
  # Returns: 
  #     results: dataframe / table of summarised results
  #
  # Example: 
  #     percent_ci_all(times=0:5, df=point, variable='Arth_12m', outcome="Yes")
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  t <- lapply(times, util_percent_ci, df=df, variable=variable, outcome=outcome)
  results <- data.frame(matrix(unlist(t), nrow=length(t), byrow=T))
  
  # rename column vals
  oldnames <- c('X1','X2', 'X3', 'X4', 'X5', 'X6', 'X7')
  newnames <- c('estimate', 'lower', 'upper', 'time', 'observed', 'n', 'variable')
  
  
  
  results <- results %>% rename_at(vars(oldnames), ~ newnames)
  results <- results[,c(4,1,2,3,5,6,7)]
  
  # convert
  results <- transform(results,
                       estimate=as.numeric(as.character(estimate)),
                       lower=as.numeric(as.character(lower)),
                       upper=as.numeric(as.character(upper)),
                       time=factor(time),
                       observed=as.integer(as.character(observed)),
                       n = as.integer(as.character(n)),
                       variable = factor(variable))
  return(results)
}


# continuous ci
util_lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
util_upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}


get_complete_case <- function(df, variable, yrs=6){
  # Get complete case data = df.cc

  # keep those based on medication diary
  df.cc <- df[complete.cases(df[, variable]), ] # 7152
  
  # 1. Get count of ID's presented over time period (min = 1, max = 6) 
  id_counts <- table(df.cc$Participant_ID)
  
  # 2. Get ID's for people  who participated in all waves 
  id_all_waves <- names(id_counts[id_counts == yrs])
  
  # 3. Subset data by these people (N = 4842)
  df.cc <- df.cc[df.cc$Participant_ID %in% id_all_waves,] # 4830
  table(df.cc$time) # 805 at each wave, N=4830
  
  return(df.cc)
}

# t <- get_complete_case(df = point, variable = 'totalopioiddose')
##############################################################################
#################################### END #####################################
##############################################################################
