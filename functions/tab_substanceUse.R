##############################################################################
# Purpose: POINT Dashboard -> SECTION 09: Substance Use
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################

# ROW 1:
su_ever_plot <- function(su_ever_plot_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE: Plots baseline 'ever' used substance as bar plot
  #    XX
  #
  # INPUTS:
  #     su_ever_plot_data (data frame) summarised in prepare_data.R
  #
  # Returns: 
  #    Bar plot
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  p <- 
    ggplot(su_ever_plot_data, aes(x=Drug, y = prop, fill=Drug)) +
    geom_bar(stat='identity') +
    ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = "Proportion of Participants (%)")+ 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  return(p)
}

su_age_plot <- function(df, variable, outcome="Yes", time_ind=0){
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
  # Ever vars
  variables <- c(
    'heroin_first_age',
    'op_nonmed_first_age',
    'op_somelse_first_age',
    'op_other_first_age',
    'metham_first_age',
    'cocaine_first_age',
    'ecstasy_first_age',
    'can_first_age',
    'alc_first_age',
    'cig_first_age',
    'benz_nonmed_first_age',
    'benz_somelse_first_age',
    'halluc_first_age'
  )
  
  names <- c(
    'Heroin',
    'Opioids Non-medical',
    'Opioids someone elses',
    'Opioids other',
    'Methamphetamine',
    'Cocaine',
    'Ecstasy',
    'Cannabis',
    'Alcohol',
    'Cigarettes',
    'Benzodiazepines Non-medical',
    'Benzodiazepines someone elses',
    'Hallucinagens'
  )
  
  return(p)
}

su_drug_trend_plot <- function(su_drug_trend_plot_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #    XX
  #
  # INPUTS:
  #     df: XX
  #     variable: XX
  #
  # Returns: 
  #     Line chart with substance use used over time
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

  # Create plot from summary table data
  y_label <- paste0("Percentage of Participants (%) (N=", su_drug_trend_plot_data$n[1], ")")
  p <- ggplot(data=su_drug_trend_plot_data, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=Condition))+
    geom_ribbon(alpha = 0.3, aes(fill=Condition), show.legend = T) +
    geom_line(aes(color=Condition)) +
    geom_point(aes(color=Condition)) +
    labs(title="",
         x = "Time",
         y = y_label) + 
    ylim(0,100)
  return(p)
}
##############################################################################
# DEPENDENCE
su_dep_proportion_plot <- function(su_dep_proportion_plot_data){
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  # PURPOSE:
  #    Proportion plot for substance use, users. (Rip off of medication diary
  #    proportion plot)
  #
  # INPUTS:
  #     df: point dataset
  #     variable: variable of interest, her it is for dependence or harmful use
  #
  # Returns: 
  #     estimate: XX
  #
  # ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

  ##############################################################################
  y_label <- paste0("Percentage of Participants (%) (N=", su_dep_proportion_plot_data$N[1], ")")
  p <- ggplot(data=su_dep_proportion_plot_data, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=1))+
    #geom_line(size = 8, colour="black",) +
    #geom_df(size=1, color='blue') +
    geom_ribbon(alpha=0.43, fill='blue') +
    geom_line(size=2, color='blue') +
    labs(title="",
         x = "Time",
         y = y_label) + 
    theme_bw()
  return(p)
}
# ROW 2
su_proportion_tbl <- function(df, variable, outcome = "yes"){
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
  
  target_name = ""
  variable = 'Pharm_Opioids_Dep_ICD10'
  outcome = "Yes"
  df <- df %>% select(Participant_ID, time, variable)
  df.cc <- na.omit(df)
  df.cc <- df.cc[which(df.cc$time != 1),]
  
  # keep those based on medication diary
  #df.cc <- df[complete.cases(df[, variable]), ] # 7152
  
  # 1. Get count of ID's presented over time period (min = 1, max = 6) 
  id_counts <- table(df.cc$Participant_ID)
  
  # 2. Get ID's for people  who participated in all waves 
  id_all_waves <- names(id_counts[id_counts == 5])
  
  # 3. Subset data by these people (N = 4842)
  df.cc <- df.cc[df.cc$Participant_ID %in% id_all_waves,] # 4830
  
  ##############################################################################
  t0 <- util_percent_ci(df=df.cc, variable=variable,  outcome=outcome, time_ind=0)
  #t1 <- util_percent_ci(df=df.cc, variable=variable,  outcome=outcome, time_ind=1)
  t2 <- util_percent_ci(df=df.cc, variable=variable,  outcome=outcome, time_ind=2)
  t3 <- util_percent_ci(df=df.cc, variable=variable,  outcome=outcome, time_ind=3)
  t4 <- util_percent_ci(df=df.cc, variable=variable,  outcome=outcome, time_ind=4)
  t5 <- util_percent_ci(df=df.cc, variable=variable,  outcome=outcome, time_ind=5)
  
  tall <- as.data.frame(rbind(t0,t2,t3,t4,t5))
  names(tall)[names(tall) == "V1"] <- 'estimate'
  names(tall)[names(tall) == "V2"] <- 'lower'
  names(tall)[names(tall) == "V3"] <- 'upper'
  names(tall)[names(tall) == "V4"] <- 'time'
  names(tall)[names(tall) == "V5"] <- 'Users'
  names(tall)[names(tall) == "V6"] <- 'N'
  names(tall)[names(tall) == "V7"] <- 'variable'
  
  tall <- tall[,c(1:6)]
  tall$time <- as.factor(tall$time)
  tall <- tall[which(tall$time != 1), ]
  
  # make numeric
  numerics <- c('estimate', 'lower', 'upper')
  tall <- tall %>% mutate_at(numerics, as.character)
  tall <- tall %>% mutate_at(numerics, as.numeric)
  
  return(tall)
}
##############################################################################
# ROW 3: 
su_plot <- function(su_plot_data,var){
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
  
  outcome <- su_plot_data[, var]
  outcome <- na.omit(outcome)
  
  p <-
    ggplot(data = NULL, aes(x=outcome, fill=outcome)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    labs(title="",
         x = "",
         y = "Proportion of Participants (%)")
  
  return(p)
}


##############################################################################
##############################################################################
#################################### END #####################################
##############################################################################
