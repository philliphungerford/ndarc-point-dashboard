##############################################################################
# Purpose: POINT Dashboard -> SECTION 09: Substance Use
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################

# ROW 1:
su_ever_plot <- function(df){
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
    'heroin_ever',
    'op_nonmed_ever',
    'op_somelse_ever',
    'op_other_ever',
    'metham_ever',
    'cocaine_ever',
    'ecstasy_ever',
    'can_ever',
    'alc_ever',
    'cig_ever',
    'benz_nonmed_ever',
    'benz_somelse_ever',
    'halluc_ever'
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
  
  tmp <- df %>% select(time, all_of(variables))
  tmp <- tmp[which(tmp$time==0),]
  tmp <- tmp %>% mutate_at(variables, as.numeric)-1
  t <- as.data.frame(colSums(tmp, na.rm=T))
  names(t)[1] <- "users"
  t$prop <- round((t$users / 1504)*100,2)
  t <- setDT(t, keep.rownames = TRUE)[]
  
  t$prop <- as.numeric(t$prop)
  t$rn <- as.factor(t$rn)
  
  t$Drug <- ""
  for (i in 1:length(variables)){
    t$Drug[t$rn == variables[i]] <- names[i]
  }
  
  p <- 
    ggplot(t, aes(x=Drug, y = prop, fill=Drug)) +
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

su_drug_trend_plot <- function(df){
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
  variables <- c(
    'heroin_12m',
    'op_nonmed_12m',
    'op_somelse_12m',
    'op_other_12m',
    'metham_12m',
    'cocaine_12m',
    'ecstasy_12m',
    'can_12m',
    'alc_12m',
    'cig_12m',
    'benz_nonmed_12m',
    'benz_somelse_12m',
    'halluc_12m'
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
  
  # Creates summary table and plots
  t <- lapply(variables, util_percent_ci_all, times=0:5, df=df, outcome="Yes")
  t2 <- bind_rows(t, .id = "column_label")
  
  t2$Condition <- ""
  for (i in 1:length(names)){
    t2$Condition[t2$variable == variables[i]] <- names[i]
  }
  
  
  # Create plot from summary table data
  y_label <- paste0("Proportion of POINT Users")
  p <- ggplot(data=t2, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=Condition))+
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
# DEPENDENCE
su_proportion_plot <- function(df, variable, outcome = "yes"){
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
  
  target_name = ""
  variable = 'Pharm_Opioids_Dep_ICD10'
  outcome = "Yes"
  ##############################################################################
  t0 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=0)
  t1 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=1)
  t2 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=2)
  t3 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=3)
  t4 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=4)
  t5 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=5)
  
  tall <- as.data.frame(rbind(t0,t1,t2,t3,t4,t5))
  names(tall)[names(tall) == "V1"] <- 'estimate'
  names(tall)[names(tall) == "V2"] <- 'lower'
  names(tall)[names(tall) == "V3"] <- 'upper'
  names(tall)[names(tall) == "V4"] <- 'time'
  names(tall)[names(tall) == "V5"] <- 'Users'
  names(tall)[names(tall) == "V6"] <- 'N'
  
  tall$time <- as.factor(tall$time)
  tall <- tall[which(tall$time != 1), ]
  
  # make numeric
  numerics <- c('estimate', 'lower', 'upper')
  tall <- tall %>% mutate_at(numerics, as.character)
  tall <- tall %>% mutate_at(numerics, as.numeric)
  
  ##############################################################################
  y_label <- paste0("Proportion of Participants (%)")
  p <- ggplot(data=tall, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=1))+
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
  ##############################################################################
  t0 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=0)
  t1 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=1)
  t2 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=2)
  t3 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=3)
  t4 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=4)
  t5 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=5)
  
  tall <- as.data.frame(rbind(t0,t1,t2,t3,t4,t5))
  names(tall)[names(tall) == "V1"] <- 'estimate'
  names(tall)[names(tall) == "V2"] <- 'lower'
  names(tall)[names(tall) == "V3"] <- 'upper'
  names(tall)[names(tall) == "V4"] <- 'time'
  names(tall)[names(tall) == "V5"] <- 'Users'
  names(tall)[names(tall) == "V6"] <- 'N'
  
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
su_plot <- function(df,var){
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
  
  outcome <- df[df$time==0, var]
  outcome <- na.omit(outcome)
  
  p <-
    ggplot(data = NULL, aes(x=outcome, fill=outcome)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    labs(title="",
         x = "",
         y = "Proportion of Participants (%)") + 
    theme_bw()
  
  return(p)
}


##############################################################################
##############################################################################
#################################### END #####################################
##############################################################################
