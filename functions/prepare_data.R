##############################################################################
# Purpose: Summarise data
# Author: Phillip Hungerford
# Date: 2020-08-13
##############################################################################
# INFO
# This script is for loading the master data set and summarising it for the dashboard
# From 2020-09-08 all of the plots will read the summarised data
##############################################################################
library(dplyr)
##############################################################################
list_to_df <- function(list){
  require(dplyr)
  df <- do.call(rbind.data.frame, list)
  df <- data.table::setDT(df, keep.rownames = TRUE)[]
  df <- as.data.frame(df)
  df <- df %>% select(-rn)
  return(df)
}

##############################################################################
# TAB 1: Overview = NOT APPLICABLE

##############################################################################
# TAB 2: Measures
measures_data <- read.csv("data/raw/measures_data_20200825.csv", fileEncoding='UTF-8-BOM') # Section 2: measures

##############################################################################
# TAB 3: DEMOGRAPHICS
create_demographics_data <- function(df=point){
  
  # demographic variables
  demographic_int <- c("age", # int
                       "height", # int
                       "weight", # int
                       "bmi", # int
                       "Live_Count") # int
  
  demographic_fac <- c("sex", # fact
                       "edu", # fac
                       "cob", # fac
                       "indig", # fac
                       "maritalstatus", # fac
                       "Employ", # fac
                       "income_wk", # fac
                       "accom") # fac 
  
  ## DATA 
  demographics_data <- df %>% select(time, all_of(demographic_int), all_of(demographic_fac))
  demographics_data <- subset(demographics_data, time == 0)
  return(demographics_data)
}

##############################################################################
# TAB 4: pain
create_pain_baseline_data <- function(df){
  variables <- c(
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
  
  # select chronic pain variables
  tmp <- df %>% select(time, all_of(variables))
  
  #subset for baseline only 
  tmp <- tmp[tmp$time==0, ]
  tmp <- tmp %>% mutate_at(variables, as.character)
  tmp <- tmp %>% mutate_at(variables, as.factor)
  tmp <- tmp %>% mutate_at(variables, as.numeric)-1
  
  # get proportions
  pain_baseline_data <- round((colSums(tmp, na.rm=T) / nrow(tmp))*100,2)
  
  pain_baseline_data <-pain_baseline_data[2:9]
  pain_baseline_data <- as.data.frame(pain_baseline_data)
  
  library(data.table)
  pain_baseline_data <- setDT(pain_baseline_data, keep.rownames = TRUE)[]
  
  pain_baseline_data$Condition <- ""
  for (i in 1:length(chronic_names)){
    pain_baseline_data$Condition[pain_baseline_data$rn == variables[i]] <- chronic_names[i]
  }
  
  # rename for understanding
  names(pain_baseline_data)[names(pain_baseline_data) == 'pain_baseline_data'] <- 'proportion'
  
  return(pain_baseline_data)
}
#=============================================================================
create_pain_past12m_data <- function(df){
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
  
  
  df <- df %>% select(Participant_ID, time, all_of(chronic_variables))
  df.cc <- na.omit(df)
  
  # keep those based on medication diary
  #df.cc <- df[complete.cases(df[, variable]), ] # 7152
  
  # 1. Get count of ID's presented over time period (min = 1, max = 6) 
  id_counts <- table(df.cc$Participant_ID)
  
  # 2. Get ID's for people  who participated in all waves 
  id_all_waves <- names(id_counts[id_counts == 6])
  
  # 3. Subset data by these people (N = 4842)
  df.cc <- df.cc[df.cc$Participant_ID %in% id_all_waves,] # 4830
  
  # Creates summary table and plots
  t <- lapply(chronic_variables, util_percent_ci_all, times=0:5, df=df.cc, outcome="Yes")
  pain_past12m_data <- bind_rows(t, .id = "column_label")
  
  pain_past12m_data$Condition <- ""
  for (i in 1:length(chronic_names)){
    pain_past12m_data$Condition[pain_past12m_data$variable == chronic_variables[i]] <- chronic_names[i]
  }
  return(pain_past12m_data)
}
#=============================================================================
create_pain_bpi_data <- function(df){
  variables <- c("BPI_interference", "BPI_PScore")
  
  tmp <- df %>% select(Participant_ID, time, all_of(variables))
  tmp <- get_complete_case(df = tmp, variable = variables, yrs=6) #n=5820
  
  # Convert to long format
  library(reshape2)
  dfw_long <- melt(tmp,
                   id.vars = "time",
                   measure.vars = c("BPI_interference","BPI_PScore"),
                   variable.name = "condition")
  interference <- tmp %>%
    group_by(time) %>%
    dplyr::summarise(smean = mean(BPI_interference, na.rm = TRUE),
                     ssd = sd(BPI_interference, na.rm = TRUE),
                     count = n()) %>%
    dplyr::mutate(se = ssd / sqrt(count),
                  lower_ci = util_lower_ci(smean, se, count),
                  upper_ci = util_upper_ci(smean, se, count))
  
  severity <- tmp %>%
    group_by(time) %>%
    dplyr::summarise(smean = mean(BPI_PScore, na.rm = TRUE),
                     ssd = sd(BPI_PScore, na.rm = TRUE),
                     count = n()) %>%
    dplyr::mutate(se = ssd / sqrt(count),
                  lower_ci = util_lower_ci(smean, se, count),
                  upper_ci = util_upper_ci(smean, se, count))
  
  interference$Pain <- "Interference"
  severity$Pain <- "Severity"
  
  mydata <- rbind(interference, severity)
  
  return(mydata)
}

##############################################################################
# TAB 5: physical function
# pf_ex_days_data <- create_pf_ex_days_data(df=point)
create_pf_ex_days_data <- function(df){
  
  variables <- c("ex_days")
  tmp <- df %>% select(Participant_ID, time, all_of(variables)) # n =7578
  
  # some days are in 0.5 e.g. 1.5 days, here I will round down being conservative
  tmp[tmp == 9] <- NA
  
  tmp <- na.omit(tmp) # n=5619
  
  # remove year 1 as it was not collected
  tmp <- tmp[which(tmp$time !=1), ]
  
  # get complete case data
  tmp <- get_complete_case(df = tmp, variable = variables, yrs=5) #n=2985
  
  tmp$ex_days <- as.integer(tmp$ex_days)
  tmp$ex_days <- as.factor(tmp$ex_days)
  
  t <- as.data.frame(table(tmp$time, tmp$ex_days))
  names(t)[names(t) == "Var1"] <- "time"
  names(t)[names(t) == "Var2"] <- "days"
  
  # for proportions
  t2 <- table(tmp$time)
  #t2 <- table(df$time)
  
  t$n <- NA
  
  times <- c(0,2,3,4,5)
  for (i in 1:length(times)){
    t$n[ which(t$time == times[i]) ] <- t2[i]
  }
  t$prop <- (t$Freq / t$n)*100
  t$prop <- format(round(t$prop,2),2)
  t$days <- as.factor(t$days)
  t$time <- as.factor(t$time)
  t$prop <- as.character(t$prop)
  t$prop <- as.numeric(t$prop)
  return(t)
}
#=============================================================================
create_pf_ex_in_data <- function(df){
  
  variables <- c("ex_in")
  
  tmp <- df %>% select(Participant_ID, time, all_of(variables)) # 7578
  
  # remove year 1 as it was not collected
  tmp <- tmp[which(tmp$time !=1), ] # 6243
  
  # get complete case data
  tmp <- get_complete_case(df = tmp, variable = variables, yrs=5) #n=2985
  
  tmp$ex_in <- ordered(tmp$ex_in, levels = c("Very light (walk, slow pace, wash dishes)",
                                             "Light (walk, medium pace, carry light load on level ground)",
                                             "Moderate (walk, medium pace, carry heavy load on ground)",
                                             "Heavy (jogging, climb stairs, hill at moderate pace)",
                                             "Very heavy (running, hills, carry heavy load)"
  ))
  
  tmp <- na.omit(tmp) # 4297
  
  t <- as.data.frame(table(tmp$time, tmp$ex_in))
  
  #names(t)[names(t) == "Var1"] <- "time"
  #names(t)[names(t) == "Var2"] <- "Intensity"
  
  # remove time 1 because it was not collected
  t <- t[which(t$Var1 != 1), ]
  
  # for proportions
  t2 <- table(tmp$time)
  #t2 <- table(df$time)
  
  t$n <- NA
  
  times <- c(0,2,3,4,5)
  for (i in 1:length(times)){
    t$n[ which(t$Var1 == times[i]) ] <- t2[i]
  }
  
  t$prop <- (t$Freq / t$n)*100
  t$prop <- format(round(t$prop,2),2)
  t$Var2 <- as.factor(t$Var2)
  t$Var1 <- as.factor(t$Var1)
  t$prop <- as.character(t$prop)
  t$prop <- as.numeric(t$prop)
  
  # rename outcome
  names(t)[names(t) == "Var2"] <- "Intensity"
  return(t)
}
#=============================================================================
create_pf_ex_tp_data <- function(df){
  variables <- c("ex_type")
  
  tmp <- df %>% select(Participant_ID, time, all_of(variables))
  
  tmp <- na.omit(tmp) # 2366
  # get complete case data
  tmp <- get_complete_case(df = tmp, variable = variables, yrs=5) #n=2985
  
  t <- as.data.frame(table(tmp$time, tmp$ex_type))
  
  #names(t)[names(t) == "Var1"] <- "time"
  #names(t)[names(t) == "Var2"] <- "Type"
  
  # remove time 1 because it was not collected
  t <- t[which(t$Var1 != 1), ]
  
  # for proportions
  t2 <- table(tmp$time)
  t2 <- table(df$time)
  
  t$n <- NA
  
  times <- c(0,2,3,4,5)
  for (i in 1:length(times)){
    t$n[ which(t$Var1 == times[i]) ] <- t2[i]
  }
  
  t$prop <- (t$Freq / t$n)*100
  t$prop <- format(round(t$prop,2),2)
  t$Var2 <- as.factor(t$Var2)
  t$Var1 <- as.factor(t$Var1)
  t$prop <- as.character(t$prop)
  t$prop <- as.numeric(t$prop)
  names(t)[names(t) == "Var2"] <- "Type"
  
  pf_ex_tp_data <- t
  return(t)
}
#=============================================================================
create_pf_pseq_data <- function(df){
  variables <- c("PSEQ_Score")
  
  tmp <- df %>% select(Participant_ID, time, all_of(variables))
  
  tmp <- tmp[which(tmp$time != 1), ]
  tmp <- get_complete_case(df = tmp, variable = variables, yrs=5) #n=5060
  
  # Convert to long format
  library(reshape2)
  dfw_long <- melt(tmp,
                   id.vars = "time",
                   measure.vars = c("PSEQ_Score"),
                   variable.name = "condition")
  
  pseq <- tmp %>%
    group_by(time) %>%
    dplyr::summarise(smean = mean(PSEQ_Score, na.rm = TRUE),
                     ssd = sd(PSEQ_Score, na.rm = TRUE),
                     count = n()) %>%
    dplyr::mutate(se = ssd / sqrt(count),
                  lower_ci = util_lower_ci(smean, se, count),
                  upper_ci = util_upper_ci(smean, se, count))
  return(pseq)
}
#=============================================================================
create_pf_slp_data <- function(df){
  variables <- c("SLP9")
  tmp <- df %>% select(Participant_ID, time, all_of(variables))
  tmp <- get_complete_case(df = tmp, variable = variables, yrs=6) #n=4866
  # Convert to long format
  library(reshape2)
  dfw_long <- melt(tmp,
                   id.vars = "time",
                   measure.vars = c("SLP9"),
                   variable.name = "condition")
  
  slp <- tmp %>%
    group_by(time) %>%
    dplyr::summarise(smean = mean(SLP9, na.rm = TRUE),
                     ssd = sd(SLP9, na.rm = TRUE),
                     count = n()) %>%
    dplyr::mutate(se = ssd / sqrt(count),
                  lower_ci = util_lower_ci(smean, se, count),
                  upper_ci = util_upper_ci(smean, se, count))
  
  return(slp)
}

##############################################################################
# TAB 6: treatment # ** Selections may be difficult

#=============================================================================
# ORBIT ITEMS
create_tmt_orbit_data <- function(variables, df){
  
  tmp <- df %>% select('Participant_ID', 'time', all_of(variables))
  tmp <- get_complete_case(df = tmp, variable = variables, yrs=6) #n=4128
  
  
  #rename it for usability
  names(tmp)[names(tmp) == variables] <- "item"
  
  tmp$item <- ordered(tmp$item, levels = c("Never",
                                           "Hardley ever",
                                           "Sometimes",
                                           "Often",
                                           "Very Often"
  ))
  
  tmp <- na.omit(tmp) # 4297
  
  t <- as.data.frame(table(tmp$time, tmp$item))
  
  #names(t)[names(t) == "Var1"] <- "time"
  #names(t)[names(t) == "Var2"] <- "Intensity"
  
  # remove time 1 because it was not collected
  
  # for proportions
  t2 <- table(tmp$time)
  #t2 <- table(df$time)
  
  t$n <- NA
  
  times <- c(0,1,2,3,4,5)
  for (i in 1:length(times)){
    t$n[ which(t$Var1 == times[i]) ] <- t2[i]
  }
  
  t$prop <- (t$Freq / t$n)*100
  t$prop <- format(round(t$prop,2),2)
  t$Var2 <- as.factor(t$Var2)
  t$Var1 <- as.factor(t$Var1)
  t$prop <- as.character(t$prop)
  t$prop <- as.numeric(t$prop)
  t$question <- variables
  
  # rename outcome
  names(t)[names(t) == "Var2"] <- "Item"
  return(t)
}
##############################################################################
# TAB 7: quality of life
create_qol_q1_data <- function(df){
  df <- get_complete_case(df, variable='WHO_QOL_q1', yrs=6) # 4920
  t <- table(df$time, df$WHO_QOL_q1)
  t <- as.data.frame(t)
  t$prop <- NA
  names(t)[names(t)=="Var2"] <- "Quality of Life"
  for (i in 0:5){
    t$prop[which(t$Var1==i)] <- (round(t$Freq[which(t$Var1==i)] / length(!is.na(df$WHO_QOL_q1[which(df$time==i)])),2))*100
  }
  
  qol_q1_n <- table(df$time)[1]
  results <- list(t, qol_q1_n)
}

#=============================================================================
create_qol_q2_data <- function(df){
  df <- get_complete_case(df, variable='WHO_QOL_q2', yrs=6) # 4920
  t <- table(df$time, df$WHO_QOL_q2)
  t <- as.data.frame(t)
  t$prop <- NA
  names(t)[names(t)=="Var2"] <- "Health Satisfaction"
  for (i in 0:5){
    t$prop[which(t$Var1==i)] <- (round(t$Freq[which(t$Var1==i)] / length(!is.na(df$WHO_QOL_q2[which(df$time==i)])),2))*100
  }
  
  qol_q2_data <- t[which(t$`Health Satisfaction` != 'Not Applicable'), ]
  qol_q2_n <- table(df$time)[1]
  
  results <- list(qol_q2_data, qol_q2_n)
  return(results)
}

##############################################################################
# TAB 8: mental health
create_mh_ever_plot_data <- function(df){
  # Ever vars
  variables <- c(
    'dep_ever',
    'ptsd_ever',
    'anx_ever',
    'bipol_ever',
    'ocd_ever',
    'schiz_ever',
    'psychosis_ever',
    'sud_ever',
    'adhd_ever',
    'pers_disorder_ever'
  )
  
  names <- c(
    'Depression',
    'Post-traumatic stress disorder (PTSD)',
    'Anxiety',
    'Bipolar Disorder',
    'Obsessive compulsive disorder (OCD)',
    'Schizophrenia',
    'Psychosis',
    'Substance use/dependence',
    'Attention-deficit/hyperactivity disorder(ADHD)',
    'Personality disorder'
  )
  
  tmp <- df %>% select(time, all_of(variables))
  tmp <- tmp[which(tmp$time==0),]
  tmp <- tmp %>% mutate_at(variables, as.character)
  tmp <- tmp %>% mutate_at(variables, as.factor)
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
  return(t)
}

#=============================================================================
create_mh_drug_trend_data <- function(df){
  variables <- c(
    'dep_12m',
    'ptsd_12m',
    'anx_12m',
    'bipol_12m',
    'ocd_12m',
    'schiz_12m',
    'psychosis_12m',
    'sud_12m',
    'adhd_12m',
    'pers_disorder_12m'
  )
  
  names <- c(
    'Depression',
    'Post-traumatic stress disorder (PTSD)',
    'Anxiety',
    'Bipolar Disorder',
    'Obsessive compulsive disorder (OCD)',
    'Schizophrenia',
    'Psychosis',
    'Substance use/dependence',
    'Attention-deficit/hyperactivity disorder(ADHD)',
    'Personality disorder'
  )
  
  df <- df %>% select(Participant_ID, time, all_of(variables))
  
  df  <- df[which(df$time !=1 ), ] # 6343
  df.cc <- get_complete_case(df, variable='dep_12m', yrs=5)
  
  # Creates summary table and plots
  t <- lapply(variables, util_percent_ci_all, times=c(0,2,3,4,5), df=df.cc, outcome="Yes")
  t2 <- bind_rows(t, .id = "column_label")
  
  t2$Condition <- ""
  for (i in 1:length(names)){
    t2$Condition[t2$variable == variables[i]] <- names[i]
  }
  # remove year 1 data because there was no data collected at this time point
  #t2 <- t2[which(t2$time != 1), ]
  
  mh_drug_trend_n <- table(df.cc$time)[1]
  #mh_drug_trend_data <- t2
  results <- list(t2, mh_drug_trend_n)
  return(results)
}

##############################################################################
# TAB 9: substance use # ** Selections may be difficult
#=============================================================================
create_su_ever_plot <- function(df){
  
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
  tmp <- tmp %>% mutate_at(variables, as.character)
  tmp <- tmp %>% mutate_at(variables, as.factor)
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
  
  return(t)
}
#=============================================================================
create_su_age_plot <- function(){
  return()
}
#=============================================================================
create_su_drug_trend_plot <- function(df){
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
  df.cc <- df %>% select(Participant_ID, time, all_of(variables))
  
  # keep those based on medication diary
  #df.cc <- df[complete.cases(df[, variable]), ] # 7152
  
  # 1. Get count of ID's presented over time period (min = 1, max = 6) 
  id_counts <- table(df.cc$Participant_ID)
  
  # 2. Get ID's for people  who participated in all waves 
  id_all_waves <- names(id_counts[id_counts == 6])
  
  # 3. Subset data by these people (N = 4842)
  df.cc <- df.cc[df.cc$Participant_ID %in% id_all_waves,] # 4830
  
  # Creates summary table and plots
  t <- lapply(variables, util_percent_ci_all, times=0:5, df=df.cc, outcome="Yes")
  t2 <- bind_rows(t, .id = "column_label")
  
  t2$Condition <- ""
  for (i in 1:length(names)){
    t2$Condition[t2$variable == variables[i]] <- names[i]
  }
  return(t2)
}
#=============================================================================
create_su_dep_proportion_plot_data <- function(df){
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
  #t1 <- util_percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=1)
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

#create_su_proportion_tbl #same as above i think
#=============================================================================
create_su_plot_data <- function(df, var){
  outcome <- df[df$time==0, var]
  outcome <- na.omit(outcome)
  return(outcome)
}
##############################################################################
# TAB 10: medication diary # ** Selections may be difficult
# There are two components of this tab:
## (1) proportions
## (2) OME summary

percent_ci <- function(df, variable, outcome="Yes", time_ind=0){
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
  df <- subset(df, time == time_ind)
  df <- df[!is.na(df$totalopioiddose), ]
  # pci <- percent_ci(df, "Male", "T1_sex", outcome="Male")
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
  result <- c(estimate, lower, upper, time_ind, observed, n)
  # return results
  return(result)
}

proportion_make <- function(df, variable, outcome = "Yes"){
  # get complete case data
  df <- get_complete_case(df = df, variable = 'totalopioiddose')
  ##############################################################################
  t0 <- percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=0)
  t1 <- percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=1)
  t2 <- percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=2)
  t3 <- percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=3)
  t4 <- percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=4)
  t5 <- percent_ci(df=df, variable=variable,  outcome=outcome, time_ind=5)
  
  tall <- as.data.frame(rbind(t0,t1,t2,t3,t4,t5))
  names(tall)[names(tall) == "V1"] <- 'Proportion'
  names(tall)[names(tall) == "V2"] <- 'Lower CI'
  names(tall)[names(tall) == "V3"] <- 'Upper CI'
  names(tall)[names(tall) == "V4"] <- 'Time'
  names(tall)[names(tall) == "V5"] <- 'Users'
  names(tall)[names(tall) == "V6"] <- 'Diaries'
  
  # reorder so time is at the front
  tall <- tall[,c(4,1,2,3,5,6)]
  tall$Drug <- variable
  ##############################################################################
  return(tall)
}

# medication ome
medication_ome_data <- function(df, variable){
  # get complete case data
  df <- get_complete_case(df = df, variable = 'totalopioiddose')
  tmp <- df %>% select(time, totalopioiddose, all_of(variable))
  names(tmp)[names(tmp) == variable] <- 'drug'
  
  tmp[,3] <- as.integer(tmp[,3])-1
  # subset for only those drug users 
  tmp2 <- subset(tmp, drug == 1)
  tmp2 <- tmp2[complete.cases(tmp2),]
  
  # calculate the mean ome and plot it 
  t0 <- mean(tmp2$totalopioiddose[which(tmp2$time == 0)], na.rm=T)
  t1 <- mean(tmp2$totalopioiddose[which(tmp2$time == 1)], na.rm=T)
  t2 <- mean(tmp2$totalopioiddose[which(tmp2$time == 2)], na.rm=T)
  t3 <- mean(tmp2$totalopioiddose[which(tmp2$time == 3)], na.rm=T)
  t4 <- mean(tmp2$totalopioiddose[which(tmp2$time == 4)], na.rm=T)
  t5 <- mean(tmp2$totalopioiddose[which(tmp2$time == 5)], na.rm=T)
  
  time <- c(0:5)
  means <- c(t0,t1,t2,t3,t4,t5)
  
  results <- data.frame(Time = time, OME = means)
  results$Drug <- variable
  return(results)
}

##############################################################################
# TAB 11: DICTIONARY = SEE BELOW

##############################################################################
# TAB 12: PUBLISHED  = NOT APPLICABLE

##############################################################################
## RUN
source("functions/utilities.R")
load("data/raw/point-v1.0.0.RData")
point <- subset(point, followup=='followed up') # remove attrition N=7578

### TESTS
# DEMOGRAPHICS
demographics_data <- create_demographics_data(df=point)

# PAIN 
pain_baseline_data <- create_pain_baseline_data(df = point)
pain_past12m_data <- create_pain_past12m_data(df = point)
pain_bpi_data <- create_pain_bpi_data(df=point)

# PHYSICAL FUNCTION
pf_ex_days_data <- create_pf_ex_days_data(df=point)
pf_ex_in_data <- create_pf_ex_in_data(df=point)
pf_ex_tp_data <- create_pf_ex_tp_data(df=point)
pf_pseq_data <- create_pf_pseq_data(df=point)
pf_slp_data <- create_pf_slp_data(df=point)

# TREATMENT
# apply to all orbit items
orbit_items <- c('orb_1', 'orb_2', 'orb_3', 'orb_4', 'orb_5', 'orb_6', 'orb_7', 'orb_8', 'orb_9', 'orb_10')
tmt_orbit_data <- lapply(orbit_items, create_tmt_orbit_data, df=point)
tmt_orbit_data <- list_to_df(tmt_orbit_data)

# QOL 
qol_q1_data_results <- create_qol_q1_data(df=point)
qol_q1_data <- qol_q1_data_results[[1]]
qol_q1_n <- qol_q1_data_results[[2]]
rm(qol_q1_data_results)

qol_q2_data_results <- create_qol_q2_data(df=point)
qol_q2_data <- qol_q2_data_results[[1]]
qol_q2_n <- qol_q2_data_results[[2]]
rm(qol_q2_data_results)

# MENTAL HEALTH
mh_ever_plot_data <- create_mh_ever_plot_data(df=point)
mh_drug_trend_data_results <- create_mh_drug_trend_data(df=point)
mh_drug_trend_data <- mh_drug_trend_data_results[[1]]
mh_drug_trend_n <- mh_drug_trend_data_results[[2]]
rm(mh_drug_trend_data_results)

# SUBSTANCE USE
su_ever_plot_data <- create_su_ever_plot(df=point)
su_drug_trend_plot_data <- create_su_drug_trend_plot(df=point)
su_dep_proportion_plot_data <- create_su_dep_proportion_plot_data(df=point)

# this will be the same for orbit items but larger
su_items <- dictionary$Variable[dictionary$Subcategory == "Drug and alcohol abuse dependence"]
su_items <- su_items[!is.na(su_items)]

# find the values that start with num, e.g. num_lifetime_S8_opioids (not usefule here)
idx <- grep("num_", su_items, value = T) # n = 5
su_items <- su_items[!(su_items %in% idx)] # n = 161

# grab items
su_plot_data <- point %>% select(time, all_of(su_items))
su_plot_data <- subset(su_plot_data, time == 0)

# MEDICATION DIARY
## We first need to get the list of medications
medications <- dictionary_dictionary_data$Variable[dictionary_dictionary_data$Subcategory == "Drug"]
medications <- as.character(medications)
medications <- medications[!is.na(medications)]

medication_proportion_data_list <- lapply(medications, proportion_make, df=point) # takes about 5min
medication_proportion_data <- list_to_df(medication_proportion_data_list) # takes about

# remove rn from data
#medication_proportion_data <- medication_proportion_data[, c(2,3,4,5,6,7,8)]

# OME DATA
medication_ome_data_list <- lapply(medications, medication_ome_data, df=point) # takes about 5min
medication_ome_data <- list_to_df(medication_ome_data_list) # takes about

rm(medication_proportion_data_list, medication_ome_data_list)

# DATA DICTIONARY
dictionary_dictionary_data <- dictionary
dictionary_values_data <- values
rm(point, dictionary, values)

# Published data
published_data <- read.csv("data/raw/published_data.csv", fileEncoding = 'UTF-8-BOM') # section 12: published papers

# save data
summarised_data <- 'test_all'
data_dir = 'data/processed/'
file_name <- paste0(data_dir, summarised_data, ".RData")
save.image(file=file_name)

# to do: treatment, substance use, medication diary
##############################################################################
#################################### END #####################################
##############################################################################
