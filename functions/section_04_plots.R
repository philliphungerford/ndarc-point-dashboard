##############################################################################
# Purpose: POINT Dashboard -> SECTION 4: PAIN
# Author: Phillip Hungerford
# Date: 2020-07-30
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
  t <- round((colSums(tmp, na.rm=T) / nrow(tmp))*100,2)
  
  t <-t[2:9]
  t <- as.data.frame(t)
  
  library(data.table)
  t <- setDT(t, keep.rownames = TRUE)[]
  
  t$Condition <- ""
  for (i in 1:length(chronic_names)){
    t$Condition[t$rn == variables[i]] <- chronic_names[i]
  }
  
    
  p <- 
    ggplot(t, aes(x=Condition, weight=t, fill=Condition)) + 
    geom_bar() + 
    ylab("Proportion of POINT participants (%)") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + ylim(0,100)
  return(p)
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
  t2 <- bind_rows(t, .id = "column_label")
  
  t2$Condition <- ""
  for (i in 1:length(chronic_names)){
    t2$Condition[t2$variable == chronic_variables[i]] <- chronic_names[i]
  }

  
  # Create plot from summary table data
  y_label <- paste0("Percentage of Participants (%) (N=", table(df.cc$time)[1], ")")
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

# PLOT 3: Line graph of BPI interference and severity
pain_bpi_plot <- function(df){
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

  y_label <- paste0("Mean score (out of 10) (N=", table(tmp$time)[1], ")")
  p <- 
    ggplot(mydata, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci, group=Pain)) +
    geom_ribbon(aes(fill=Pain), alpha=0.4) +
    geom_line(aes(color=Pain), size=1) + 
    geom_point(aes(color=Pain), size=2) + 
    ylim(0,10) + 
    labs(title ="",
         x = "Years",
         y = y_label)
  p
  return(p)
}

# PLOT 4: Summary of BPI scores 
pain_bpi_tbl <- function(df){
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
  
  mydata <- mydata[, c(8,1,2,3,4,5,6,7)]
  return(mydata)
}

##############################################################################
#################################### END #####################################
##############################################################################
