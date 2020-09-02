##############################################################################
# Purpose: POINT Dashboard -> SECTION 05: Physical Function
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################
## this section has 3 line plots over time and 2 bar charts 

## EXERCISE 
# NOTE: Exercise variables not collected at year 1

# DEPENDENCE
pf_ex_days <- function(df){
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
  
  y_label <- paste0("Percentage of Participants (%) (N=", table(tmp$time)[1], ")")
  p <- 
    ggplot(t, aes(x=time, y = prop, group = days, color= days)) + 
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
pf_ex_in <- function(df){
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
  y_label <- paste0("Percentage of Participants (%) (N=", table(tmp$time)[1], ")")
  
  p <- 
    ggplot(t, aes(x=Var1, y = prop, group = Intensity, color= Intensity)) + 
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
pf_ex_tp <- function(df){
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
 
  y_label <- paste0("Percentage of Participants (%) (N=", table(tmp$time)[1], ")")
  
   p <- 
    ggplot(t, aes(x=Var1, y = prop, group = Type, color= Type)) + 
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
pf_pseq <- function(df){
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
  
  y_label <- paste0("Mean PSEQ Score (N=", table(tmp$time)[1], ")")
  p <- 
    ggplot(pseq, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci)) +
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
pf_slp <- function(df){
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
  
  y_label <- paste0("Mean Sleep Score (N=", table(tmp$time)[1], ")")
  p <- 
    ggplot(slp, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci)) +
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