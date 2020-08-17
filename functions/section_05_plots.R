##############################################################################
# Purpose: POINT Dashboard -> SECTION 05: Physical Function
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################
## this section has 3 line plots over time and 2 bar charts 

## EXERCISE 
# NOTE: Exercise variables not collected at year 1

# Line plot for days used
table(df$time, df$ex_days)



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
  
  tmp <- df %>% select(time, all_of(variables))
  # some days are in 0.5 e.g. 1.5 days, here I will round down being conservative
  tmp[tmp == 9] <- NA
  
  tmp <- na.omit(tmp)
  
  tmp$ex_days <- as.integer(tmp$ex_days)
  tmp$ex_days <- as.factor(tmp$ex_days)
  
  t <- as.data.frame(table(tmp$time, tmp$ex_days))
  names(t)[names(t) == "Var1"] <- "time"
  names(t)[names(t) == "Var2"] <- "days"
  
  # remove time 1 because it was not collected
  t <- t[which(t$time != 1), ]
  
  # for proportions
  t2 <- table(tmp$time)
  t2 <- table(df$time)
  
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
  
  p <- 
    ggplot(t, aes(x=time, y = prop, group = days, color= days)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    #ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = "Percentage of Participants (%)")
  
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
  
  tmp <- df %>% select(time, all_of(variables))
  
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
  
  # rename outcome
  names(t)[names(t) == "Var2"] <- "Intensity"
  p <- 
    ggplot(t, aes(x=Var1, y = prop, group = Intensity, color= Intensity)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    #ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = "Percentage of Participants (%)") 
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
  
  tmp <- df %>% select(time, all_of(variables))
  
  tmp <- na.omit(tmp) # 2366
  
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
 
   p <- 
    ggplot(t, aes(x=Var1, y = prop, group = Type, color= Type)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    #ylim(0,100) + 
    labs(title="",
         x = "Years",
         y = "Percentage of Participants (%)") 
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
  
  tmp <- df %>% select(time, all_of(variables))
  
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
  
  # remove time 1 because it was not collected then 
  pseq <- pseq[which(pseq$time != 1),]
  
  p <- 
    ggplot(pseq, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci)) +
    geom_ribbon(alpha=0.4, fill = 'red') +
    geom_line(size=1, color = 'red') + 
    geom_point(size=2, color = 'red') + 
    labs(title ="",
         x = "Years",
         y = "Mean PSEQ Score")
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
  
  tmp <- df %>% select(time, all_of(variables))
  
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
  
  # remove time 1 because it was not collected then 
  pseq <- pseq[which(pseq$time != 1),]
  
  p <- 
    ggplot(slp, aes(x=time, y=smean, ymin=lower_ci, ymax=upper_ci)) +
    geom_ribbon(alpha=0.4, fill = 'purple') +
    geom_line(size=1, color = 'purple') + 
    geom_point(size=2, color = 'purple') + 
    labs(title ="",
         x = "Years",
         y = "Mean Sleep Score")
  p
  return(p)
}

##############################################################################
#################################### END #####################################
##############################################################################