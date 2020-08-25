##############################################################################
# Purpose: POINT Dashboard -> SECTION 4: PAIN
# Author: Phillip Hungerford
# Date: 2020-07-30
##############################################################################
# PLOT 1: Bar chart of orbit trends over time
# This is a bar plot of baseline presentation of chronic pain problems. 

tmt_orbit_plot_1 <- function(df, variables='orb_1'){
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
  #variables='orb_1'
  tmp <- df %>% select(Participant_ID, time, all_of(variables))
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
  
  # rename outcome
  names(t)[names(t) == "Var2"] <- "Item"
  
  # colours
  colours <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e')
  
  y_label <- paste0("Percentage of Participants (%) (N=", table(tmp$time)[1], ")")
  
  p <- 
    ggplot(t, aes(x=Var1, y = prop, group = Item, color= Item)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    scale_color_manual(values = colours) + 
    ylim(0,100) +
    labs(title="",
         x = "Years",
         y = y_label) 
  p
  return(p)
}

tmt_orbit_plot_2 <- function(df, variables='orb_1'){
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
  #variables='orb_1'
  tmp <- df %>% select(Participant_ID, time, all_of(variables))
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
  
  t <- t[which(t$Var2 != "Never"),]
  
  # rename outcome
  names(t)[names(t) == "Var2"] <- "Item"
  
  colours <- c('#d95f02','#7570b3','#e7298a','#66a61e')
  
  y_label <- paste0("Percentage of Participants (%) (N=", table(tmp$time)[1], ")")
  
  p <- 
    ggplot(t, aes(x=Var1, y = prop, group = Item, color= Item)) + 
    geom_line(size=1) + 
    geom_point(size=2) +
    scale_color_manual(values = colours) + 
    labs(title="",
         x = "Years",
         y = y_label) 
  p
  return(p)
}
##############################################################################
#################################### END #####################################
##############################################################################
