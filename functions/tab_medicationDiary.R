##############################################################################
# Purpose: POINT Dashboard -> SECTION 10: MEDICATION DIARY
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################
medication_proportion_table <- function(medication_proportion_data, variable){
  drug_data <- medication_proportion_data[which(medication_proportion_data$Drug == variable),] 
  return(drug_data)
}
#=============================================================================
medication_proportion_plot <- function(medication_proportion_data, variable){

  # subset the data based on chosen variable
  drug_data <- medication_proportion_data[which(medication_proportion_data$Drug == variable),] 
  
  y_label <- paste0("Proportion of ", variable," Users")
  p <- ggplot(data=drug_data, aes(x=Time, y=Proportion, ymin=`Lower CI`, ymax=`Upper CI`, group=1))+
    #geom_line(size = 8, colour="black",) +
    #geom_df(size=1, color='blue') +
    geom_ribbon(alpha=0.43, fill='blue') +
    geom_line(size=2, color='blue') +
    labs(title="",
         x = "Time",
         y = y_label) + theme_bw()
return(p)
}
#=============================================================================
medication_ome_plot <- function(medication_ome_data, variable){
  
  # subset data for given variable
  drug_data <- medication_ome_data[which(medication_ome_data$Drug == variable),] 
  
  ##############################################################################
  y_label <- paste0("Mean oral morphine equivalent for \n", variable," users across time")
  p <- ggplot(data=drug_data, aes(x=Time, y=OME))+
    geom_line(size=2, color='red') +
    labs(title="",
         x = "Time",
         y = y_label) + theme_bw()
  return(p)
}
#=============================================================================
medication_ome_summary <- function(medication_ome_data, variable){
  # subset data for given variable
  drug_data <- medication_ome_data[which(medication_ome_data$Drug == variable),] 
  return(drug_data)
}
##############################################################################
#################################### END #####################################
##############################################################################