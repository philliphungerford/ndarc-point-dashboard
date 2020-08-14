##############################################################################
# Purpose: POINT data visualisation
# Author: Phillip Hungerford
# Date: 2020-06-19
##############################################################################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 
# Dashboard help: 
# https://rstudio.github.io/shinydashboard/structure.html
#
# Adding text to your shiny app
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
#
# Icons:
# https://fontawesome.com/v4.7.0/icons/
##############################################################################
# Import libraries
library(dplyr)
library(haven)
library(shiny)
library(shinydashboard) # for tabs
library(DT) # for displaying tables
library(ggplot2)
library(data.table)
# functions for plots
source("functions/utilities.R")
source("functions/section_02_plots.R")
#3
source("functions/section_04_plots.R")
#5
#6
source("functions/section_07_plots.R")
source("functions/section_08_plots.R")
source("functions/section_09_plots.R")
source("functions/section_10_plots.R")
##############################################################################
# load data
#point_master <- read_sav("../versions/point-v0.9.1.9.sav")
#point <- subset(point_master, followup==1) # remove attrition

point_master <- read.csv("data/point-v0.9.5.csv", na.strings=c("", " "))
point <- subset(point_master, followup=='followed up') # remove attrition N=7578

# SECTION 2: Measures
table_measures <- read.csv("data/measures-converted.csv")


# SECTION 3: Demographics (use class() to check dtype)
demographic_int <- c("Age" = "age", # int
                      "Height" = "height", # int
                      "Weight" =  "weight", # int
                      "Body Mass Index" = "bmi", # int
                      "Number of people living with" = "Live_Count") # int

                     
demographic_fac <- c("Sex" = "sex", # fact
                     "Education" = "edu", # fac
                     "Country of birth" = "cob", # fac
                     "Indigenous" = "indig", # fac
                     "Marital Status" = "maritalstatus", # fac
                     " Employment" = "Employ", # fac
                     "Weekly income" = "income_wk", # fac
                     "Accomodation status" = "accom" # fac
)

# SECTION X : Data Dictionary
data_dictionary <- read.csv("data/point-v0.9.5-dictionary.csv")
values_dictionary <- read.csv("data/point-v0.9.5-dictionary-values.csv", na.strings=c(""))
values_dictionary$Variable <- zoo::na.locf(values_dictionary$Variable)

# Medication vars 
medications <- data_dictionary$Variable[data_dictionary$Subcategory == "Drug"]
medications <- as.character(medications)
medications <- as.factor(unique(medications)) # 187 drugs

# substance use options
substance_use_options <- data_dictionary$Variable[data_dictionary$Subcategory == "Drug and alcohol abuse dependence"]
substance_use_options <- as.character(substance_use_options)
substance_use_options <- as.factor(unique(substance_use_options)) # 184 options

# Parameters
box_height = "height:300px"
select_height = "height:100px"
##############################################################################
# SECTION 1: USER INTERFACE
##############################################################################
# Define UI for application that draws a histogram
ui <- dashboardPage(
    #=========================================================================
    # START DASHBOARD
    #=========================================================================
    # MAIN TITLE
    dashboardHeader(title = "POINT Dashboard"),
    #=========================================================================
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            # icons from (https://fontawesome.com/v4.7.0/icons/)
            menuItem("Overview", tabName = "overview", icon = icon("columns")),#
            menuItem("Measures", tabName = "measures", icon = icon("dashboard")),#
            menuItem("Demographics", tabName = "demographics", icon = icon("address-book")),#
            menuItem("Pain", tabName = "pain", icon = icon("heartbeat")),#
            menuItem("Physical functioning", tabName = "physical", icon = icon("wheelchair")),#
            menuItem("Treatment", tabName = "treatment", icon = icon("hospital-o")),# 
            menuItem("Quality of life", tabName = "qol", icon = icon("heart")),#
            menuItem("Mental health", tabName = "mental_health", icon = icon("smile-o")),#
            menuItem("Substance use", tabName = "substance_use", icon = icon("toggle-on")),# or try eyedropper
            menuItem("Medication diary", tabName = "med_diary", icon = icon("medkit")),#
            menuItem("Data Dictionary", tabName = "dictionary", icon = icon("search")),#
            menuItem("Published Papers", tabName = "published", icon = icon("file-word-o")),#
            menuItem("Acknowledgements", tabName = "acknowledgements", icon = icon("info"))#
        )
    ),
    #=========================================================================
    ## Body content
    dashboardBody(
        tabItems(
            #-----------------------------------------------------------------
            # SECTION ONE: Overview
            tabItem(tabName = "overview",
                    h1("The Pain and Opioids In Treatment (POINT) study"),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    fluidRow(
                        # Participants = 1514
                        valueBox(value = 1514, "Participants", icon = icon("male")),
                        
                        # Years collected
                        valueBox(value = 6, "Years Collected", icon = icon("line-chart"), color = "purple"),
                    ),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    h2("Background"),
                    p("Internationally, there is concern about the increased prescribing of pharmaceutical opioids for chronic non-cancer pain (CNCP). In part, this is related to limited knowledge about the long-term benefits and outcomes of opioid use for CNCP. There has also been increased injection of some pharmaceutical opioids by people who inject drugs, and for some patients, the development of problematic and/or dependent use. To date, much of the research on the use of pharmaceutical opioids among people with CNCP, have been clinical trials that have excluded patients with complex needs, and have been of limited duration (i.e. fewer than 12 weeks). The Pain and Opioids In Treatment (POINT) study is unique study that aims to: 1) examine patterns of opioid use in a cohort of patients prescribed opioids for CNCP; 2) examine demographic and clinical predictors of adverse events, including opioid abuse or dependence, medication diversion, other drug use, and overdose; and 3) identify factors predicting poor pain relief and other outcomes."),
                    
                    h2("Methods/Design"),
                    p("The POINT cohort comprises around 1,500 people across Australia prescribed pharmaceutical opioids for CNCP. Participants will be followed-up at four time points over a two year period. POINT will collect information on demographics, physical and medication use history, pain, mental health, drug and alcohol use, non-adherence, medication diversion, sleep, and quality of life. Data linkage will provide information on medications and services from Medicare (Australia’s national health care scheme). Data on those who receive opioid substitution therapy, and on mortality, will be linked."),
                    
                    h2("Discussion"),
                    p("This study will rigorously examine prescription opioid use among CNCP patients, and examine its relationship to important health outcomes. The extent to which opioids for chronic pain is associated with pain reduction, quality of life, mental and physical health, aberrant medication behavior and substance use disorders will be extensively examined. Improved understanding of the longer-term outcomes of chronic opioid therapy will direct community-based interventions and health policy in Australia and internationally. The results of this study will assist clinicians to better identify those patients who are at risk of adverse outcomes and who therefore require alternative treatment strategies.")
                    
            ),
            #-----------------------------------------------------------------
            # SECTION TWO: MEASURES
            tabItem(tabName = "measures",
                    h2("Measures"),
                    p("Here are the measures, tools, domains and time-points for data collection for the POINT study.
                      Taken from Table 2 of the POINT Protocol."),
                    # Display measures table
                    DT::dataTableOutput("table_measures")
            ),
            #-----------------------------------------------------------------
            # SECTION THREE: DEMOGRAPHICS
            tabItem(tabName = "demographics",
                    h2("Demographics"),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    fluidRow(
                        # box 1 and 2 for selection
                        box(
                            style = select_height,
                            title = "Select continuous variable",
                            "Select a variable from", br(), "",
                            selectInput(inputId = "demographic_int_selection", label = "Variable:", choices = demographic_int)),
                        box(
                            style = select_height,
                            title = "Select discrete variable",
                            "Select a variable from", br(), "",
                            selectInput(inputId = "demographic_fac_selection", label = "Variable:", choices = demographic_fac))
                    ),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    fluidRow(
                        # box 3 is the Histogram
                        box(
                            style = box_height,
                            title = "Density plot",
                            plotOutput("demographic_density", height = 250)),
                        # box 4 is the summary
                        box(
                            style = box_height,
                            title = "Pie chart",
                            plotOutput("demographic_donut", height = 250))
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    fluidRow(
                        # box 5 is the histogram
                        box(
                            style = box_height,
                            title = "Histogram",
                            plotOutput("demographic_histogram", height = 250)),
                        # box 6 is the summary for pie chart
                        box(
                            style = box_height,
                            title = "Summary plot",
                            tableOutput("demographic_sum"))
                    )
                    
            ),
            #-----------------------------------------------------------------
            # SECTION FOUR: PAIN
            tabItem(tabName = "pain",
                    h2("Pain Measures"),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    fluidRow(
                        # BOX 1: Histogram of bar
                        box(
                            style = box_height,
                            title = "Chronic conditions at baseline",
                            plotOutput("pain_baseline", height = 250)),
                        # BOX 2: Histogram of bar
                        box(
                            style = box_height,
                            title = "Past 12m reported chronic conditions",
                            plotOutput("pain_past12m", height = 250))
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    fluidRow(
                        # BOX 3: Line graph BPI interference / severity
                        box(
                            style = box_height,
                            title = "Brief Pain Inventory: Interference & Severity",
                            plotOutput("pain_bpi_p", height = 250)),
                        
                        # BOX 4: table of BPI interference / severity
                        box(
                            style = box_height,
                            title = "Brief Pain Inventory Summaries over Time",
                            tableOutput("pain_bpi_t"))
                    )
            ),
            #-----------------------------------------------------------------
            # SECTION FIVE: PHYSICAL FUNCTION
            tabItem(tabName = "physical",
                    h2("Physical Function Measures")
            ),
            #-----------------------------------------------------------------
            # SECTION SIX: TREATMENT
            tabItem(tabName = "treatment",
                    h2("Treatment Received")
            ),
            #-----------------------------------------------------------------
            # SECTION SEVEN: QOL
            tabItem(tabName = "qol",
                    h2("Quality of Life Assessment"),
                    p("The following questions ask how you feel about your quality of life, health, or other areas of your life. I will read out each question to you, along with the response options. Please choose the answer that appears most appropriate. If you are unsure about which response to give to a question, the first response you think of is often the best one."),
                    br(),
                    p(" Please keep in mind your standards, hopes, pleasures and concerns. We ask that you think about your life in the last four weeks."),
                    br(),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    fluidRow(
                        # BOX 1: Quality of life plot
                        box(
                            style = box_height,
                            title = "How would you rate your quality of life?",
                            plotOutput("qol_q1", height = 500)),
                        # BOX 2: Health satisfaction plot
                        box(
                            style = box_height,
                            title = "How satisfied are you with your health?",
                            plotOutput("qol_q2", height = 500))
                    )
            ),
            #-----------------------------------------------------------------
            # SECTION EIGHT: MENTAL HEALTH
            tabItem(tabName = "mental_health",
                    h2("Mental Health"),
                    fluidRow(
                        
                        # BOX 1: Ever used (stacked plot of yes/no for each drug)
                        box(
                            style = box_height,
                            title = "Ever diagnosed",
                            plotOutput("mh_r1b1", height = 250)),
                        
                        # Box 2: Plot of past 12m use
                        box(
                            style = box_height,
                            title = "Past 12m presentation",
                            plotOutput("mh_r1b2", height = 250))
                    )
            ),
            #-----------------------------------------------------------------
            # SECTION NINE: SUBSTANCE
            tabItem(tabName = "substance_use",
                    h2("Substance Use"),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Top Row: Life time
                    h3("Lifetime and current drug use"),
                    fluidRow(
                        
                        # BOX 1: Ever used (stacked plot of yes/no for each drug)
                        box(
                            style = box_height,
                            title = "Ever Used",
                            plotOutput("substance_use_r1b1", height = 250)),
                        
                        # Box 2: Plot of past 12m use
                        box(
                            style = box_height,
                            title = "Past 12m Use",
                            plotOutput("substance_use_r1b2", height = 250))
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Second row: opioid dependence
                    h3("Opioid Abuse and Dependence"),
                    fluidRow(
                        # BOX 4: Opioid dependence plot
                        box(
                            style = box_height,
                            title = "ICD10 pharmaceutical opioid dependence",
                            plotOutput("substance_use_r2b1", height = 250)),
                        
                        # BOX 5: Opioid dependence summary
                        box(
                            style = box_height,
                            title = "ICD10 pharmaceutical opioid dependence Summary",
                            tableOutput("substance_use_r2b2"))
                    ),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Second row: Baseline drug dependence
                    h3("Drug and Alcohol Abuse and Dependence Measured at Baseline"),
                    fluidRow(
                        # BOX 6: Opioid dependence plot
                        box(
                            style = select_height,
                            title = "Select",
                            selectInput("substance_use_select", "Criteria:", substance_use_options))),
                        
                    fluidRow(
                        # BOX 7: Bar plot of yes no 
                        box(
                            style = "height:500px",
                            title = "Presentation",
                            plotOutput("substance_use_r3b2")),
                        
                        # BOX 8: Opioid dependence summary
                        box(
                            style = "height:500px",
                            title = "Summary",
                            tableOutput("substance_use_r3b3"))
                    )
            ),
            #-----------------------------------------------------------------
            # SECTION TEN: MEDICATION DIARY
            tabItem(tabName = "med_diary",
                    h2("Medication Diary"),
                    p("At each wave, a seven-day medication diary collected frequency and dose information on all consumed pain-related medicines, psychiatric medicines and prescribed sleep medicines. The measures, tools, and data domains were selected based on recommendations made by the Initiative on Methods, Measurement, and Pain Assessment in Clinical Trials (IMMPACT). Based on the seven day medication diary of participants, explore the medication use measured in oral morphine equivalent (OME), usage across the six years."),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    fluidRow(
                        # box 3 is the medication selection
                        box(
                            style = "height:110px",
                            title = "Select Medication",
                            "Select a medication from", br(), "participant's medication diary.",
                            selectInput("medication", "Variable:", medications))
                    ),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    fluidRow(
                        # box 1 is the medication summary
                        box(
                            style = box_height,
                            title = "Proportion of users Plot",
                            plotOutput("medication_plot", height = 250)),
                        # box 2 is the plot
                        box(
                            style = box_height,
                            title = "Proportion Summary",
                            tableOutput("medication_proportions"))
                    ),
                   
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    fluidRow(
                        # box 1 is the medication summary
                        box(
                            style = box_height,
                            title = "Mean Oral Morphine Equivalent over study",
                            plotOutput("medication_ome", height = 250)),
                        # box 3 is the medication selection
                        box(
                            style = box_height,
                            title = "OME Summary",
                            tableOutput("medication_ome_summary"))
                    )
            ),
            #-----------------------------------------------------------------
            # Eleventh tab content: Data Dictionary
            tabItem(tabName = "dictionary",
                    h2("POINT Data Dictionary"),
                    
                    # have two sub-tabs within the data dictionary tab
                    tabsetPanel(
                                tabPanel("Data Dictionary", DT::dataTableOutput("data_dictionary")), 
                                tabPanel("Variable Values", DT::dataTableOutput("values_dictionary"))
                    ),
            ),
            #-----------------------------------------------------------------
            # Twelfth tab content
            tabItem(tabName = "published",
                    h2("Published Papers")
            ),
            #-----------------------------------------------------------------
            # Thirteenth tab content
            tabItem(tabName = "acknowledgements",
                    h2("Acknowledgements"),
                    p("A special thanks to all of the research assistants, chief investigators and most of all the participants who have contributed to this work."),
                    br(),
                    em("This dashboard was made my Phillip Hungerford at the National Drug and Alcohol Research Centre (NDARC). More details about the code used to make this dashboard can be found on GitHub (https://github.com/philliphungerford/ndarc-point-dashboard)"),
                    br(),
                    p("The National Drug and Alcohol Research Centre (NDARC) is a premier research institution in Sydney, Australia and is recognised internationally as a Research Centre of Excellence. NDARC was established at UNSW Sydney in May 1986 and officially opened in November 1987. The Centre is supported by funding from the Australian Government Department of Health under the Drug and Alcohol Program."),
                    p("https://ndarc.med.unsw.edu.au"),
                    img(src='ndarc.png', align = "bottom")
                    )
            #-----------------------------------------------------------------
        ) # tabItems
    ) # body
    #=========================================================================
    # END DASHBOARD
    #=========================================================================
) # dashboard page

##############################################################################
# SECTION 2: SERVER
##############################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
    #=========================================================================
    # Start server
    #=========================================================================
    # set seed for replicability
    set.seed(122)
    
    #=========================================================================
    # SECTION 2: MEASURES
    output$table_measures = DT::renderDataTable({
        DT::datatable(table_measures, options = list(lengthMenu = c(10, 20, 43), pageLength = 43))
    })
    
    #=========================================================================
    # SECTION 3: Demographics
    #-------------------------------------------------------------------------
    # PLOT 1: Density plot
    output$demographic_density <- renderPlot({
        density_plot(df = point, variable = input$demographic_int_selection)
    })
    #-------------------------------------------------------------------------
    # PLOT 2: Donut
    output$demographic_donut <- renderPlot({
        donut_plot(df = point, variable = input$demographic_fac_selection)
    })
    #-------------------------------------------------------------------------
    # PLOT 3: Histogram
    output$demographic_histogram <- renderPlot({
        histogram_plot(df = point, variable = input$demographic_int_selection)
    })
    #-------------------------------------------------------------------------
    # PLOT 4: Summary of pie chart
    output$demographic_sum <- renderTable({
        donut_summary(point, input$demographic_fac_selection)
    })
    
    #=========================================================================
    # SECTION 4: Pain
    #-------------------------------------------------------------------------
    # PLOT 1: BASELINE CHRONIC PAIN CONDITIONS
    output$pain_baseline <- renderPlot({
        pain_baseline_plot(df=point)
    })
    
    # PLOT 2: PAST 12m CHRONIC PAIN CONDITIONS
    output$pain_past12m <- renderPlot({
        pain_past12m_plot(df=point)
    })
    
    # PLOT 3: BPI plot
    output$pain_bpi_p <- renderPlot({
        pain_bpi_plot(df=point)
    })
    
    # Box 4: BPI summary
    output$pain_bpi_t <- renderTable({
        pain_bpi_tbl(df=point)
    })
    #=========================================================================
    # SECTION 05: Physical Function
    #-------------------------------------------------------------------------
    
    #=========================================================================
    # SECTION 06: Treatment
    #-------------------------------------------------------------------------
    
    #=========================================================================
    # SECTION 07: Quality of life
    #-------------------------------------------------------------------------
    # PLOT 1: Quality of life
    output$qol_q1 <- renderPlot({
        qol_q1_plot(df=point)
    })
    # PLOT 2: Health satisfaction
    output$qol_q2 <- renderPlot({
        qol_q2_plot(df=point)
    })
    
    #=========================================================================
    # SECTION 08: Mental Health
    #-------------------------------------------------------------------------
    ## ROW 1 BOX 1: 
    output$mh_r1b1 <- renderPlot({
        mh_ever_plot(point)
    })
    
    ## ROW 1 BOX 3: Past 12m 
    output$mh_r1b2 <- renderPlot({
        mh_drug_trend_plot(point)
    })
    
    #=========================================================================
    # SECTION 09: Substance Use
    #-------------------------------------------------------------------------
    # ROW 1: lifetime and current drug use
    
    ## ROW 1 BOX 1: 
    output$substance_use_r1b1 <- renderPlot({
        su_ever_plot(point)
    })
    
    ## ROW 1 BOX 3: Past 12m 
    output$substance_use_r1b2 <- renderPlot({
        su_drug_trend_plot(point)
    })
    
    
    # ROW 2: Opioid dependence
    
    output$substance_use_r2b1 <- renderPlot({
        su_proportion_plot(point, 'Pharm_Opioids_Dep_ICD10', outcome = "Yes")
    })
    
    output$substance_use_r2b2 <- renderTable({
        su_proportion_tbl(point, 'Pharm_Opioids_Dep_ICD10', outcome = "Yes")
    })
    
    # ROW 3: Drug abuse and dependence
    output$substance_use_r3b2 <- renderPlot({
        su_plot(point, input$substance_use_select)
    })
    
    output$substance_use_r3b3 <- renderTable({
        table(point[which(point$time==0), input$substance_use_select])
    })
    
    
    #=========================================================================
    # SECTION 10: medication diary
    #-------------------------------------------------------------------------
    # Proportions Summary
    output$medication_proportions <- renderTable({
        proportion_make(point, input$medication, outcome = "Yes")
    })
    #-------------------------------------------------------------------------
    # Proportion plot
    output$medication_plot <- renderPlot({
        proportion_plot(point, input$medication, outcome = "Yes")
    })
    #-------------------------------------------------------------------------
    # OME plot
    output$medication_ome <- renderPlot({
        ome_plot(point, input$medication)
    })
    #-------------------------------------------------------------------------
    # OME Summary
    output$medication_ome_summary <- renderTable({
        ome_summary(point, input$medication)
    })
    #=========================================================================
    # SECTION ELEVEN: Data Dictionary
    output$data_dictionary = DT::renderDataTable({
        DT::datatable(data_dictionary, options = list(lengthMenu = c(100, 500, 1000, nrow(data_dictionary)), pageLength = 100))
    })
    output$values_dictionary = DT::renderDataTable({
        DT::datatable(values_dictionary, options = list(lengthMenu = c(100, 500, 1000, nrow(values_dictionary)), pageLength = 100))
    })
    #=========================================================================
    # End server
    #=========================================================================
}

##############################################################################
# SECTION 3: RUN APPLICATION
##############################################################################
shinyApp(ui = ui, server = server)
##############################################################################
################################### END ######################################
##############################################################################