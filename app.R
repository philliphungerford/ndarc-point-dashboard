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
library(reshape2)
##############################################################################
# functions for plots
source("functions/utilities.R")
# 1 - overview
# 2 - measurs
source("functions/section_03_plots.R") # 3 - demographics
source("functions/section_04_plots.R") # 4 - pain
source("functions/section_05_plots.R") # 5 - physical function
source("functions/section_06_plots.R") # 6 - treatment
source("functions/section_07_plots.R") # 7 - quality of life
source("functions/section_08_plots.R") # 8 - mental health
source("functions/section_09_plots.R") # 9 - substance use
source("functions/section_10_plots.R") # 10 - medication diary
# 11 - data dictionary
# 12 - published
# 13 - acknowledgements
##############################################################################
# load data
point_master <- read.csv("data/point-v0.9.5.csv", na.strings=c("", " "), encoding='UTF-8')
names(point_master)[names(point_master) == "X.U.FEFF.Participant_ID"] <- "Participant_ID"
point <- subset(point_master, followup=='followed up') # remove attrition N=7578

# SECTION 2: Measures
table_measures <- read.csv("data/20200825-measures-converted.csv", fileEncoding='UTF-8-BOM')
    
# SECTION 10: Data dictionary
data_dictionary <- read.csv("data/point-v0.9.5-dictionary.csv")
values_dictionary <- read.csv("data/point-v0.9.5-dictionary-values.csv", na.strings=c(""))

# SECTION 12: Published
published_papers_dat <- read.csv("data/published-year-title.csv", fileEncoding = 'UTF-8-BOM')

##############################################################################
# fine tune parameters for each tab
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

# SECTION 06: Treatment
## Orbit
orbit_items <- c(
    'Q1. Asked my doctor for an increase in my prescribed dose' = 'orb_1',
    'Q2. Asked my doctor for an early renewal of my prescription' = 'orb_2',
    'Q3. Used another person’s opioid medication, or bought it from the street' = 'orb_3',
    'Q4. Saved up my opioid medication, just in case I needed it later' = 'orb_4',
    'Q5. Gone to a different doctor to get more opioid medication'= 'orb_5',
    'Q6. Asked my doctor for another prescription because either I had lost, had it stolen, or someone used it' = 'orb_6',
    'Q7. Given or sold my prescribed medication to someone else' = 'orb_7',
    'Q8. Altered my dose in some other way' = 'orb_8',
    'Q9. Taken my opioid medication by a different route than was prescribed' = 'orb_9',
    'Q10. Have used my opioid medication for other purposes' = 'orb_10'
)

# SECTION 09: SUbstance Use
# substance use options
substance_use_options <- data_dictionary$Variable[data_dictionary$Subcategory == "Drug and alcohol abuse dependence"]
substance_use_options <- as.character(substance_use_options)
substance_use_options <- as.factor(unique(substance_use_options)) # 184 options

# SECTION 10: Medication diary
# Medication vars 
medications <- data_dictionary$Variable[data_dictionary$Subcategory == "Drug"]
medications <- as.character(medications)
medications <- as.factor(unique(medications)) # 187 drugs

# SECTION 11 : Data Dictionary
values_dictionary$Variable <- zoo::na.locf(values_dictionary$Variable)

# Parameters
box_height = "height:600px"
plot_height = 400
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
                    #h1("The Pain and Opioids In Treatment (POINT) study"),
                    div(img(src='point_logo.jpg', align = "center"), style="text-align: center;"),
                    h1("Overview"),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    fluidRow(
                        # Drug type
                        valueBox(value = "Opioids", "Drug type", icon = icon("line-chart"), color = "orange"),
                        
                        # Participants = 1514
                        valueBox(value = 1514, "Participants", icon = icon("male"), color = "green"),
                        
                        # Years collected
                        valueBox(value = 6, "Years Collected", icon = icon("line-chart"), color = "purple"),
                        
                        # Date commenced
                        valueBox(value = "01/2012", "Date commenced", icon = icon("calendar-o"), color = "blue"),
                        
                        # Expected Date of Completion
                        valueBox(value = "12/2019", "Expected Date of Completion", icon = icon("calendar"), color = "blue"),
                        
                        # Project Supporters
                        valueBox(value = p("National Health & Medical Research Council", style = 'font-size: 60%'), "Project Supporters", icon = icon("institution"), color = "green")
                        

                    ),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    h2("Background"),
                    p("Internationally, there is concern about the increased 
                      prescribing of pharmaceutical opioids for chronic
                      non-cancer pain (CNCP). In part, this is related to 
                      limited knowledge about the long-term benefits and 
                      outcomes of opioid use for CNCP. There has also been 
                      increased injection of some pharmaceutical opioids by 
                      people who inject drugs, and for some patients, the
                      development of problematic and/or dependent use. To date,
                      much of the research on the use of pharmaceutical opioids 
                      among people with CNCP, have been clinical trials that 
                      have excluded patients with complex needs, and have been 
                      of limited duration (i.e. fewer than 12 weeks). The Pain 
                      and Opioids In Treatment (POINT) study is unique study 
                      that aims to: 1) examine patterns of opioid use in a 
                      cohort of patients prescribed opioids for CNCP; 2) 
                      examine demographic and clinical predictors of adverse
                      events, including opioid abuse or dependence, medication
                      diversion, other drug use, and overdose; and 3) identify
                      factors predicting poor pain relief and other outcomes."),
                    
                    
                    h2("Design and Method "),
                    p("The POINT study is a national prospective cohort that 
                      aims to follow 1500 chronic pain patients newly prescribed
                      pharmaceutical opioids over a 24 month period. Follow-ups 
                      have occurred at three months, 12 months and 24 months. 
                      POINT II follow ups will occur at 3, 4 and 5 years. The
                      interviews will cover topics such as; demographics, 
                      chronic pain, treatment, physical and mental health, 
                      physical functioning, social support and current and 
                      lifetime substance use. Participants were recruited 
                      through pharmacies throughout Australia. We will continue
                      to follow patients that discontinue their pharmaceutical 
                      opioid in order to examine reasons and effects of 
                      discontinuance. "),
                    
                    h2("Progress/Update "),
                    h3("POINT I"),
                    p("The POINT study was approved by the university ethics
                      committee in May, 2012 and National Pharmacy Guild in
                      July, 2012. Recruitment for the study began late August 
                      2012. Baseline interviews of 1,514 participants were 
                      completed in April 2014 and 3-month self-complete surveys
                      completed in September 2014, with a follow-up rate of 80%.
                      12-month surveys were completed in June 2015, with a 
                      follow-up rate of 82%. In December 2015 2- year interviews
                      were finished, with a follow up rate of 85%."),
                    
                    h3("POINT II"),
                    p("In July 2016, NHMRC awarded funding for the project: 
                      Pharmaceutical opioids for chronic non-cancer pain: 
                      Evaluating health outcomes and economic impact over
                      five years. The four-year study is an extension of 
                      POINT  and is expected to conclude in 2019, with a 
                      final report due in 2020. POINT II started January
                      2016 with participants completing a 30-45 minute 
                      interview over the phone. At the conclusion of 2016,
                      interviews had been conducted with 1,210 participants 
                      (80% of the sample). Four year interviews were initiated 
                      in January 2017 and 1,206 have been completed thus far, 
                      again reaching 80%. Five year interviews will begin in 
                      January 2018."),
                    
                    p("Through collating data over 5 years, POINT II aims to 
                    answer the following questions;"),
                    
                    p("1. What is the impact of opioids for CNCP on pain, 
                    functioning, quality of life, healthcare, and healthcare
                    costs over 5 years?"),
                    
                    p("2. What predicts transitions to and from opioid 
                    non-adherence and dependence over 5 years?"),
                    
                    p("3. What are CNCP patients preferences with respect to 
                      outcomes, side-effects, different medications and to 
                      non-medication approaches to pain management?"),
                   
                    h2("Benefits"),
                    p("The POINT project was the first large-scale Australian 
                    prospective cohort study to rigorously examine opioid 
                    analgesic prescribing patterns amongst chronic pain patients
                    at a population level, and their relationship to important
                    health outcomes and mortality. It was the first to 
                    comprehensively examine the extent, to which opioid therapy
                    for chronic pain is associated with pain reduction, adverse
                    events including side effects, quality of life, and mental 
                    and physical health outcomes."),
                    
                    p("Currently, the evidence base for the regulation and 
                    monitoring of opioid analgesics is weak. Regulators across
                    jurisdictions use different criteria for authorising 
                    long-term opioid therapy, and different criteria for
                    identifying at-risk patients. The results of this study will
                    assist doctors and regulators in Australia to better 
                    identify those patients who are at risk of adverse outcomes
                    and who therefore require alternative treatment strategies.
                    Improved understanding of the longer-term outcomes of
                    chronic opioid therapy will direct community-based
                    interventions and health policy in Australia."),
                   
                    p("Through another 3 years of follow up interviews, POINT II
                    will further be able to shed light on the extent to which 
                    patients experience problematic opioid use, some of the 
                    precursors and protective factors to problematic use, and 
                    the consequences of problematic opioid use resulting from
                    chronic opioid therapy. It will lead to improved knowledge 
                    of dose escalation and the positive and negative outcomes 
                    for those who undergo rapid dose escalation and ultimately
                    end up using high doses of opioid analgesics."),
                    
                    p("POINT II also aims to establish a long term trajectory of
                    potential impacts on health care use and costs. It will 
                    investigate patient preferences for interventions that
                    improve pain and functioning, providing insight into why
                    some ineffective but expensive treatments are used and why 
                    long term opioid use persists even in light of little 
                    clinical benefit."),
                    
                    p("Finally, the project will achieve the establishment of a
                      cohort of Australians with chronic health problems. 
                      The project will provide the groundwork for further 
                      follow-up of the sample to determine the longer-term 
                      outcomes for chronic pain patients."),
           
                    # Source for citation
                    h3("Protocol citation:"),
                    p("Campbell, G., Mattick, R., Bruno, R., 
                      Larance, B., Nielsen, S., Cohen, M., Lintzeris, N., 
                      Shand, F., Hall, W., Hoban, B., Kehler, C., Farrell, M.,
                      & Degenhardt, L. (2014). Cohort protocol: The Pain and 
                      Opioids IN Treatment (POINT) study. BMC Pharmacology and
                      Toxicology, 15; 17")
            ),
            #-----------------------------------------------------------------
            # SECTION TWO: MEASURES
            tabItem(tabName = "measures",
                    h2("Measures"),
                    p("Here are the measures, tools, domains and time-points for data collection for the POINT study.
                      Taken from Table 2 of the POINT Protocol."),

                    p("These measures were based on recommendations made under 
                      the auspices of the Initiative on Methods, Measurement, 
                      and Pain As- sessment in Clinical Trials (IMMPACT). This 
                      Initiative involved 27 specialists from academia, 
                      governmental agencies, and the pharmaceutical industry 
                      who partici- pated in a consensus meeting and identified
                      core out- come domains and measures that should be
                      considered in clinical trials of treatments for chronic
                      pain [1,2]. The draft content of our interview was also
                      reviewed and discussed by the POINT advisory committee."),
                    
                    h3("References:"),
                    p("1. Dworkin RH, Turk DC, Farrar JT, Haythornthwaite JA, 
                      Jensen MP, Katz NP, Kerns RD, Stucki G, Allen RR,
                      Bellamy N, Dworkin RH, Turk DC, Farrar JT, 
                      Haythornthwaite JA, Jensen MP, Katz NP, Kerns RD,
                      Stucki G, Allen RR, Bellamy N, Carr DB, Chandler J, 
                      Cowan P, Dionne R, Galer BS, Hertz S, Jadad AR, Kramer LD,
                      Manning DC, Martin S, et al: Core outcome measures for 
                      chronic pain clinical trials: IMMPACT recommendations. 
                      Pain 2005, 113(1–2):9–19."),

                    p("2. Turk DC, Dworkin RH, Allen RR, Bellamy N, 
                      Brandenburg N, Carr DB, Cleeland C, Dionne R, Farrar JT,
                      Galer BS, Turk DC, Dworkin RH, Allen RR, Bellamy N,
                      Brandenburg N, Carr DB, Cleeland C, Dionne R, Farrar JT,
                      Galer BS, Hewitt DJ, Jadad AR, Katz NP, Kramer LD, 
                      Manning DC, McCormick CG, McDermott MP, McGrath P, 
                      Quessy S, Rappaport BA, et al: Core outcome domains for 
                      chronic pain clinical trials: IMMPACT recommendations. 
                      Pain 2003, 106(3):337–345."),
                    
                    # Display measures table
                    div(DT::dataTableOutput(outputId = "table_measures", width = '100%', height = 'auto'), style = "font-size: 100%; width: 95%")
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
                            title = "Select categorical variable",
                            "Select a variable from", br(), "",
                            selectInput(inputId = "demographic_fac_selection", label = "Variable:", choices = demographic_fac))
                    ),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    fluidRow(
                        # box 3 is the Histogram
                        box(
                            style = "height:300px",
                            title = "Density plot",
                            plotOutput("demographic_density", height = 250)),
                        # box 4 is the summary
                        box(
                            style = "height:300px",
                            title = "Pie chart",
                            plotOutput("demographic_donut", height = 250))
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    fluidRow(
                        # box 5 is the histogram
                        box(
                            style = "height:450px",
                            title = "Histogram",
                            plotOutput("demographic_histogram", height = 250)),
                        # box 6 is the summary for pie chart
                        box(
                            style = "height:450px",
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
                            style = "height:450px",
                            title = "Brief Pain Inventory: Interference & Severity",
                            plotOutput("pain_bpi_p", height = 250)),
                        
                        # BOX 4: table of BPI interference / severity
                        box(
                            style = "height:450px",
                            title = "Brief Pain Inventory Summaries over Time",
                            tableOutput("pain_bpi_t"))
                    )
            ),
            #-----------------------------------------------------------------
            # SECTION FIVE: PHYSICAL FUNCTION
            tabItem(tabName = "physical",
                    h2("Physical Function Measures"),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Top Row: Exercise 
                    h3("Exercise"),
                    fluidRow(
                        
                        # BOX 1: Days exercising per week 
                        box(
                            style = box_height,
                            title = "Days per week exercising",
                            plotOutput("pf_r1b1", height = 250)),
                        
                        # Box 2: Exercise intensity (pie chart)
                        box(
                            style = box_height,
                            title = "Exercise Intensity",
                            plotOutput("pf_r1b2", height = 250)),
                        
                        
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Second row: exercise type

                    fluidRow(
                        # BOX 3: Exercise type (pie chart)
                        box(
                            style = box_height,
                            title = "Exercise type",
                            plotOutput("pf_r2b1"))
                    ),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Third Row: Sleep & PSEQ
                    h3("Pain Self Efficacy Questionnaire and Sleep"),
                    fluidRow(
                        # BOX 4: PSEQ (line graph)
                        box(
                            style = "height:500px",
                            title = "Pain Self Efficacy Questionnaire Score (PSEQ)",
                            plotOutput("pf_r3b1")),
                        
                        # BOX 5: Sleep (line graph)
                        box(
                            style = "height:500px",
                            title = "Sleep Score",
                            plotOutput("pf_r3b2"))
                    )
            ),
            #-----------------------------------------------------------------
            # SECTION SIX: TREATMENT
            tabItem(tabName = "treatment",
                    h2("Treatment Received"),
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    # Info boxes for Overview
                    h3("Opioid Related Behaviours In Treatment (ORBIT)"),
                    fluidRow(
                        # BOX 6: Opioid dependence plot
                        box(
                            style = select_height,
                            title = "Select ORBIT item",
                            selectInput("tmt_orbit_select", "In the past three months, I have:", orbit_items)),
                    
                    ),
                    ## ROW 2: OUTPUTS
                    fluidRow(
                        # BOX 6: Opioid dependence plot
                        box(
                            style = box_height,
                            title = "ORBIT responses",
                            plotOutput("tmt_r2b1", height = 500)),
                        
                        # BOX 2: Health satisfaction plot
                        box(
                            style = box_height,
                            title = "ORBIT responses (excluding 'Never')",
                            plotOutput("tmt_r2b2", height = 500))
                    )
                    
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
                                tabPanel("Data Dictionary", DT::dataTableOutput("data_dictionary_tab")), 
                                tabPanel("Variable Values", DT::dataTableOutput("values_dictionary_tab"))
                    ),
            ),
            #-----------------------------------------------------------------
            # Twelfth tab content
            tabItem(tabName = "published",
                    h2("Published Papers"),
                    # Display published papers table
                    DT::dataTableOutput("published_papers")
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
                    p("For more details about the POINT study visit: ", a("POINT study details", href="https://ndarc.med.unsw.edu.au/project/point-study-pain-and-opioids-treatment")),
                    p("For more details about NDARC visit: ", a("NDARC", href="https://ndarc.med.unsw.edu.au")),
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
    }, height = plot_height)
    
    # PLOT 2: PAST 12m CHRONIC PAIN CONDITIONS
    output$pain_past12m <- renderPlot({
        pain_past12m_plot(df=point)
    }, height = plot_height)
    
    # PLOT 3: BPI plot
    output$pain_bpi_p <- renderPlot({
        pain_bpi_plot(df=point)
    }, height = plot_height)
    
    # Box 4: BPI summary
    output$pain_bpi_t <- renderTable({
        pain_bpi_tbl(df=point)
    })
    #=========================================================================
    # SECTION 05: Physical Function
    #-------------------------------------------------------------------------
    # Exercise
    ## R1B1: Exercise days
    output$pf_r1b1 <- renderPlot({
        pf_ex_days(df=point)
    }, height = plot_height)
    ## R1B2: Exercise intensity
    output$pf_r1b2 <- renderPlot({
        pf_ex_in(df=point)
    }, height = plot_height)
    ## R2B1: Exercise type
    output$pf_r2b1 <- renderPlot({
        pf_ex_tp(df=point)
    }, height = plot_height)
    
    ## R3B1: PSEQ
    output$pf_r3b1 <- renderPlot({
        pf_pseq(df=point)
    }, height = plot_height)
    ## R3B2: Sleep
    output$pf_r3b2 <- renderPlot({
        pf_slp(df=point)
    }, height = plot_height)
    #=========================================================================
    # SECTION 06: Treatment
    #-------------------------------------------------------------------------
    # ROW 3: Drug abuse and dependence
    output$tmt_r2b1 <- renderPlot({
        tmt_orbit_plot_1(df = point, variables = input$tmt_orbit_select)
    }, height = plot_height)
    
    output$tmt_r2b2 <- renderPlot({
        tmt_orbit_plot_2(df = point, variables = input$tmt_orbit_select)
    }, height = plot_height)
    #=========================================================================
    # SECTION 07: Quality of life
    #-------------------------------------------------------------------------
    # PLOT 1: Quality of life
    output$qol_q1 <- renderPlot({
        qol_q1_plot(df=point)
    }, height = plot_height)
    # PLOT 2: Health satisfaction
    output$qol_q2 <- renderPlot({
        qol_q2_plot(df=point)
    }, height = plot_height)
    
    #=========================================================================
    # SECTION 08: Mental Health
    #-------------------------------------------------------------------------
    ## ROW 1 BOX 1: 
    output$mh_r1b1 <- renderPlot({
        mh_ever_plot(point)
    }, height = plot_height)
    
    ## ROW 1 BOX 3: Past 12m 
    output$mh_r1b2 <- renderPlot({
        mh_drug_trend_plot(point)
    }, height = plot_height)
    
    #=========================================================================
    # SECTION 09: Substance Use
    #-------------------------------------------------------------------------
    # ROW 1: lifetime and current drug use
    
    ## ROW 1 BOX 1: 
    output$substance_use_r1b1 <- renderPlot({
        su_ever_plot(point)
    }, height = plot_height)
    
    ## ROW 1 BOX 3: Past 12m 
    output$substance_use_r1b2 <- renderPlot({
        su_drug_trend_plot(point)
    }, height = plot_height)
    
    
    # ROW 2: Opioid dependence
    
    output$substance_use_r2b1 <- renderPlot({
        su_proportion_plot(point, 'Pharm_Opioids_Dep_ICD10', outcome = "Yes")
    }, height = plot_height)
    
    output$substance_use_r2b2 <- renderTable({
        su_proportion_tbl(point, 'Pharm_Opioids_Dep_ICD10', outcome = "Yes")
    })
    
    # ROW 3: Drug abuse and dependence
    output$substance_use_r3b2 <- renderPlot({
        su_plot(point, input$substance_use_select)
    }, height = plot_height)
    
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
    }, height = plot_height)
    #-------------------------------------------------------------------------
    # OME plot
    output$medication_ome <- renderPlot({
        ome_plot(point, input$medication)
    }, height = plot_height)
    #-------------------------------------------------------------------------
    # OME Summary
    output$medication_ome_summary <- renderTable({
        ome_summary(point, input$medication)
    })
    #=========================================================================
    # SECTION ELEVEN: Data Dictionary
    output$data_dictionary_tab = DT::renderDataTable({
        DT::datatable(data_dictionary, options = list(lengthMenu = c(100, 500, 1000, nrow(data_dictionary)), pageLength = 100))
    })
    output$values_dictionary_tab = DT::renderDataTable({
        DT::datatable(values_dictionary, options = list(lengthMenu = c(100, 500, 1000, nrow(values_dictionary)), pageLength = 100))
    })
    
    #=========================================================================
    # SECTION TWELVE: Published papers
    output$published_papers = DT::renderDataTable({
        DT::datatable(published_papers_dat, options = list(lengthMenu = c(5, nrow(published_papers_dat)), pageLength = 100))
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