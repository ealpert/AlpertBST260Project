#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(haven)
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(survey)
library(esquisse)
library(survey)
library(shiny)
library(dslabs)

# Reading in data

# Reading in "Demographic Variables and Sample Weights" data (covariates)
demo <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT") %>%
    select(SEQN, RIDSTATR, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2,
           WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA)

# Reading in health insurance data (covariate)
insure <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HIQ_J.XPT") %>%
    select(SEQN, HIQ011, HIQ031A, HIQ031B, HIQ031D)

# Reading in self-reported health status (covariate)
health <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HSQ_J.XPT") %>%
    select(SEQN, HSD010)

# Reading in disability data (covariate)
disab <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DLQ_J.XPT") %>%
    select(SEQN, DLQ010, DLQ020, DLQ040, DLQ050, DLQ060, DLQ080)

# Reading in self-reported oral health status and utilization data (outcome)
ohques <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/OHQ_J.XPT") %>%
    select(SEQN, OHQ030, OHQ845)

# Reading in examiner-assessed oral health status data (outcome)
ohexam <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/OHXREF_J.XPT") %>%
    select(SEQN, OHAREC, OHDEXSTS)

# Merging NHANES data
nhanesoh <- left_join(demo, insure, by = "SEQN")
nhanesoh <- left_join (nhanesoh, health, by = "SEQN")
nhanesoh <- left_join (nhanesoh, disab, by = "SEQN")
nhanesoh <- left_join (nhanesoh, ohques, by = "SEQN")
nhanesoh <- left_join (nhanesoh, ohexam, by = "SEQN")

# Creating categorical variable for age
nhanesoh <- mutate(nhanesoh, RIDAGEYR, agecat =
            ifelse(RIDAGEYR %in% 0:19, "0-19",
            ifelse(RIDAGEYR %in% 20:29, "20-29",
            ifelse(RIDAGEYR %in% 30:39, "30-39",
            ifelse(RIDAGEYR %in% 40:49, "40-49",
            ifelse(RIDAGEYR %in% 50:59, "50-59",
            ifelse(RIDAGEYR %in% 60:69, "60-69",
            ifelse(RIDAGEYR %in% 70:79, "70-79",
            ifelse(RIDAGEYR %in% 80:89, "80-89",
            ifelse(RIDAGEYR %in% 90:99, "90-99", "unsure"
            ))))))))))

# Defining disability covariate as dichotomous (presence of a disability = 1, no disability = 0)
nhanesoh <- mutate(nhanesoh, disability = ifelse(DLQ010==1 | DLQ020==1 | DLQ040==1 | DLQ050==1 | DLQ060==1 | DLQ080==1, 1, 0))

# Creating categorical variable for insurance
nhanesoh <- mutate(nhanesoh,
                 healthins = case_when(
                     HIQ011==2 ~ 1, #uninsured
                     HIQ031D==17 ~ 2, #medicaid
                     HIQ031B==15 ~ 3, #medicare
                     HIQ031A==14 ~ 4, #privateins
                 ))

# Reordering oral health utilization
nhanesoh <- mutate(nhanesoh,
                   ohutil = case_when(
                       OHQ030==7 ~ 1, #Never have been
                       OHQ030==6 ~ 2, #More than 5 years ago
                       OHQ030==5 ~ 3, #3-5 years ago
                       OHQ030==4 ~ 4, #2-3 years ago
                       OHQ030==3 ~ 5, #1-2 years ago
                       OHQ030==2 ~ 6, #6 months-1 year ago
                       OHQ030==1 ~ 7, #6 months or less
                   ))

# Reordering self-reported oral health
nhanesoh <- mutate(nhanesoh,
                   srohstatus = case_when(
                       OHQ845==5 ~ 1, #Poor
                       OHQ845==4 ~ 2, #Fair
                       OHQ845==3 ~ 3, #Good
                       OHQ845==2 ~ 4, #Very good
                       OHQ845==1 ~ 5, #Excellent
                   ))

# Removing irrelevant data frames
remove(demo, disab, health, insure, ohexam, ohques)

# Filtering on individuals who were both interviewed and examined (RIDSTATR), filtering on individuals who received complete dental exam (OHDEXSTS) 
# From NHANES documentation, there are no missing values for gender, age, race/ethnicity
nhanesoh <- nhanesoh %>%
    filter(RIDSTATR==2) %>% #Dropped 550 observations (if not both interviewed adn examined)
    filter(OHDEXSTS==1) %>% #Dropped 267 observations with partial or not done oral health exams
    drop_na(disability) %>% #Dropped 2159 observations with missing information on disability
    #drop_na(education) %>% #Dropped 884 observations with missing information on education
    drop_na(HSD010) %>% #Dropped 267 observations with missing information on self-reported health status
    # At this point, no missing values for ohutil, srohstatus, exohstatus
    # Updating data frame with created exposure and outcome variables, covariates (renamed), and relevant survey weights
    select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2,
           WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, HSD010, disability, ohutil, OHAREC, healthins, agecat, srohstatus) %>%
    rename(
        seqn = SEQN,
        gender = RIAGENDR,
        age = RIDAGEYR,
        "race/ethnicity" = RIDRETH3,
        education = DMDEDUC2,
        srhealthstatus = HSD010,
        psu = SDMVPSU,
        strata = SDMVSTRA,
        interviewweight = WTINT2YR,
        examweight = WTMEC2YR,
        exohstatus = OHAREC
    )

nhanesoh$education <- as.factor(nhanesoh$education)
nhanesoh$srohstatus <- as.factor(nhanesoh$srohstatus)
nhanesoh$gender <- as.factor(nhanesoh$gender)
nhanesoh$srohstatus <- as.factor(nhanesoh$srohstatus)
nhanesoh$exohstatus <- as.factor(nhanesoh$exohstatus)
nhanesoh$disability <- as.factor(nhanesoh$disability)
nhanesoh$"race/ethnicity" <- as.factor(nhanesoh$"race/ethnicity")
nhanesoh$ohutil <- as.factor(nhanesoh$ohutil)
nhanesoh$healthins <- as.factor(nhanesoh$healthins)

nhanesoh <- nhanesoh %>%
    filter(education %in% 1:5) %>%
    filter(srhealthstatus %in% 1:5) %>%
    filter(ohutil %in% 1:7) %>%
    filter(srohstatus %in% 1:5) %>%
    filter(exohstatus %in% 1:4) %>%
    filter(healthins %in% 1:4)

ui <- navbarPage("NHANES (2017-2018)",
    tabPanel("Oral Health Outcomes",
        fluidPage(
            fluidRow(
                column(6, radioButtons(inputId = "outcome",
                                       label = "Oral Health Outcome",
                                       choiceNames = c("Oral Health Utilization", "Self-Reported Oral Health Status", "Examiner-Assessed Oral Health Status"),
                                       choiceValues = c("ohutil", "srohstatus", "exohstatus"),
                                       selected = "ohutil"))),
                    
            fluidRow(
                column(12, plotOutput("bar")))
    )),

    tabPanel("Disability and Oral Health Utilization",
         fluidPage(
             fluidRow(
                 column(6, selectInput(inputId = "disabutil",
                                        label = "Exposure",
                                        choices = c("Gender"="gender", "Age"="agecat", "Race/Ethnicity"="race/ethnicity", "Education"="education", "Health Insurance"="healthins"),
                                        selected = "gender"))),
             
             fluidRow(
                 column(12, plotOutput("utilbar")))
     )),
    
    tabPanel("Disability and Self-Reported Oral Health Status",
             fluidPage(
                 
                 
                 fluidRow(
                     column(6, selectInput(inputId = "disabstat",
                                           label = "Exposure",
                                           choices = c("Gender"="gender", "Age"="agecat", "Race/Ethnicity"="race/ethnicity", "Education"="education", "Health Insurance"="healthins"),
                                           selected = "gender"))),
                 
                 fluidRow(
                     column(12, plotOutput("statbar"))),
    )),
    
    tabPanel("Disability and Examiner-Assessed Oral Health Status",
             fluidPage(
                 
                 
                 fluidRow(
                     column(6, selectInput(inputId = "disabstat2",
                                           label = "Exposure",
                                           choices = c("Gender"="gender", "Age"="agecat", "Race/Ethnicity"="race/ethnicity", "Education"="education", "Health Insurance"="healthins"),
                                           selected = "gender"))),
        
                 fluidRow(
                     column(12, plotOutput("statbar2")))
             )),
    
    tabPanel("The Impact of Age",
             fluidPage(
                 
                 
                 fluidRow(
                     column(6, selectInput(inputId = "age",
                                           label = "Oral Health Outcome",
                                           choices = c("ohutil", "srohstatus", "exohstatus"),
                                           selected = "ohutil"))),
                 
                 fluidRow(
                     column(12, plotOutput("boxplot")))
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bar <- renderPlot(
        if (input$outcome == "ohutil")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = ohutil, fill = "#0c4c8a"), show.legend = FALSE) +
                xlab("When Did You Last Visit a Dentist?") +
                ylab("Count") +
                theme_minimal() +
                scale_x_discrete(labels=c("Never", "5+ Years Ago", "3-5 Years Ago", "2-3 Years Ago", "1-2 Years Ago", "6 Months-1 Year Ago", "< 6 Months Ago"))
        }
        
        else if (input$outcome == "srohstatus")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = srohstatus, fill = "#0c4c8a"), show.legend = FALSE) +
                xlab("Self-Reported Oral Health Status") +
                ylab("Count") +
                theme_minimal() +
                scale_x_discrete(labels=c("Poor", "Fair", "Good", "Very Good", "Excellent"))
        }
        
        else
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = exohstatus, fill = "#0c4c8a"), show.legend = FALSE) +
                xlab("When Examiner Recommends Seeing a Dentist") +
                ylab("Count") +
                theme_minimal() +
                scale_x_discrete(labels=c("Immediately", "Within 2 Weeks", "At Earliest Convenience", "Continue Regular Care"))
        }
        )
    
    output$utilbar <- renderPlot(
        if (input$disabutil == "gender")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = ohutil, fill = gender)) +
                xlab("Gender") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
                
        else if (input$disabutil == "agecat")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = ohutil, fill = agecat)) +
                xlab("Age") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabutil == "race/ethnicity")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = ohutil, fill = `race/ethnicity`)) +
                xlab("Race/Ethnicity") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabutil == "education")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = ohutil, fill = education)) +
                xlab("Education") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabutil == "healthins")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = ohutil, fill = healthins)) +
                xlab("Type of Health Insurance") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
    )


    
        
    output$statbar <- renderPlot(
        if (input$disabstat == "gender")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = srohstatus, fill = gender)) +
                xlab("Gender") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat == "agecat")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = srohstatus, fill = agecat)) +
                xlab("Age") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat == "race/ethnicity")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = srohstatus, fill = `race/ethnicity`)) +
                xlab("Race/Ethnicity") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat == "education")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = srohstatus, fill = education)) +
                xlab("Education") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat == "healthins")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = srohstatus, fill = healthins)) +
                xlab("Type of Health Insurance") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
    )
    
    output$statbar2 <- renderPlot(
        if (input$disabstat2 == "gender")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = exohstatus, fill = gender)) +
                xlab("Gender") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat2 == "agecat")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = exohstatus, fill = agecat)) +
                xlab("Age") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat2 == "race/ethnicity")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = exohstatus, fill = `race/ethnicity`)) +
                xlab("Race/Ethnicity") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat2 == "education")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = exohstatus, fill = education)) +
                xlab("Education") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
        else if (input$disabstat2 == "healthins")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = exohstatus, fill = healthins)) +
                xlab("Type of Health Insurance") +
                ylab("Count") +
                scale_fill_hue() +
                theme_minimal() +
                facet_wrap(vars(disability))
        }
        
    )
    
    output$boxplot <- renderPlot(
        if (input$age == "ohutil")
        {
            nhanesoh %>% 
                ggplot() +
                geom_boxplot(aes(x = ohutil, y = age, fill = disability)) +
                scale_fill_hue() +
                xlab("Oral Health Utilization") +
                ylab("Age") +
                theme_minimal()
        }
        
        else if (input$age == "srohstatus")
        {
            nhanesoh %>% 
                ggplot() +
                geom_boxplot(aes(x = srohstatus, y = age, fill = disability)) +
                scale_fill_hue() +
                xlab("Self-Reported Oral Health Status") +
                ylab("Age") +
                theme_minimal()
        }
        
        else
        {
            nhanesoh %>% 
                ggplot() +
                geom_boxplot(aes(x = exohstatus, y = age, fill = disability)) +
                scale_fill_hue() +
                xlab("Examiner-Assessed Oral Health Status") +
                ylab("Age") +
                theme_minimal()
        }
    )


}

# Run the application 
shinyApp(ui = ui, server = server)
