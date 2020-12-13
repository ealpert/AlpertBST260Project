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
                     HIQ011==2 ~ 0, #uninsured
                     HIQ031D==17 ~ 1, #medicaid
                     HIQ031A==14 ~ 2, #privateins
                     HIQ031B==15 ~ 3 #medicare
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
           WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, HSD010, disability, OHQ030, OHQ845, OHAREC, healthins, agecat) %>%
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
        srohstatus = OHQ845,
        exohstatus = OHAREC,
        ohutil = OHQ030
    )

nhanesoh$education <- as.factor(nhanesoh$education)
nhanesoh$srohstatus <- as.factor(nhanesoh$srohstatus)
nhanesoh$gender <- as.factor(nhanesoh$gender)
nhanesoh$srohstatus <- as.factor(nhanesoh$srohstatus)
nhanesoh$exohstatus <- as.factor(nhanesoh$exohstatus)
nhanesoh$disability <- as.factor(nhanesoh$disability)
nhanesoh$"race/ethnicity" <- as.factor(nhanesoh$"race/ethnicity")
nhanesoh$ohutil <- as.factor(nhanesoh$ohutil)

nhanesoh <- nhanesoh %>%
    filter(education %in% 1:5) %>%
    filter(srhealthstatus %in% 1:5) %>%
    filter(ohutil %in% 1:7) %>%
    filter(srohstatus %in% 1:5) %>%
    filter(exohstatus %in% 1:4)

data("nhanesoh")
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
                column(8, plotOutput("bar"))),
    )),

    tabPanel("Oral Health Outcomes",
         fluidPage(
             fluidRow(
                 column(6, radioButtons(inputId = "outcome",
                                        label = "Oral Health Outcome",
                                        choiceNames = c("Oral Health Utilization", "Self-Reported Oral Health Status", "Examiner-Assessed Oral Health Status"),
                                        choiceValues = c("ohutil", "srohstatus", "exohstatus"),
                                        selected = "ohutil"))),
             
             fluidRow(
                 column(8, plotOutput("bar"))),
     ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bar <- renderPlot(
        if (input$outcome == "ohutil")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = ohutil, fill = "#0c4c8a")) +
                xlab("Oral Health Utilization") +
                ylab("Count") +
                theme_minimal()
        }
        
        else if (input$outcome == "srohstatus")
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = srohstatus, fill = "#0c4c8a")) +
                xlab("Self-Reported Oral Health Status") +
                ylab("Count") +
                theme_minimal()
        }
        
        else
        {
            nhanesoh %>% 
                ggplot() +
                geom_bar(aes(x = exohstatus, fill = "#0c4c8a")) +
                xlab("Examiner-Assessed Oral Health Status") +
                ylab("Count") +
                theme_minimal()
        }
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
