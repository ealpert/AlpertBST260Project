```{r}
library(haven)
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(survey)
library(esquisse)
library(survey)
library(shiny)
```

Reading in NHANES 2017-2018 data.
```{r}
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
```

Previewing data and creating merged data frame.
```{r}
# Previewing data
head(demo)
head(insure)
head(health)
head(disab)
head(ohques)
head(ohexam)

# Joining datasets by SEQN (respondent sequence number)
nhanes <- left_join(demo, insure, by = "SEQN")
nhanes <- left_join (nhanes, health, by = "SEQN")
nhanes <- left_join (nhanes, disab, by = "SEQN")
nhanes <- left_join (nhanes, ohques, by = "SEQN")
nhanes <- left_join (nhanes, ohexam, by = "SEQN")
```

```{r}
# Getting an overview of the data
names(nhanes)
summary(nhanes)
```

Defining covariates (age, gender, race/ethnicity, education, health insurance, self-reported health status, disability).
Defining outcomes (oral health utilization, self-reported oral health status, examiner-assessed oral health status)

"HIQ011"   "HIQ031A" 
[13] "HIQ031B"  "HIQ031D"  "HSD010"   "DLQ010"   "DLQ020"   "DLQ040"   "DLQ050"   "DLQ060"   "DLQ080"   "OHQ030"   "OHQ845"   "OHAREC"  
[25] "OHDEXSTS"
```{r}
# Keeping gender, race/ethnicity, education, self-reported health status "as-is" for now.

# Creating categorical variable for age
nhanes <- mutate(nhanes, RIDAGEYR, agecat =
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
nhanes <- mutate(nhanes, disability = ifelse(DLQ010==1 | DLQ020==1 | DLQ040==1 | DLQ050==1 | DLQ060==1 | DLQ080==1, 1, 0))

# Creating categorical variable for insurance
nhanes <- mutate(nhanes,
                 healthins = case_when(
                     HIQ011==2 ~ 0, #uninsured
                     HIQ031D==17 ~ 1, #medicaid
                     HIQ031A==14 ~ 2, #privateins
                     HIQ031B==15 ~ 3 #medicare
                 ))

summary(nhanes)
```

Refining data frame with relevant covariates, exposure, and outcomes of interest.
```{r}
# Removing irrelevant data frames
remove(demo, disab, health, insure, ohexam, ohques)

# Checking how many in sample had complete dental exam
table(nhanes$RIDSTATR)

# Filtering on individuals who were both interviewed and examined (RIDSTATR), filtering on individuals who received complete dental exam (OHDEXSTS) 
# From NHANES documentation, there are no missing values for gender, age, race/ethnicity
nhanes <- nhanes %>%
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

names(nhanes)

# Recoding education, privateins, srohstatus, gender, uninsured, srhealthstatus, exohstatus, medicaid, disability, race/ethnicity, medicare, ohutil to factor variables from numeric
nhanes$education <- as.factor(nhanes$education)
nhanes$srohstatus <- as.factor(nhanes$srohstatus)
nhanes$gender <- as.factor(nhanes$gender)
nhanes$srohstatus <- as.factor(nhanes$srohstatus)
nhanes$exohstatus <- as.factor(nhanes$exohstatus)
nhanes$disability <- as.factor(nhanes$disability)
nhanes$"race/ethnicity" <- as.factor(nhanes$"race/ethnicity")
nhanes$ohutil <- as.factor(nhanes$ohutil)

head(nhanes)
summary(nhanes)

# Further cleaning dataset
nhanes <- nhanes %>%
    filter(education %in% 1:5) %>%
    filter(srhealthstatus %in% 1:5) %>%
    filter(ohutil %in% 1:7) %>%
    filter(srohstatus %in% 1:5) %>%
    filter(exohstatus %in% 1:4)
```

```{r}
# According to CDC tutorial (https://wwwn.cdc.gov/nchs/nhanes/tutorials/Module3.aspx), since I am using both interview and exam data, I should use the MEC exam weight

# Creating variable "nhanesdesign" to use in runnning analysis, using "svydesign" to assign weights
nhanesdesign <- svydesign(id      = ~psu,
                          strata  = ~strata,
                          weights = ~examweight,
                          nest    = TRUE,
                          data    = nhanes)

# Applying weights to covariates (age, agecat, gender, race/ethnicity, education, srhealthstatus, disability, healthins)
agedesign <- subset(nhanesdesign, age %in% 0:200)
svymean(~age, agedesign, na.rm = TRUE)
summary(nhanes$age)

# Applying weights to outcomes (srohstatus, exohstatus, ohutil)

```


