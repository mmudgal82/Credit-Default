#=====================#
# 0 Setup Environment #
#=====================#
# optional - clean up workspace:  rm(list=ls())

setwd("D:\\Documents\\MMA Queens\\Courses\\MMA867\\Assignment 3")

#----- FIRST PROCESS: EDA, Features, and Cleaning -------#

CreditData <- read.csv("Credit_Data.csv", 
                       na.strings=c(""," ","NA"), 
                       header=TRUE, 
                       stringsAsFactors = TRUE)

library(DataExplorer)
library(dplyr)
head(InsuranceData)

CreditData %>% glimpse()

CreditData %>%
  create_report(
    output_file  = "Credit_data_profile_report",
    output_dir   = "D:\\Documents\\MMA Queens\\Courses\\MMA867\\Assignment 3",
    y            = "default_0",
    report_title = "EDA Report - Credit Data"
  )

#commentary:
#1 - Limit Balance exhibits a right-skewed distribution indicating most customers have limits at the lower end of the spectrum. This is expected as banks regularly offer a pre-approved credit line to customers typically lower in value. Based on utilization and performance reviews, the limit adjustment occurs. We decided to bin the limits based on business acumen.
#2 - Education levels indicate that most customers have a college degree (1 and 2). Categories 5 and 6 should be merged as they reflect the same information. 
#3 - Marriage: The dataset contains slightly more single customers but is almost balanced. We observed customers are categorized as 0, which is an invalid value per the data dictionary and can be merged with 3.
#4 - Age exhibits a right-skewed distribution indicative of a younger customer base. We decided to bin the ages based on business acumen. 

#Check Discrete and Continuous - Initial Check missing

CreditData %>% introduce()
CreditData %>% plot_intro()

#Missing

CreditData %>% plot_missing()
CreditData %>% profile_missing()

#Continuous

CreditData %>% plot_density()
CreditData %>% plot_histogram()

#Categorical

CreditData %>% plot_bar()

#Correlation

CreditData %>% plot_correlation(maxcat = 20L)

#outcomes:
#We proceeded to extract the payment behavior of customers by calculating Payment Percentage (Paid amount[t] / Billed amount[t-1]).  
#The above step led to some infinite values which had to be capped applying business acumen. 
#We created bins for the Payment Percentage variables. 
#We then created bins for the Limit and Age variables based on our best judgment and aimed for a reasonable distribution. 
#Our next step was to combine the unknown values in Marriage status and Education. 
#We created interaction variables between Sex, Education, and Marriage to extract value from combinations of these features.  
#The interactions, as well as the previously observed variables, were redesignated factors. 
#We also implemented a function to Fix any NA values.  
#Extracted a data file for modeling needs. 

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071", "tidyverse")  #Check, and if needed install the necessary packages

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code

###
### Load
###

CreditData<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = TRUE) # Load the datafile to R

str(CreditData) # See if some data types were misclassified when importing data from CSV
head(CreditData)

#
#Feature Engineering - This is here because it needs execution before FIXNA
#

# Feature - Payment percentage - Payment amount (t)/ Bill amount (t-1)
CreditData <- mutate(CreditData, PAY_PERC_1 = PAY_AMT1 / BILL_AMT2 * 100)
CreditData <- mutate(CreditData, PAY_PERC_2 = PAY_AMT2 / BILL_AMT3 * 100)
CreditData <- mutate(CreditData, PAY_PERC_3 = PAY_AMT3 / BILL_AMT4 * 100)
CreditData <- mutate(CreditData, PAY_PERC_4 = PAY_AMT4 / BILL_AMT5 * 100)
CreditData <- mutate(CreditData, PAY_PERC_5 = PAY_AMT5 / BILL_AMT6 * 100)

# Feature - Limits Binned - Had a look at the distribution to decide
CreditData$LIMIT_BAL_BIN <- cut(CreditData$LIMIT_BAL, breaks = c(0, 50000, 100000, 200000, 300000, 10000000), labels = c("VLOW", "LOW", "MED", "HIGH", "VHIGH"))
table(CreditData$LIMIT_BAL_BIN)

# Feature - Age Binned - Had a look at the distribution to decide
CreditData$AGE_BIN <- cut(CreditData$AGE, breaks = c(0, 24, 32, 45, 60, 110), labels = c("0-24", "24-32", "32-45", "45-60", "60+"))
table(CreditData$AGE_BIN)
#combine values

CreditData$EDUCATION <- ifelse(CreditData$EDUCATION == 0,6,
                               ifelse(CreditData$EDUCATION == 5,6, CreditData$EDUCATION))

CreditData$MARRIAGE <- ifelse(CreditData$MARRIAGE ==0,3, CreditData$MARRIAGE)

# Categorical Features - Interactions
CreditData <- mutate(CreditData, SEX_EDUCATION = SEX * EDUCATION)
CreditData <- mutate(CreditData, EDUCATION_MARRIAGE = EDUCATION * MARRIAGE)
CreditData <- mutate(CreditData, SEX_MARRIAGE = SEX * MARRIAGE)
CreditData <- mutate(CreditData, SEX_EDUCATION_MARRIAGE = SEX * EDUCATION * MARRIAGE)

str(CreditData)

# Fixing incorrectly classified data types:
CreditData$SEX <- as.factor(CreditData$SEX)
CreditData$EDUCATION <- as.factor(CreditData$EDUCATION)
CreditData$MARRIAGE <- as.factor(CreditData$MARRIAGE)
CreditData$PAY_1 <- as.factor(CreditData$PAY_1)
CreditData$PAY_2 <- as.factor(CreditData$PAY_2)
CreditData$PAY_3 <- as.factor(CreditData$PAY_3)
CreditData$PAY_4 <- as.factor(CreditData$PAY_4)
CreditData$PAY_5 <- as.factor(CreditData$PAY_5)
CreditData$PAY_6 <- as.factor(CreditData$PAY_6)
CreditData$default_0 <- as.factor(CreditData$default_0)
CreditData$SEX_EDUCATION <- as.factor(CreditData$SEX_EDUCATION)
CreditData$EDUCATION_MARRIAGE <- as.factor(CreditData$EDUCATION_MARRIAGE)
CreditData$SEX_MARRIAGE <- as.factor(CreditData$SEX_MARRIAGE)
CreditData$SEX_EDUCATION_MARRIAGE <- as.factor(CreditData$SEX_EDUCATION_MARRIAGE)

str(CreditData)
# Create a custom function to fix missing values ("NAs") and preserve the NA info as surrogate variables
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame)
}

CreditData<-fixNAs(CreditData) #Apply fixNAs function to the data to fix missing values

str(CreditData)

table(CreditData$PAY_2)# check for rare categories
#PAY2 (8) has 1 instance, Pay4 (1 and 8) have 2 instances, Pay5 (6 and 8) have 2 and 1 instances respectively.
#Pay6 (8) has 2 instances

# Create another a custom function to combine rare categories into "Other."+the name of the original variavle (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"
#combinerarecategories<-function(data_frame,mincount){ 
#  for (i in 1 : ncol(data_frame)){
#   a<-data_frame[,i]
#   replace <- names(which(table(a) < mincount))
#   levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
#   data_frame[,i]<-a }
# return(data_frame) }

#Apply combinerarecategories function to the data and then split it into testing and training data.

#CreditData<-combinerarecategories(CreditData,2) #combine categories with <10 values in STCdata into "Other"
??combinerarecategories



str(CreditData)
summary(CreditData)
write.csv(CreditData, "Credit_Data_FE.csv")

###----------------------------------###
#For applicants file#

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code

###
### Load
###

ApplicantData<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE, stringsAsFactors = TRUE) # Load the datafile to R

str(ApplicantData) # See if some data types were misclassified when importing data from CSV
head(ApplicantData)

#
#Feature Engineering - This is here because it needs execution before FIXNA
#

# Feature - Payment percentage - Payment amount (t)/ Bill amount (t-1)
ApplicantData <- mutate(ApplicantData, PAY_PERC_1 = PAY_AMT1 / BILL_AMT2 * 100)
ApplicantData <- mutate(ApplicantData, PAY_PERC_2 = PAY_AMT2 / BILL_AMT3 * 100)
ApplicantData <- mutate(ApplicantData, PAY_PERC_3 = PAY_AMT3 / BILL_AMT4 * 100)
ApplicantData <- mutate(ApplicantData, PAY_PERC_4 = PAY_AMT4 / BILL_AMT5 * 100)
ApplicantData <- mutate(ApplicantData, PAY_PERC_5 = PAY_AMT5 / BILL_AMT6 * 100)

# Feature - Limits Binned - Had a look at the distribution to decide
ApplicantData$LIMIT_BAL_BIN <- cut(ApplicantData$LIMIT_BAL, breaks = c(0, 50000, 100000, 200000, 300000, 10000000), labels = c("VLOW", "LOW", "MED", "HIGH", "VHIGH"))
table(ApplicantData$LIMIT_BAL_BIN)

# Feature - Age Binned - Had a look at the distribution to decide
ApplicantData$AGE_BIN <- cut(ApplicantData$AGE, breaks = c(0, 24, 32, 45, 60, 110), labels = c("0-24", "24-32", "32-45", "45-60", "60+"))
table(ApplicantData$AGE_BIN)
#combine values

ApplicantData$EDUCATION <- ifelse(ApplicantData$EDUCATION == 0,6,
                                  ifelse(ApplicantData$EDUCATION == 5,6, ApplicantData$EDUCATION))

ApplicantData$MARRIAGE <- ifelse(ApplicantData$MARRIAGE ==0,3, ApplicantData$MARRIAGE)

# Categorical Features - Interactions
ApplicantData <- mutate(ApplicantData, SEX_EDUCATION = SEX * EDUCATION)
ApplicantData <- mutate(ApplicantData, EDUCATION_MARRIAGE = EDUCATION * MARRIAGE)
ApplicantData <- mutate(ApplicantData, SEX_MARRIAGE = SEX * MARRIAGE)
ApplicantData <- mutate(ApplicantData, SEX_EDUCATION_MARRIAGE = SEX * EDUCATION * MARRIAGE)

str(ApplicantData)

# Fixing incorrectly classified data types:
ApplicantData$SEX <- as.factor(ApplicantData$SEX)
ApplicantData$EDUCATION <- as.factor(ApplicantData$EDUCATION)
ApplicantData$MARRIAGE <- as.factor(ApplicantData$MARRIAGE)
ApplicantData$PAY_1 <- as.factor(ApplicantData$PAY_1)
ApplicantData$PAY_2 <- as.factor(ApplicantData$PAY_2)
ApplicantData$PAY_3 <- as.factor(ApplicantData$PAY_3)
ApplicantData$PAY_4 <- as.factor(ApplicantData$PAY_4)
ApplicantData$PAY_5 <- as.factor(ApplicantData$PAY_5)
ApplicantData$PAY_6 <- as.factor(ApplicantData$PAY_6)
ApplicantData$default_0 <- as.factor(ApplicantData$default_0)
ApplicantData$SEX_EDUCATION <- as.factor(ApplicantData$SEX_EDUCATION)
ApplicantData$EDUCATION_MARRIAGE <- as.factor(ApplicantData$EDUCATION_MARRIAGE)
ApplicantData$SEX_MARRIAGE <- as.factor(ApplicantData$SEX_MARRIAGE)
ApplicantData$SEX_EDUCATION_MARRIAGE <- as.factor(ApplicantData$SEX_EDUCATION_MARRIAGE)

str(ApplicantData)
# Create a custom function to fix missing values ("NAs") and preserve the NA info as surrogate variables
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame)
}

ApplicantData<-fixNAs(ApplicantData) #Apply fixNAs function to the data to fix missing values

str(ApplicantData)

table(ApplicantData$PAY_2)# check for rare categories
#PAY2 (8) has 1 instance, Pay4 (1 and 8) have 2 instances, Pay5 (6 and 8) have 2 and 1 instances respectively.
#Pay6 (8) has 2 instances

# Create another a custom function to combine rare categories into "Other."+the name of the original variavle (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"
#combinerarecategories<-function(data_frame,mincount){ 
#  for (i in 1 : ncol(data_frame)){
#   a<-data_frame[,i]
#   replace <- names(which(table(a) < mincount))
#   levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
#   data_frame[,i]<-a }
# return(data_frame) }

#Apply combinerarecategories function to the data and then split it into testing and training data.

#ApplicantData<-combinerarecategories(ApplicantData,2) #combine categories with <10 values in STCdata into "Other"
??combinerarecategories

str(ApplicantData)
summary(ApplicantData)
write.csv(ApplicantData, "Applicant_Data_FE.csv")

#----- SECOND PROCESS: Model building and tuning --------#

## Notes to combine the data ##
## (1) remove the first column 'X' as it is same as ID. 
## (2) append the 1K records from hold out set and also update the IDs (24001 - 25000)
## (3) For the 1K hold-out records, replace the 'NA' in default_0 column to 999. If we keep it as NA, R will fail in the model.matrix() step.

# b) Load Packages
library(tidyverse)
library(mice)
library(GGally)
library(MLmetrics)
library(ROCR)
library(lift)
library(tidymodels)
library(caret)
library(xgboost)

library(mlrMBO)  # for bayesian optimisation
library(smoof)   # mlrmbo requires this : define the objective function which will be optimized
library(ParamHelpers) # mlrmbo requires this :  define a parameter space in which we will perform the bayesian search for a global optimum
library(DiceKriging) # mlrmbo requires this : provides the gaussian process interpolation
library(rgenoud)
library(plot3D)

#=======================#
# 1 Data Pre-processing #
#=======================#
# 1a) import data
Combined_CreditData_FE <- read.csv("Combined_Credit_Data_FE.csv", 
                                    na.strings=c(""," ","NA"), 
                                    header=TRUE, 
                                    stringsAsFactors = TRUE)

str(Combined_CreditData_FE)
tail(Combined_CreditData_FE)

#### ------------------- EDA ----------------###
# 1b) rename columns
# Combined_CreditData_FE <- Combined_CreditData_FE %>% subset(select=-X) %>% rename(ID = ?..ID)

# 1c) create new variables
Combined_CreditData_FE$PAY_PERC_1 <- ifelse(Combined_CreditData_FE$PAY_PERC_1 == Inf,100,Combined_CreditData_FE$PAY_PERC_1)
Combined_CreditData_FE$PAY_PERC_2 <- ifelse(Combined_CreditData_FE$PAY_PERC_2 == Inf,100,Combined_CreditData_FE$PAY_PERC_2)
Combined_CreditData_FE$PAY_PERC_3 <- ifelse(Combined_CreditData_FE$PAY_PERC_3 == Inf,100,Combined_CreditData_FE$PAY_PERC_3)
Combined_CreditData_FE$PAY_PERC_4 <- ifelse(Combined_CreditData_FE$PAY_PERC_4 == Inf,100,Combined_CreditData_FE$PAY_PERC_4)
Combined_CreditData_FE$PAY_PERC_5 <- ifelse(Combined_CreditData_FE$PAY_PERC_5 == Inf,100,Combined_CreditData_FE$PAY_PERC_5)

Combined_CreditData_FE$PAY_PERC_1_BIN <- cut(Combined_CreditData_FE$PAY_PERC_1, breaks = c(-18215001,0.00, 0.02, 0.05, 0.2, 0.5, 0.9, 1000000), labels = c("LOWEST","SuperLOW", "VLOW", "LOW", "MED", "HIGH", "VHIGH"))
Combined_CreditData_FE$PAY_PERC_2_BIN <- cut(Combined_CreditData_FE$PAY_PERC_2, breaks = c(-18215001,0.00, 0.02, 0.05, 0.2, 0.5, 0.9, 1000000), labels = c("LOWEST","SuperLOW", "VLOW", "LOW", "MED", "HIGH", "VHIGH"))
Combined_CreditData_FE$PAY_PERC_3_BIN <- cut(Combined_CreditData_FE$PAY_PERC_3, breaks = c(-18215001,0.00, 0.02, 0.05, 0.2, 0.5, 0.9, 1000000), labels = c("LOWEST","SuperLOW", "VLOW", "LOW", "MED", "HIGH", "VHIGH"))
Combined_CreditData_FE$PAY_PERC_4_BIN <- cut(Combined_CreditData_FE$PAY_PERC_4, breaks = c(-18215001,0.00, 0.02, 0.05, 0.2, 0.5, 0.9, 1000000), labels = c("LOWEST","SuperLOW", "VLOW", "LOW", "MED", "HIGH", "VHIGH"))
Combined_CreditData_FE$PAY_PERC_5_BIN <- cut(Combined_CreditData_FE$PAY_PERC_5, breaks = c(-18215001,0.00, 0.02, 0.05, 0.2, 0.5, 0.9, 1000000), labels = c("LOWEST","SuperLOW", "VLOW", "LOW", "MED", "HIGH", "VHIGH"))

str(Combined_CreditData_FE)

# 1c) change data type from integer to factor
factor_cols <- c("SEX","EDUCATION","MARRIAGE",
                 "PAY_1","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6",
                 "SEX_EDUCATION","EDUCATION_MARRIAGE","SEX_MARRIAGE","SEX_EDUCATION_MARRIAGE",
                 "PAY_PERC_1_surrogate","PAY_PERC_2_surrogate","PAY_PERC_3_surrogate","PAY_PERC_4_surrogate","PAY_PERC_5_surrogate",
                 "default_0")

Combined_CreditData_FE[factor_cols] <- lapply(Combined_CreditData_FE[factor_cols],as.factor)
str(Combined_CreditData_FE)


#=========================================================#
# 2 xgboost special data requirement - matrices as inputs #
#=========================================================#
# 2a) create xgboost matrix prior to train test split to make sure levels remain the same
# model.matrix automatically creates dummy variables
Combined_CreditData_FE_matrix <- model.matrix(default_0 ~.,data=Combined_CreditData_FE)[,-1] #-1 means no intercept needed


# 2b) Split into 24K training set with 'default_0' and 1K hold-out set we need to submit
#>>>> 24K training with default_0 <<<<#
#### Model Matrix Format
CreditData_FE_matrix <- Combined_CreditData_FE_matrix[1:24000,] #click and verify that CreditData_FE_matrix contains ID 1 - 24000
#CreditData_FE_matrix <- CreditData_FE_matrix[, colnames(CreditData_FE_matrix) != "ID"]

#### Data Frame Format
CreditData_FE <- dplyr::filter(Combined_CreditData_FE,default_0 %in% c("0","1")) 
CreditData_FE$default_0 <- as.factor(as.character(CreditData_FE$default_0))       #drop unused level 999 
#CreditData_FE <- select(CreditData_FE,c(-ID))  #Drop ID - as 1K test set shares same ID. It will mess up with the prediction

#### audit - to ensure we only run on 24k rows
levels(CreditData_FE$default_0)
table(CreditData_FE$default_0)

#>>>> 1K hold-out no default_0 <<<<#
#### Model Matrix Format
real_test_matrix <- Combined_CreditData_FE_matrix[24001:25000,]  #click and verify that real_test_matrix contains ID 24001 - 25000
#real_test_matrix <- real_test_matrix[, colnames(real_test_matrix) != "ID"]      #Drop ID - as 1K test set shares same ID. It will mess up with the prediction

#### Data Frame Format - although we might not need it 
Real_Test <- Combined_CreditData_FE %>% filter(default_0=='999') %>% select(-c(default_0,ID))

# 2c) train/test/validation split
set.seed(42) 

#--------- Train vs Non-Train -----------#
train_ratio <- 21999/24000
inTrain <- createDataPartition(y = CreditData_FE$default_0, #If the y argument to this function is a factor, the random sampling occurs within each class and should preserve the overall class distribution of the data
                               p = train_ratio, list = FALSE) #list = FALSE avoids returning the data as a list

training <- CreditData_FE[inTrain,]
testing_validation <- CreditData_FE[-inTrain,]

x_train <- CreditData_FE_matrix[inTrain,]
x_testing_validation <- CreditData_FE_matrix[-inTrain,]
y_train <- training$default_0

#--------- Test vs Validation -----------#
test_ratio <- 1000/2000
inTest <- createDataPartition(y = testing_validation$default_0, 
                               p = test_ratio, list = FALSE) 

testing <- testing_validation[inTest,]
validation <- testing_validation[-inTest,] 

x_test <- x_testing_validation[inTest,]
x_validation <- x_testing_validation[-inTest,] 
y_test <- testing$default_0
y_validation <- validation$default_0

#--------- xgb sparse matrix --------------#
#>>>>>> 24K Records With default_0. xgb.DMatrix() needs label(default_0)
dtrain <- xgb.DMatrix(data = as.matrix(x_train),label = as.numeric(as.character(y_train))) 
dtest <- xgb.DMatrix(data = as.matrix(x_test),label = as.numeric(as.character(y_test))) 
dvalidation <- xgb.DMatrix(data = as.matrix(x_validation),label = as.numeric(as.character(y_validation)))

#>>>>>> 1K hold-out set no default_0. xgb predict() should also accept model matrix format
x_real_test <- as.matrix(real_test_matrix)

#--------- 24K records with default_0 -----------#
## training:        x_train         y_train
## validation:      x_validation    y_validation
## testing:         x_test          y_test
#------------------------------------------------#

#--------- 1K records no default_0 -----------#
## DataFrame Format:   Real_Test
## model matrix format:  x_real_test
#---------------------------------------------#


#============================#
# 3 xgboost default setting  #
# AUC on Test set: 0.7696316
#============================#
neg <- filter(training,default_0==0)
pos <- filter(training,default_0==1)
neg_to_pos <- count(neg)/count(pos)
neg_to_pos

# 3a) xgboost default training
xbg_model_v1<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=100,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic",
                      scale_pos_weight = neg_to_pos
)

# 3b) Confusion Matrix on test set 
#Predict classification (for confusion matrix)
xgboost_pred_proba_v1<- predict(xbg_model_v1,newdata=x_test,type="response") 

#Display confusion matrix
confusionMatrix(as.factor(ifelse(xgboost_pred_proba_v1>0.231,1,0)),
                y_test,positive="1") 


# 3c) ROC Curve and AUC - 0.7783502
### ROC Curve
XGboost_ROC_prediction <- prediction(xgboost_pred_proba_v1, y_test) #Calculate errors
XGboost_ROC_testing <- ROCR::performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) 

####AUC
auc.tmp <- ROCR::performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

# 3d) Lift Chart
#### Lift chart
plotLift(xgboost_pred_proba_v1, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_XGboost <- ROCR::performance(XGboost_ROC_prediction,"lift","rpp")
plot(Lift_XGboost)


#===============================#
# 4 xgb - bayesian optimization # 
#===============================#
# 4a) generate a list of indices used for K-folds cross-validation
cv_folds = rBayesianOptimization::KFold(training$default_0, 
                                        nfolds= 5,
                                        stratified = TRUE,
                                        seed= 42)

# 4b) define objective function(auc) we want to maximize 
# https://www.rdocumentation.org/packages/xgboost/versions/1.4.1.1/topics/xgb.cv
# https://insightr.wordpress.com/2018/05/17/tuning-xgboost-in-r-part-i/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
# https://www.r-bloggers.com/2021/01/bayesian-model-based-optimization-in-r/
# https://www.simoncoulombe.com/2019/01/bayesian/
# scale_pos_weight : number of negative class to the positive class

obj.fun  <- smoof::makeSingleObjectiveFunction(
  name = "xgb_cv_bayes",
  fn = function(x){
    set.seed(42)
    
    cv <- xgb.cv(params = list(
      booster          = "gbtree",
      eta              = x["eta"],
      max_depth        = x["max_depth"],
      gamma            = x["gamma"],             #minimum reduction in the loss function required to grow a new node in a tree
      subsample        = x["subsample"],
      min_child_weight = x["min_child_weight"],  #minimum number of observations (instances) in a terminal node
      colsample_bytree = x["colsample_bytree"],
      max_delta_step = x["max_delta_step"],
      objective        = "binary:logistic", 
      eval_metric     = "auc"),    #Siyang's thought: change "auc" to "aucpr" to improve imbalance data performance    
      data = dtrain,
      nround = 500,        ## Set this large and use early stopping
      early_stopping_rounds = 25,
      folds=cv_folds,
      prediction = FALSE,  #whether to return the test fold predictions from each CV mode
      showsd = TRUE,       #whether to show standard deviation of cross validation
      verbose = 1,
      print_every_n = 30)

    cv$evaluation_log %>% pull(4) %>% max  ## column 4 is the eval metric here
  },
  
  par.set = makeParamSet(
    makeNumericParam("eta",              lower = 0.001, upper = 0.05),   # Siyang's thought: maybe try increase it to 0.1? default value is 0.3. larger learning rate will result in lower nrounds
    makeNumericParam("gamma",            lower = 0,     upper = 5),
    makeIntegerParam("max_depth",        lower= 2,      upper = 10),
    makeIntegerParam("min_child_weight", lower= 1,      upper = 10),
    makeNumericParam("subsample",        lower = .2,   upper = .9),
    makeNumericParam("colsample_bytree", lower = .2,   upper = .9),
    makeNumericParam("max_delta_step",lower = 0, upper = 0.75)  # Siyang's though: maybe increase the upper bound to 1 or above 1. help with extremely unbalanced data
  ),
  
  minimize = FALSE 
)

# 4c) generate the design, which are a set of hyperparameters that will be tested before starting the bayesian optimization
### generate an optimal design with only 10  points
des = generateDesign(n=10,
                     par.set = getParamSet(obj.fun), 
                     fun = lhs::randomLHS)  ## . If no design is given by the user, mlrMBO will generate a maximin Latin Hypercube Design of size 4 times the number of the black-box function’s parameters.

### i still want my favorite hyperparameters to be tested
simon_params <- data.frame(eta  = 0.01,
                           max_depth = 6,
                           gamma = 0 ,
                           min_child_weight = 3,
                           subsample = 0.8,
                           colsample_bytree= 0.8,
                           max_delta_step = 0
                           ) %>% as_tibble()

### final design  is a combination of latin hypercube optimization and my own preferred set of parameters
final_design =  simon_params  %>% bind_rows(des)
final_design

### bayes will have 100 additional iterations
control = makeMBOControl()
control = setMBOControlTermination(control, iters = 100)

# 4d) run the bayesian search
run = mbo(fun = obj.fun, 
          design = final_design,  
          control = control, 
          show.info = TRUE)
write_rds( run, here::here("run.rds"))

# 4e) return best hyperparameter
### print a summary with run
run

### return  best model hyperparameters 
best.params <- run$x
print(best.params)

### return best auc  - 0.7844
run$y

### return all results using run$opt.path$env$path
run$opt.path$env$path  %>% 
  mutate(Round = row_number()) %>%
  mutate(type = case_when(
    Round==1  ~ "1- hardcoded",
    Round<= 11 ~ "2 -design ",
    TRUE ~ "3 - mlrMBO optimization")) %>%
  ggplot(aes(x= Round, y= y, color= type)) + 
  geom_point() +
  labs(title = "mlrMBO optimization")+
  ylab("AUC")

plot(run)

# 4f) re-train the model using best hyperparameters
#-------------Record the best hyperparameters------------#
# $eta
# [1] 0.03749208
# 
# $gamma
# [1] 3.722242
# 
# $max_depth
# [1] 4
# 
# $min_child_weight
# [1] 3
# 
# $subsample
# [1] 0.8365318
# 
# $colsample_bytree
# [1] 0.3814702
# 
# $max_delta_step
# [1] 0.7157611

#$nrounds
#[1] 289
#-------------Record the best hyperparameters------------#

### However, I used early_stopping in xgb.cv() and it does not have nrounds fixed. I need to run one more evaluation calling xgb.cv()
optimal.cv <- xgb.cv(params = best.params,
                     booster = "gbtree",
                     objective= "binary:logistic", 
                     eval_metric = "auc",    #Siyang's thought: change "auc" to "aucpr" to improve imbalance data performance
                     data = dtrain,
                     nround = 500,
                     folds = cv_folds,
                     prediction = FALSE,
                     showsd = TRUE,
                     early_stopping_rounds = 25,
                     verbose = 1,
                     print_every_n = 30,
                     seed=42)

best.params$nrounds <- optimal.cv$best_ntreelimit
best.params[[8]] 

## finally, retrain the best learners
final.model <- xgboost(#params = best.params[[8]],
                       data = dtrain,
                       booster = "gbtree",
                       objective= "binary:logistic", 
                       eval_metric = "auc",    #Siyang's thought: change "auc" to "aucpr" to improve imbalance data performance
                       nrounds = 289,
                       verbose = 1,
                       print_every_n = 5,
                       eta = 0.04998109,
                       gamma = 3.263842,
                       max_depth = 6,
                       min_child_weight = 3,
                       subsample = 0.557327,
                       colsample_bytree = 0.7829977,
                       max_delta_step = 0.1559746
)

# 4g) use validation set to find the optimal threshold p
## Calculate net profit
net_profit_calc <- function(model,x_valid =x_validation,y_valid = y_validation, threshold_p=0.22) {
  #######
  ## This function tests different threshold and calculates net profit
  #######
  
  pred_proba <- predict(model,newdata=x_valid,type="response") 
  cm <- confusionMatrix(as.factor(ifelse(pred_proba>threshold_p,1,0)),
                        y_valid,positive="1") 
  true_negative <- cm$table[1,1]
  false_negative <- cm$table[1,2]
  false_positive <- cm$table[2,1]
  true_positive <- cm$table[2,2]
  
  net_profit <- true_negative*1500 - false_negative*5000
  return(net_profit)
}

net_profit_df <- NULL

for (p in seq(0, 1, by = 0.001)){
  net_profit <- net_profit_calc(model=final.model,threshold_p=p)
  net_profit_df <- rbind(net_profit_df,cbind(p,net_profit))
}

net_profit_df <- data.frame(net_profit_df)

ggplot(data=net_profit_df, aes(x=p, y=net_profit)) +
  geom_line()

optimal_threshold <- net_profit_df[which.max(net_profit_df$net_profit),]
optimal_threshold
#   p     net_profit
# 0.21     560,000

# 4h) performance measurement on test set
#Predict classification (for confusion matrix)
xgboost_pred_proba_v2<- predict(final.model,newdata=x_test,type="response")
print(xgboost_pred_proba_v2)

#Display confusion matrix 
cm_tuned <- confusionMatrix(as.factor(ifelse(xgboost_pred_proba_v2>0.21,1,0)),
                            y_test,positive="1") 
print(cm_tuned)

### ROC Curve
XGboost_ROC_prediction_v2 <- prediction(xgboost_pred_proba_v2, y_test) #Calculate errors
XGboost_ROC_testing_v2 <- ROCR::performance(XGboost_ROC_prediction_v2,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing_v2) 

####AUC - 0.7896828
auc.tmp_v2 <- ROCR::performance(XGboost_ROC_prediction_v2,"auc") #Create AUC data
XGboost_auc_testing_v2 <- as.numeric(auc.tmp_v2@y.values) #Calculate AUC
XGboost_auc_testing_v2 #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

# 3d) Lift Chart
#### Lift chart
plotLift(xgboost_pred_proba_v2, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_XGboost2 <- ROCR::performance(XGboost_ROC_prediction_v2,"lift","rpp")
plot(Lift_XGboost2)


#### ------ Performance measurement on applicant dataset---------- ####
pred <- predict(final.model,newdata=x_real_test,type="response")
print(pred)
prediction_1K <- as.numeric(pred > 0.21)
prediction_1K

write.csv(prediction_1K, "newapplicants_submission.csv") #for submission
write.csv(pred, "newapplicants_probability of default.csv") #for question 2

