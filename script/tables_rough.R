#download the packages if necessary, then load the packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, psych,dplyr,tidyverse, knitr,kableExtra)

library(haven)
data <- read_sav("FW satisfaction-Study 2-data.sav")

# Load the psych package
library(psych)
library(dplyr)
library(tidyverse)
library(knitr)

# Mutate data reversely similar to what paper has
data <- mutate(data,
                    fw_16r = case_when(
                      fw_16 == 1 ~ 6,
                      fw_16 == 2 ~ 5,
                      fw_16 == 3 ~ 4,
                      fw_16 == 4 ~ 3,
                      fw_16 == 5 ~ 2,
                      fw_16 == 6 ~ 1,
                      TRUE ~ fw_16
                    ),
                    fw_17r = case_when(
                      fw_17 == 1 ~ 6,
                      fw_17 == 2 ~ 5,
                      fw_17 == 3 ~ 4,
                      fw_17 == 4 ~ 3,
                      fw_17 == 5 ~ 2,
                      fw_17 == 6 ~ 1,
                      TRUE ~ fw_17
                    ),
                    fw_21r = case_when(
                      fw_21 == 1 ~ 6,
                      fw_21 == 2 ~ 5,
                      fw_21 == 3 ~ 4,
                      fw_21 == 4 ~ 3,
                      fw_21 == 5 ~ 2,
                      fw_21 == 6 ~ 1,
                      TRUE ~ fw_21
                    ),
                    fw_22r = case_when(
                      fw_22 == 1 ~ 6,
                      fw_22 == 2 ~ 5,
                      fw_22 == 3 ~ 4,
                      fw_22 == 4 ~ 3,
                      fw_22 == 5 ~ 2,
                      fw_22 == 6 ~ 1,
                      TRUE ~ fw_22
                    )
)

# Compute means for each group of variables
data$FWDfw <- rowMeans(data[, c("fw_1", "fw_2", "fw_3", "fw_4", "fw_5")])
data$FWDagency <- rowMeans(data[, c("fw_6", "fw_7", "fw_8", "fw_9")])
data$FWDmoral <- rowMeans(data[, c("fw_10", "fw_11", "fw_12", "fw_13", "fw_14", "fw_15")])
data$FWDpower <- rowMeans(data[, c("fw_16r", "fw_17r", "fw_18")])
data$FWDresp <- rowMeans(data[, c("fw_19", "fw_20")])
data$FWDlimit <- rowMeans(data[, c("fw_21r", "fw_22r")])
data$FWDall <- rowMeans(data[, c("fw_1", "fw_2", "fw_3", "fw_4", "fw_5", 
                                           "fw_10", "fw_11", "fw_12", "fw_13", "fw_14", "fw_15", 
                                           "fw_19", "fw_20", "fw_16r", "fw_17r", "fw_18", 
                                           "fw_21r", "fw_22r")])
data$FWDfwagency <- rowMeans(data[, c("fw_1", "fw_2", "fw_3", "fw_4", "fw_5", 
                                                "fw_6", "fw_7", "fw_8", "fw_9")])
data$FWself <- rowMeans(data[, c("fw_6", "fw_18", "fw_8", "fw_21r", "fw_3", "fw_7", "fw_22r", "fw_9")])
data$FWgeneral <- rowMeans(data[, c("fw_1", "fw_2", "fw_4", "fw_5", "fw_10", "fw_11", "fw_12", "fw_13",
                                    "fw_14", "fw_15", "fw_16r", "fw_17r", "fw_19", "fw_20")])

# Assign variable labels
variable.labels <- c("FWDfw" = "FW T1 Free will subscale",
                     "FWDagency" = "FW T1 personal agency subscale",
                     "FWDmoral" = "FW T1 Moral responsibility subscale",
                     "FWDpower" = "FW T1 Higher power control subscale",
                     "FWDresp" = "FW T1 Personal responsibility subscale",
                     "FWDlimit" = "FW T1 Personal limitations subscale",
                     "FWDall" = "FW T1 both scales",
                     "FWDfwagency" = "FW T1 agency and free will subscales",
                     "FWself" = "Free will beliefs - personal T1",
                     "FWgeneral" = "Free will beliefs - general T1")

# Assign variable labels to the variables
attr(data$FWDfw, "label") <- variable.labels["FWDfw"]
attr(data$FWDagency, "label") <- variable.labels["FWDagency"]
attr(data$FWDmoral, "label") <- variable.labels["FWDmoral"]
attr(data$FWDpower, "label") <- variable.labels["FWDpower"]
attr(data$FWDresp, "label") <- variable.labels["FWDresp"]
attr(data$FWDlimit, "label") <- variable.labels["FWDlimit"]
attr(data$FWDall, "label") <- variable.labels["FWDall"]
attr(data$FWDfwagency, "label") <- variable.labels["FWDfwagency"]
attr(data$FWself, "label") <- variable.labels["FWself"]
attr(data$FWgeneral, "label") <- variable.labels["FWgeneral"]

##### Table 1. Study 2 Means, Standard Deviations, and Correlations################################################################################
# Select the variables for correlation analysis
selected_variables <-data[, c("jobsat1", "jobsat2", "FWDfwagency", "jobaut", "jobaut2", "locus", "ess_kind", "selfest", "selfeff", "selfcontrol")]

# Compute pairwise correlations
correlation_matrix <- cor(selected_variables, use = "pairwise.complete.obs")

# modifying the format so it matches to the correlation table like in paper
## making the upper right part zero

## Add Mean and SD
M <- c(mean(selected_variables$jobsat1), mean(selected_variables$jobsat2,na.rm = TRUE), mean(selected_variables$FWDfwagency),
       mean(selected_variables$jobaut,na.rm = TRUE),mean(selected_variables$jobaut2,na.rm = TRUE),mean(selected_variables$locus),
       mean(selected_variables$ess_kind),mean(selected_variables$selfest),mean(selected_variables$selfeff),
       mean(selected_variables$selfcontrol))

SD <- c(sd(selected_variables$jobsat1), sd(selected_variables$jobsat2,na.rm = TRUE), sd(selected_variables$FWDfwagency),
       sd(selected_variables$jobaut,na.rm = TRUE),sd(selected_variables$jobaut2,na.rm = TRUE),sd(selected_variables$locus),
       sd(selected_variables$ess_kind),sd(selected_variables$selfest),sd(selected_variables$selfeff),
       sd(selected_variables$selfcontrol))

#choose desired rows
correlation_matrix <- correlation_matrix[,c('jobsat1','jobsat2','FWDfwagency','jobaut','jobaut2')]

correlation_matrix<- cbind(M, SD, correlation_matrix)

#rename columns and rows in r
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobsat1"] <- "Job satisfaction (T1)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobsat2"] <- "Job satisfaction (T2)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "FWDfwagency"] <- "Belief in free will (T1)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobaut"] <- "Job autonomy (T1)"
colnames(correlation_matrix)[colnames(correlation_matrix) == "jobaut2"] <- "Job autonomy (T2)"

rownames(correlation_matrix)[rownames(correlation_matrix) == "jobsat1"] <- "Job satisfaction (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "jobsat2"] <- "Job satisfaction (T2)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "FWDfwagency"] <- "Belief in free will (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "jobaut"] <- "Job autonomy (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "jobaut2"] <- "Job autonomy (T2)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "locus"] <- "Trait locus of control (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "ess_kind"] <- "Implicit beliefs (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "selfest"] <- "Trait self-esteem (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "selfeff"] <- "Job self-efficacy (T1)"
rownames(correlation_matrix)[rownames(correlation_matrix) == "selfcontrol"] <- "Trait self-control (T1)"


#### Table 2 (Appendix) - Correlations between free will subscales and job satisfaction ########################################
#Correlations between free will subscales and job satisfaction
# Select the variables for correlation analysis
selected_variables_2 <-data[, c("jobsat1", "jobsat2", "FWDfw", "FWDagency", "FWDmoral","FWDresp",
                              "FWDpower","FWDlimit","FWDall","FWDfwagency","FWself","FWgeneral")]

# Compute pairwise correlations
correlation_matrix_2 <- cor(selected_variables_2, use = "pairwise.complete.obs")

#choose desired rows and columns
correlation_matrix_2 <- correlation_matrix_2[c('FWDfwagency','FWDall','FWDfw','FWDagency','FWDmoral','FWDpower',
                                               "FWDresp",'FWDlimit','FWself','FWgeneral'),]
correlation_matrix_2 <- correlation_matrix_2[,c('jobsat1','jobsat2')]

#rename columns and rows 
colnames(correlation_matrix_2)[colnames(correlation_matrix_2) == "jobsat1"] <- "Job satisfaction T1"
colnames(correlation_matrix_2)[colnames(correlation_matrix_2) == "jobsat2"] <- "Job satisfaction T2"

rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDfwagency"] <- "FW T1 Agency and free will subscales"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDall"] <- "FW T1 All subscales combined"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDfw"] <- "FW T1 Free will subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDagency"] <- "FW T1 Personal agency subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDmoral"] <- "FW T1 Moral responsibility subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDpower"] <- "FW T1 Higher power control subscale (R)"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDresp"] <- "FW T1 Personal responsibility subscale"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWDlimit"] <- "FW T1 Personal limitations subscale (R)"

rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWself"] <- "FW T1 - Personal"
rownames(correlation_matrix_2)[rownames(correlation_matrix_2) == "FWgeneral"] <- "FW T1 - General"

#### Table 3-Controlling for demographics#####################################################################################
selected_variables_3 <-data[, c("FWgeneral","jobsat1", "jobsat2","age","gender")]

# Compute pairwise correlations
correlation_matrix_3 <- cor(selected_variables_3, use = "pairwise.complete.obs")
#choose desired rows and columns
correlation_matrix_3 <- correlation_matrix_3[,c('FWgeneral','jobsat1','jobsat2')]

#rename columns and rows 
colnames(correlation_matrix_3)[colnames(correlation_matrix_3) == "FWgeneral"] <- "Belief in free will (T1)"
colnames(correlation_matrix_3)[colnames(correlation_matrix_3) == "jobsat1"] <- "Job satisfaction (T1)"
colnames(correlation_matrix_3)[colnames(correlation_matrix_3) == "jobsat2"] <- "Job satisfaction (T2)"

rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "FWgeneral"] <- "Belief in free will (T1)"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "jobsat1"] <- "Job satisfaction (T1)"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "jobsat2"] <- "Job satisfaction (T2)"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "age"] <- "Age"
rownames(correlation_matrix_3)[rownames(correlation_matrix_3) == "gender"] <- "Gender"

