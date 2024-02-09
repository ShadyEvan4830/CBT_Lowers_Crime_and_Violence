if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, psych,dplyr,tidyverse, kableExtra)

library(haven)
setwd("C:/Users/BtChe/Documents/24 Winter/INF312/Paper II/Free-will/Data and code/Study 2")
data <- read_sav("FW satisfaction-Study 2-data.sav")

# Load the psych package
library(psych)
library(dplyr)
library(tidyverse)

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

# Assign variable labels
variable.labels <- c("FWDfw" = "FW T1 Free will subscale",
                     "FWDagency" = "FW T1 personal agency subscale",
                     "FWDmoral" = "FW T1 Moral responsibility subscale",
                     "FWDpower" = "FW T1 Higher power control subscale",
                     "FWDresp" = "FW T1 Personal responsibility subscale",
                     "FWDlimit" = "FW T1 Personal limitations subscale",
                     "FWDall" = "FW T1 both scales",
                     "FWDfwagency" = "FW T1 agency and free will subscales")

# Assign variable labels to the variables
attr(data$FWDfw, "label") <- variable.labels["FWDfw"]
attr(data$FWDagency, "label") <- variable.labels["FWDagency"]
attr(data$FWDmoral, "label") <- variable.labels["FWDmoral"]
attr(data$FWDpower, "label") <- variable.labels["FWDpower"]
attr(data$FWDresp, "label") <- variable.labels["FWDresp"]
attr(data$FWDlimit, "label") <- variable.labels["FWDlimit"]
attr(data$FWDall, "label") <- variable.labels["FWDall"]
attr(data$FWDfwagency, "label") <- variable.labels["FWDfwagency"]

# Select the variables for correlation analysis
selected_variables <-data[, c("jobsat1", "jobsat2", "FWDfwagency", "jobaut", "jobaut2", "locus", "ess_kind", "selfest", "selfeff", "selfcontrol")]

# Compute pairwise correlations
correlation_matrix <- cor(selected_variables, use = "pairwise.complete.obs")

## Add Mean and SD
M <- c(mean(selected_variables$jobsat1), mean(selected_variables$jobsat2,na.rm = TRUE), mean(selected_variables$FWDfwagency),
       mean(selected_variables$jobaut,na.rm = TRUE),mean(selected_variables$jobaut2,na.rm = TRUE),mean(selected_variables$locus),
       mean(selected_variables$ess_kind),mean(selected_variables$selfest),mean(selected_variables$selfeff),
       mean(selected_variables$selfcontrol))

SD <- c(sd(selected_variables$jobsat1), sd(selected_variables$jobsat2,na.rm = TRUE), sd(selected_variables$FWDfwagency),
       sd(selected_variables$jobaut,na.rm = TRUE),sd(selected_variables$jobaut2,na.rm = TRUE),sd(selected_variables$locus),
       sd(selected_variables$ess_kind),sd(selected_variables$selfest),sd(selected_variables$selfeff),
       sd(selected_variables$selfcontrol))

correlation_matrix <- correlation_matrix[,c('jobsat1','jobsat2','FWDfwagency','jobaut','jobaut2')]

correlation_matrix<- cbind(M, SD, correlation_matrix)

# Print the correlation matrix
print(correlation_matrix)
