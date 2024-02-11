#### Preamble ####
# Purpose: Clean Study 2 Dataset
# Author: Mingjia Chen
# Date: 11 Febrary 2024
# Contact: btchen21@outlook.com
# License: MIT
# Pre-requisites: R 4.3.2

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(haven)

#### Clean data ####
data <- read_sav("Inputs_Folder/Data/FW satisfaction-Study 2-data.sav")

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

#### Save data ####
write.csv(data, "Outputs_Folder/Data/FW_satisfaction-Study_2-data.csv")
