################################################################
# 1. Setting the scene

rm(list=ls())

library(tidyverse)
library(modelsummary)
library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)
library(data.table)

install.packages("R.utils")
library(R.utils)
dt <- fread("./Assignment_2/listings.csv.gz")
write.csv(dt, file = "./Assignment_2/Rome_listings.csv")
