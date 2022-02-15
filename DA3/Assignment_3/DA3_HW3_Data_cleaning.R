
################################
##       Daniel Gyebnar       ##
##        Data Analysis 3     ##
##         Assignment 3       ##   
##                            ##
################################

####################################
# SETTING THE SCENE

# CLEAR MEMORY
rm(list=ls())

# libs
library(tidyverse)
library(dplyr)
library(huxtable)
library(estimatr)
library(stargazer)
library(modelsummary)
library(grid)
library(fixest)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(ranger)
#install.packages("gmodels")
library(gmodels) 
# install.packages("viridis")
library(viridis)
# install.packages("pROC")
library(pROC)
library(skimr)
library(stats)

####################################
# LOAD DATA AND CHECK


# load data
data_raw <- read_csv("https://raw.githubusercontent.com/DaniDataScience/DA_3/main/DA3/Assignment_3/data/cs_bisnode_panel.csv")
data <- data_raw
# checking out
glimpse(data)
skim(data) # many missing in COGS, finished_prod, net_dom_sales, net_exp_sales, wages, exit_year


to_filter <- sapply(data, function(x) sum(is.na(x)))
sort(to_filter[to_filter > 0])

# drop variables with too many NAs more than 200k and filter years between 2012-2014 
# with full year balance sheet indicating they are not new firms
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages, D, exit_year, exit_date)) %>%
  filter(year >= 2010,
         year <= 2015,
         balsheet_length >= 360)

# Label Engineering -------------------------------------------------------

# generate status_alive to check the firm is still alive
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))


# Create log sales and sales in million
# We have negative sales values
summary(data$sales)

data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))

data$sales_mil_log_sq <- (data$sales_mil_log)^2 


# Filter out non-alive firms
data <- data %>%
  filter(status_alive == 1) %>%
  # look at firms below 10m euro revenues and above 1000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))

# save backup
df <- data
data <- df

############################################

# I will use the 2012-2014 approach, because a one-year fast growth for a young firm can happen due to extraordinary event in that year


# Change in sales
data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - lag(sales_mil_log, 2) ) %>%
  ungroup()


# replace w 0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))

data <- data %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )

# plotting hist
plot_sales_mil_log <- ggplot(data=data, aes(x=sales_mil_log)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),bins=50, boundary=0,
                 color = "black", fill = "deepskyblue4") +
  coord_cartesian(xlim = c(-8, 8)) +
  labs(x = "CAGR growth",y = "Percent")+
  theme_bw() 

png('plot_sales_mil_log.png', width=400, height=300)
plot(plot_sales_mil_log)
dev.off()


# add previous growth as a variable
data <- data %>% 
  mutate(
    cagr_sales_11_12 = ((sales_mil / lag(sales_mil,1))^(1/1)-1)*100,
    cagr_sales_10_11 = ((lag(sales_mil,1) / lag(sales_mil,2))^(1/1)-1)*100
  )

data <- data %>%
  mutate(
    cagr_sales_10_11 = ifelse(is.na(cagr_sales_10_11), 0, cagr_sales_10_11),
    cagr_sales_11_12 = ifelse(is.na(cagr_sales_11_12), 0, cagr_sales_11_12))


sum(is.na(data$cagr_sales_11_12))
sum(is.na(data$cagr_sales_10_11))

view(data %>% select(c(comp_id, year, sales_mil, cagr_sales_10_11, cagr_sales_11_12)))

# CAGR sales change in the last 2 years (2012-2014)
data <- data %>%
  group_by(comp_id) %>%
  mutate(cagr_sales = ((lead(sales_mil,2) / sales_mil)^(1/2)-1)*100)

#view(data %>% select(c(comp_id, year, sales_mil, sales_mil_log, d1_sales_mil_log, cagr_sales)))

# select observations
data <- data %>%
  filter(year == 2012,
         cagr_sales != is.na(cagr_sales),
         cagr_sales <= 3000)

plot_cagr_hist <- ggplot(data=data, aes(x=cagr_sales)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
                 color = "black", fill = "deepskyblue4") +
  coord_cartesian(xlim = c(-100, 200)) +
  labs(x = "CAGR growth",y = "Percent")+
  theme_bw() 

png('plot_cagr_hist.png', width=400, height=300)
plot(plot_cagr_hist)
dev.off()

# Create fast growth dummy as cagr larger than 50%
data <- data %>%
  group_by(comp_id) %>%
  mutate(fast_growth = (cagr_sales > 50) %>%
           as.numeric(.)) %>%
  ungroup()

data <- data %>%
  mutate(age = (year - founded_year))

# save backup
df <- data
data <- df


###########################################################
# Feature engineering
###########################################################

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

table(data$ind2_cat)

# Firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))



###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

# Compare the original and winsored data
winsorisation <- ggplot(data = data, aes(x=d1_sales_mil_log, y=d1_sales_mil_log_mod)) +
  geom_point(size=0.1,  shape=20, stroke=2, fill='blue', color='blue') +
  labs(x = "Growth rate (Diff of ln sales) (original)",y = "Growth rate (Diff of ln sales) (winsorized)") +
  theme_bw() +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, 1)) +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, 1))

png('winsorisation.png', width=400, height=300)
plot(winsorisation)
dev.off()

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(fast_growth_f = factor(fast_growth, levels = c(0,1)) %>%
           recode(., `0` = 'no_fast_growth', `1` = "fast_growth"))

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
data <- data %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(data$age)

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

ggplot(data = data, aes(x=inc_bef_tax_pl, y=as.numeric(fast_growth))) +
  geom_point(size=0.01,  shape=20, stroke=2, fill="blue", color="blue", alpha=0.01) +
  geom_smooth(method="loess", se=F, colour="black", size=1.5, span=0.9) +
  labs(x = "Income before taxes",y = "Fast Growth distribution") +
  theme_bw() +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5))


# check NAs
to_filter <- sapply(data, function(x) sum(is.na(x)))
sort(to_filter[to_filter > 0])

# check variables
# datasummary_skim(data, type="numeric")

write_csv(data, "data/bisnode_firms_clean.csv")
