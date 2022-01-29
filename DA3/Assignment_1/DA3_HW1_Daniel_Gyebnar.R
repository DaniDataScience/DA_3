################################################################
# 1. Setting the scene


rm(list=ls())

library(kableExtra)
library(tidyverse)
library(dplyr)
library(huxtable)
library(estimatr)
library(stargazer)
library(modelsummary)
library(grid)
library(fixest)
library(caret)


## Accessing the data
data_all <-  read_csv( 'https://osf.io/4ay9x/download' ) 

# Selecting observations
data <- data_all %>% filter(occ2012==710)                # selecting Computer and Information System management
data <- data %>% filter(uhours>=40)                       # full-time employees
data <- data %>% filter(earnwke>10)                       # possible error


################################################################
# Preliminary data exploration

# hourly wages
ggplot(data) +
  geom_histogram(aes(x=data$w, y = ..density..),bins=20, fill="navyblue", color="white") +
  scale_x_continuous( limits = c( 0 , 60 ) ) +
  labs( x = "Hourly wages [USD]" , y = 'Relative Frequency', fill = "female" ) +
  theme_bw() +
  geom_segment( aes(x = mean( data$w , na.rm = T ), y = 0, xend = mean( data$w , na.rm = T ), yend = 0.04) , color = 'red', size = 1 ) + 
  annotate( "text" , size=3, x = mean( data$w , na.rm = T )+0.4 , y = 0.045 , label = 'Mean' , color = 'red') +
  annotate( "text" , size=3, x = mean( data$w , na.rm = T )+0.4 , y = 0.043 , label = round(mean( data$w , na.rm = T ),3) , color = 'red') +
  geom_segment( aes(x = median( data$w , na.rm = T ), y = 0, xend = median( data$w , na.rm = T ), yend = 0.04) , color = 'orange', size = 1 ) + 
  annotate( "text" , size=3, x = median( data$w , na.rm = T )-0.4 , y = 0.045 , label = 'Median' , color = 'orange') +
  annotate( "text" , size=3, x = median( data$w , na.rm = T )-0.4 , y = 0.043 , label = round(median( data$w , na.rm = T ),3) , color = 'orange') +  theme_bw()

# checking key variables
data %>% dplyr::select(earnwke,uhours) %>% summary()

# creating hourly earning per hous
data <- data %>% mutate(w=earnwke/uhours)                 # calculating hourly wage
#dropping not needed variables

# Checking distribution
data %>% dplyr::select(w) %>% summary()

data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  theme_bw()+
  facet_wrap(~key, scales = "free") +
  geom_histogram()


# looking at data key stats & missing values
datasummary_skim( data )

################################################################
# 3. Looking at variable distributions and transforming them

# ..1
data <- data %>%
  select(-c(1))

# hhid
data <- data %>%
  select(-c(hhid))

# intmonth
data <- data %>%
  select(-c(intmonth))

# stfips
datasummary( w*factor(stfips) ~ N + SD + Percent() + Mean, data = data )
data <- data %>%
  select(-c(stfips))

# weight
ggplot(data,aes(weight,w))+geom_point()+geom_smooth(method="loess")
data <- data %>%
  select(-c(weight))

# grade
datasummary( w*factor(grade92) ~ N + SD + Percent() + Mean, data = data )
  # regrouping
  data <- data %>% mutate(grade92=case_when(
    grade92<=42 ~ "No degree",
    grade92==43 ~ "BsC",
    grade92==44 ~ "MsC",
    grade92>=45 ~ "Higher than MsC"
  ))
  # checking result
  datasummary( w*factor(grade92) ~ N + SD + Percent() + Mean, data = data )
  # include as factor
  data$grade92 <- factor(data$grade92)

# race
datasummary( w*factor(race) ~ N + SD +Percent() + Mean, data = data )
  # creating binary
  data <- data %>% mutate(white=case_when(
    race==1 ~ 1,
    TRUE ~ 0
  ))
  #checking result
  datasummary( w*factor(white) ~ N + SD + Percent() + Mean, data = data )
  # include as factor
  data$white <- factor(data$white)
  #dropping original race
  data <- data %>%
    select(-c(race))
  
  
# ethnic
datasummary( w*factor(ethnic) ~ N + Percent() + Mean, data = data ) 
  # manage missing: set as factors
  data$ethnic<- fct_explicit_na(factor(data$ethnic), na_level = "Missing")
  # 95% is missing -> create binary
  data <- data %>% mutate(ethnic_missing=case_when(
    ethnic=="Missing" ~ 1,
    TRUE ~ 0
  ))
  # checking result
    datasummary( w*factor(ethnic_missing) ~ N + Percent() + Mean, data = data )
  # no real difference -> exclude from analysis
    data <- data %>%
      select(-c(ethnic_missing, ethnic))


# age
datasummary( w*factor(age) ~ N + Percent() + Mean, data = data )
  ggplot(data, aes(x=age, y=w)) +
    geom_point() +
    geom_smooth(method="loess")
    # creating quadratic
    data$age2 <- data$age*data$age
    
    # checking formula for modeling -> could be quadratic
    ggplot(data, aes(x=age, y =w)) +
      geom_point(fill="navyblue", size=0.1) +
      geom_smooth(method="loess") +
      labs( x = "Age" , y = 'Hourly wages [USD]', fill = "female" ) +
      theme_bw()

# sex
datasummary( w*factor(sex) ~ N + Percent() + Mean, data = data ) 
  # include as factor
  data$sex <- factor(data$sex)

# marital
datasummary( w*factor(marital) ~ N +SD + Percent() + Mean, data = data )
  # creating simpler groups
  data <- data %>% mutate(marital=case_when(
    marital==1 ~ "Married",
    marital==7 ~ "Never married",
    TRUE ~ "Used to be married"
  ))
  # ccreating factor
  data$marital <- factor(data$marital)

# ownchild
datasummary( w*factor(ownchild) ~ N + SD + Mean, data = data ) 
  # creating simpler groups
  data <- data %>% mutate(ownchild=case_when(
    ownchild==0 ~ 0,
    TRUE ~ 1))
  # checking result
  datasummary( w*factor(ownchild) ~ N + SD + Mean, data = data ) 
  # creating factor
  data$ownchild <- factor(data$ownchild)
  

# chldpres
datasummary( w*factor(chldpres) ~ N + Percent() + Mean, data = data ) 
  # creating simpler groups
  data <- data %>% mutate(chldpres=case_when(
    chldpres==0 ~ 0,
    TRUE ~ 1))
  # checking result
  datasummary( w*factor(chldpres) ~ N + SD + Mean, data = data ) 
  # dropping as it is the same as ownchild
  data <- data %>%
    select(-chldpres)

# prcitshp
datasummary( w*factor(prcitshp) ~ N + Percent() + Mean, data = data ) 
  # include as factor
  data$prcitshp <- factor(data$prcitshp)

# state 
datasummary( w*factor(state) ~ N + Percent() + Mean, data = data ) 
  # exclude from analysis
  data <- data %>% select(-state)

# ind02
datasummary( w*factor(ind02) ~ N + Percent() + Mean, data = data ) 
  # exclude from model
  data <- data %>% select(-ind02)

# class
  datasummary( w*factor(class) ~ N + Percent() + Mean, data = data ) 
  # include as factor
  data$class <- factor(data$class)

# unionmme
datasummary( w*factor(unionmme) ~ N + Percent() + Mean, data = data ) 
  # exlude from model
  data <- data %>% select(-unionmme)
  
# unioncov
datasummary( w*factor(unioncov) ~ N + Percent() + Mean, data = data ) 
  # exlude from model
  data <- data %>% select(-unioncov)
  
# lfsr94
datasummary( w*factor(lfsr94) ~ N + Percent() + Mean, data = data ) 
  # exlude from model
  data <- data %>% select(-lfsr94)
  

#########################
# Setting up models 

##
# A) Creating interactions:
# a) Check the interactions for various variables with datasummary

  # age and sex
  ggplot(data, aes(x=age, y=w, color=sex)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method="loess", se=FALSE)  +
    labs( x = "Age" , y = 'Hourly wages [USD]', fill = "female" ) +
    theme_bw() +
    ggtitle("Hourly wages per age and sex")
  
# age and place of birth
  ggplot(data, aes(x=age, y=w, color=prcitshp)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method="loess", se=FALSE) +
    labs( x = "Age" , y = 'Hourly wages [USD]' ) +
    theme_bw() +
    ggtitle("Hourly wages per age and citizenship")
  
  
# degree and age
  ggplot(data, aes(x=age, y=w, color=grade92)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method="loess", se=FALSE)  +
    labs( x = "Age" , y = 'Hourly wages [USD]' ) +
    theme_bw() +
    ggtitle("Hourly wages per age and educational level")

# sex and child
datasummary( w*ownchild*sex ~ N + Percent() + Mean, data = data ) 

# race and degree
datasummary( w*grade92*white ~ N + Percent() + Mean, data = data ) 

# class and degree
datasummary( w*grade92*class ~ N + Percent() + Mean, data = data ) 

#######################
# Create model setups

model1 <- as.formula(w ~ grade92 + age )
model2 <- as.formula(w ~ grade92 + age + sex + prcitshp + white + ownchild + age2)
model3 <- as.formula(w ~ grade92 + age + sex + prcitshp + white + ownchild + class + age*grade92 + age*sex + age2)
model4 <- as.formula(w ~ grade92 + age + sex + prcitshp + white + ownchild + class + marital + age*grade92 + age*sex + age*prcitshp + sex*ownchild + white*grade92 + age2 + age2*grade92 + age2*sex + age2*ownchild + age2*prcitshp)

##################################
# Running simple OLS on full sample

reg1 <- fixest::feols(model1, data=data, vcov = 'hetero')
reg2 <- fixest::feols(model2, data=data, vcov = 'hetero')
reg3 <- fixest::feols(model3, data=data, vcov = 'hetero')
reg4 <- fixest::feols(model4, data=data, vcov = 'hetero')

# evaluation of the models: using all the sample
fixest::fitstat_register("k", function(x){length( x$coefficients ) - 1}, "No. Variables")           # adds another metrix to the etable
fixest::etable( reg1 , reg2 , reg3 , reg4  , fitstat = c('aic','bic','rmse','r2','n','k'), keepFactors = TRUE )


# Calculate RMSE for each fold and the average RMSE as well
models <- c("reg1", "reg2", "reg3", "reg4")
rmse_full_sample <- c()


##################################
# Cross validation

k <- 4

set.seed(13505)
cv1 <- train(model1, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv2 <- train(model2, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv3 <- train(model3, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv4 <- train(model4, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")


############################
# creating summary tables

# k fold results for rmse
  cv <- c("cv1", "cv2", "cv3", "cv4")
  rmse_kfold <- c()

  for(i in 1:length(cv)){  
  rmse_kfold[i] <- mean( get(cv[i])$resample$RMSE )}


# full sample results
  # calculating rmse
  rmse_full_sample <- c(
                      RMSE( predict(reg1, data), data$w, na.rm=TRUE),
                      RMSE( predict(reg2, data), data$w, na.rm=TRUE),
                      RMSE( predict(reg3, data), data$w, na.rm=TRUE),
                      RMSE( predict(reg4, data), data$w, na.rm=TRUE))

  # calculating BIC
  bic_full_sample <- c(
    BIC(reg1),
    BIC(reg2),
    BIC(reg3),
    BIC(reg4)
  )

# creating table
  colnames <- c("Model1", "Model2", "Model3", "Model4")
  Full_sample_summary <- data.frame("Models"=colnames, "RMSE_full_sample"=rmse_full_sample, "BIC_full_sample"=bic_full_sample, "RMSE_k_fold_avg"=rmse_kfold)
  Full_sample_summary

# Visualizing k fold results
  m_comp <- c()
  models <- c("reg1", "reg2", "reg3", "reg4")
  for( i in 1 : length(cv) ){
    m_comp[ i ] <- length( get( models[i] )$coefficient  - 1 ) 
  }
  
  m_comp <- tibble( model = models , 
                    complexity = m_comp,
                    RMSE = rmse_kfold )
  
  ggplot( m_comp , aes( x = complexity , y = RMSE ) ) +
    geom_point(color='red',size=2) +
    geom_line(color='blue',size=0.5)+
    labs(x='Number of explanatory variables',y='Averaged RMSE on test samples',
         title='Prediction performance and model compexity') +
    theme_bw()
  
# plotting results
ggplot(data, aes(x=predict(reg2, data), y=w)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  scale_x_continuous(limits = c(0,60)) + 
  scale_y_continuous(limits = c(0,60))
  