######################
# 0. Setting the scene
#

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
library(data.table)
library(lubridate)
library(stringr)
library(ranger)

df_raw <- read.csv("https://raw.githubusercontent.com/DaniDataScience/DA_3/main/DA3/Assignment_2/Rome_listings.csv",
                   header=T,
                   na.strings=c("","N/A", " ", "NA"))

# selecting variables
df <- data.table(df_raw)
  # accomodating 2-6 people
  df <- df[accommodates>=2 & accommodates<=6]
  # accomodation type
  table(df_raw$property_type)
  df <- df[
      property_type == "Entire rental unit" |
      property_type == "Private room in rental unit" |
      property_type == "Entire condominium (condo)" |
      property_type == "Private room in condominium (condo)" |
      property_type == "Entire loft" ]

# de-selecting unneeded columns
df <- df %>% select(-c("X",
                           "name",
                           "listing_url",
                           "scrape_id",
                           "neighborhood_overview",
                           "picture_url",
                           "last_scraped",
                           "host_id",
                           "description",
                           "host_about",
                           "host_url",
                           "host_name",
                           "host_thumbnail_url",
                           "host_url",
                           "host_response_rate",
                           "host_acceptance_rate",
                           "host_total_listings_count",
                           "host_verifications",
                           "host_identity_verified",
                           "latitude",
                           "longitude",
                           "host_listings_count",
                           "host_picture_url",
                           "host_neighbourhood",
                           "neighbourhood",
                           "neighbourhood_group_cleansed",
                           "bathrooms",
                           "availability_30",
                           "availability_60",
                           "availability_90",
                           "availability_365",
                           "maximum_nights",
                           "first_review",
                           "last_review",
                           "number_of_reviews_ltm",
                           "number_of_reviews_l30d",
                           "calendar_last_scraped",
                           "minimum_minimum_nights",
                           "maximum_maximum_nights",
                           "minimum_maximum_nights",
                           "maximum_minimum_nights",
                           "minimum_nights_avg_ntm",
                           "maximum_nights_avg_ntm",
                           "calendar_updated",
                           "calculated_host_listings_count_shared_rooms",
                           "calculated_host_listings_count_private_rooms",
                           "calculated_host_listings_count_entire_homes",
                           "license"
                           ))


glimpse(df)
# looking at data key stats & missing values
datasummary_skim( df )


######################
#  Check price
#
#

# Price
  
  # drop NA
  df <- df %>% drop_na(price)
  # getting rid of $ sign
  df$price_2 <- str_sub(df$price, start = 2)
  # getting rid of commas
  df$price_2 <- str_replace(df$price_2,",","")
  # transforming to numeric
  df$price_2 <- as.numeric(df$price_2)
  # checking if there are missing values introduced
  sapply(select(df, c("price", "price_2")), function(x) sum(is.na(x))) # no missing values
  # renaming original
  df$price <- df$price_2
  #dropping temp 
  df <- df %>%
    select(-c(price_2))
  # making sure its numeric
  df$price <- as.numeric(df$price)
  

  # exploring distribution
  df %>% dplyr::select(price) %>% summary()
  
  # restricting price per night to a reasonable range our business would operate within
  df <- df[price<500]
  #getting histogram
  ggplot(df) +
    geom_histogram(aes(x=price, y = ..density..),bins=70, fill="navyblue", color="white") +
    scale_x_continuous(  ) +
    labs( x = "Price per night [USD]" , y = 'Relative Frequency' ) +
    theme_bw() 
  
######################
# Dealing with NAs
#
  
  
# where do we have missing variables?
  to_filter <- sapply(df, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]


# Only a few NA in numerics:: replace with mode
  df <- df %>% mutate(
    host_since =  ifelse(is.na(host_since), mode(host_since), host_since), 
    host_location = ifelse(is.na(host_location),  mode(host_location), host_location),
    host_is_superhost = ifelse(is.na(host_is_superhost),  mode(host_is_superhost), host_is_superhost),
    host_has_profile_pic = ifelse(is.na(host_has_profile_pic),  mode(host_has_profile_pic), host_has_profile_pic),
    bathrooms_text = ifelse(is.na(bathrooms_text), mode(bathrooms_text), bathrooms_text)
  )
  
  
# NA categorical: Create category missing and flag it
  df <- df %>% mutate(
    f_flag_host_response_time = ifelse(is.na(host_response_time),1, 0),
    f_host_response_time =  as.factor(ifelse(is.na(host_response_time), "Missing", host_response_time))
  )
  

# Too many NA in numerics: label as missing for reviews and insert zero
  df <- df %>%
    mutate(
      
      n_flag_review_scores_rating = ifelse(is.na(review_scores_rating),1, 0),
      n_review_scores_rating =  ifelse(is.na(review_scores_rating), 0, review_scores_rating),
      
      n_flag_reviews_per_month = ifelse(is.na(reviews_per_month),1, 0),
      n_reviews_per_month =  ifelse(is.na(reviews_per_month), 0, reviews_per_month),
      
      n_flag_review_scores_cleanliness = ifelse(is.na(review_scores_cleanliness),1, 0),
      n_review_scores_cleanliness =  ifelse(is.na(review_scores_cleanliness), 0, review_scores_cleanliness),
      
      n_flag_review_scores_accuracy = ifelse(is.na(review_scores_accuracy),1, 0),
      n_review_scores_accuracy =  ifelse(is.na(review_scores_accuracy), 0, review_scores_accuracy),
      
      n_flag_review_scores_communication = ifelse(is.na(review_scores_communication),1, 0),
      n_review_scores_communication =  ifelse(is.na(review_scores_communication), 0, review_scores_communication),
      
      n_flag_review_scores_checkin = ifelse(is.na(review_scores_checkin),1, 0),
      n_review_scores_checkin =  ifelse(is.na(review_scores_checkin), 0, review_scores_checkin),
      
      n_flag_review_scores_location = ifelse(is.na(review_scores_location),1, 0),
      n_review_scores_location =  ifelse(is.na(review_scores_location), 0, review_scores_location),
      
      n_flag_review_scores_value = ifelse(is.na(review_scores_value),1, 0),
      n_review_scores_value =  ifelse(is.na(review_scores_value), 0, review_scores_value),
      )
  
  df <- df %>% select(-c("review_scores_rating", "reviews_per_month", "review_scores_cleanliness",
                         "review_scores_accuracy","review_scores_communication", "review_scores_checkin",
                         "review_scores_location","review_scores_value","host_response_time"))

# Deal with missing bedrooms and beds
  table(df$beds)
  df <- df %>% mutate(
    beds = ifelse(is.na(beds), accommodates, beds),
    bedrooms = ifelse(is.na(bedrooms), round(accommodates/2), bedrooms)
  )
  table(df$beds)
  
# Check is no more missing  
  to_filter <- sapply(df, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]
  
######################
# Creating binary dummy variables
#
    
# host location
    # Create dummy variable for local host or not
    # replacing with missing
    table(df$host_location)
    df <- df %>% mutate(d_host_local=case_when(
      host_location=="Rome, Lazio, Italy" ~ 1,
      TRUE ~ 0
    ))
    datasummary( factor(d_host_local)*price ~ N + Percent() + Mean, data = df ) 
    
    
# host is superhost
    table(df$host_is_superhost)
    df <- df %>% mutate(d_host_is_superhost=case_when(
      host_is_superhost=="f" ~ 0,
      TRUE ~ 1
    ))
    datasummary( factor(d_host_is_superhost)*price ~ N + Percent() + Mean + SD, data = df ) 
    
# host_has_profile_pic
  # check values <- drop missing (only 16)
  table(df_raw$host_has_profile_pic)
  df <- df %>% mutate(d_host_has_profile_pic=case_when(
    host_has_profile_pic=="f" ~ 0,
    TRUE ~ 1
  ))
  datasummary( factor(d_host_has_profile_pic)*price ~ N + Percent() + Mean + SD, data = df ) 
  
# has availability
  table(df$has_availability)
  df <- df %>% mutate(d_has_availability=case_when(
    has_availability=="f" ~ 0,
    TRUE ~ 1
  ))
  datasummary( factor(d_has_availability)*price ~ N + Percent() + Mean + SD, data = df )
  
######################
#
# Transform to factor
#
#
    
# neighborhood_cleansed
    # check values <- drop missing (only 16)
    table(df_raw$neighbourhood_cleansed)
    df$f_neighbourhood_cleansed <- as.factor(df$neighbourhood_cleansed)
    datasummary( neighbourhood_cleansed*price ~ N + Percent() + Mean + SD, data = df ) 
    
# property type
  table(df$property_type)
  df$f_property_type <- as.factor(df$property_type)
  datasummary( f_property_type*price ~ N + Percent() + Mean + SD, data = df ) 

  # boxplot of distributions  
  ggplot(data = df, aes(x = f_property_type , y = price)) +
    stat_boxplot(aes(group = f_property_type), geom = "errorbar", width = 0.3,
                 size = 0.5, na.rm=T)+
    geom_boxplot(aes(group = f_property_type),
                 size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
    scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
    labs(x = "Room type",y = "Price (US dollars)") +
    theme(axis.text.x = element_text(angle = 90))
  
# room type
  table(df$room_type)
  # creating dummy variables
  df$f_room_type <- as.factor(df$room_type)
  datasummary( f_room_type*price ~ N + Percent() + Mean + SD, data = df ) 
  
# bathrooms text
  table(df$bathrooms_text)
  # lets separate private (and shared bath)zero = shared)
  
  # private
    df <- df %>% mutate(bathrooms_private=case_when(
      grepl(pattern = "shared", x = df$bathrooms_text) ~ "0",
      gsub("\\ .*","",df$bathrooms_text) == "" ~ "0",
      TRUE ~ gsub("\\ .*","",df$bathrooms_text)
    ))
    # check result
    table(df$bathrooms_private)
    # simplify categories
    df <- df %>% mutate(f_bathrooms_private=as.factor(case_when(
      bathrooms_private == "0" ~ "0",
      bathrooms_private == "1" ~ "1",
      bathrooms_private == "1.5" ~ "2",
      bathrooms_private == "2" ~ "2",
      bathrooms_private == "2.5" ~ "3",
      bathrooms_private == "3" ~ "3",
      TRUE ~ "Above 3"
    )))
    # checking result
    datasummary( f_bathrooms_private*price ~ N + Percent() + Mean + SD, data = df )

# bedrooms
    # checking values
    table(df$bedrooms)
    #  lets group above 4
    df <- df %>% mutate(f_bedrooms=as.factor(case_when(
      bedrooms == "1" ~ "1",
      bedrooms == "2" ~ "2",
      bedrooms == "3" ~ "3",
      TRUE ~ "4 and above"
    )))
    datasummary( factor(f_bedrooms)*price ~ N + Percent() + Mean + SD, data = df )
    
# beds
    # checking values
    table(df$beds)
    #  lets group above 4
    df <- df %>% mutate(f_beds=as.factor(case_when(
      beds == "1" ~ "1",
      beds == "2" ~ "2",
      beds == "3" ~ "3",
      TRUE ~ "4 and above"
    )))
    datasummary( factor(f_beds)*price ~ N + Percent() + Mean + SD, data = df )


# minimum nights
    table(df$minimum_nights)
    df <- df %>%
      mutate(f_minimum_nights= cut(minimum_nights, c(1,2,3,max(df$minimum_nights)+1), labels=c(1,2,3), right = F))
    df$f_minimum_nights <- as.factor(df$f_minimum_nights)
    datasummary( f_minimum_nights*price ~ N + Percent() + Mean + SD, data = df )
      
##################
# Numeric variables
#
    
# number of reviews
  # hist
  ggplot(df) +
    geom_histogram(aes(x=df$number_of_reviews, y = ..density..),binwidth=20, fill="navyblue", color="white") +
    scale_x_continuous(  ) +
    theme_bw()
  df$n_number_of_reviews <- df$number_of_reviews


# create feature: ratio of guests and bathrooms      
  df <- df %>% mutate(n_bathrooms_guest_ratio = case_when(
    gsub("\\ .*","",df$bathrooms_text) == "Half-bath" ~ "1",
    gsub("\\ .*","",df$bathrooms_text) == "Private" ~ "1",
    gsub("\\ .*","",df$bathrooms_text) == "Shared" ~ "1",
    gsub("\\ .*","",df$bathrooms_text) == "" ~ "1",
    gsub("\\ .*","",df$bathrooms_text) == "0" ~ "1",
    TRUE ~ gsub("\\ .*","",df$bathrooms_text)
  ))
  table(df$bathrooms_text)
  df$n_bathrooms_guest_ratio <- as.numeric(df$n_bathrooms_guest_ratio)
  # dividing by guest numbers
  df$n_bathrooms_guest_ratio <- round(df$accommodates / df$n_bathrooms_guest_ratio,2)
  # replace inserted NA with 1
  df <- df %>% mutate(n_bathrooms_guest_ratio = ifelse(is.na(n_bathrooms_guest_ratio), 1, n_bathrooms_guest_ratio))
  
# host since
  # create variable as years of experience
  # extract year 
  df <- df %>% mutate( host_since_year =  as.numeric( year(ymd( host_since ) )))
  
  # check if NAs were introduced
  #view(df[,host_since_year, host_since])
  df <- df %>% mutate(host_since_year = ifelse(is.na(host_since_year), median(df$host_since_year, na.rm = TRUE), host_since_year))
  #view(df[,host_since_year, host_since])
  
  # Calculate host's years of experience
  df$n_host_years <- 2021-df$host_since_year
  # check results
  datasummary( price*factor(n_host_years) ~ N + Percent() + Mean + SD, data = df ) 
  
# accomodates
  table(df$accommodates)
  datasummary( factor(accommodates)*price ~ N + Percent() + Mean + SD, data = df )
  df$n_accommodates <- df$accommodates

  
# keep columns if contain d_, n_,f_, p_, usd_ and some others
  df_cleaned <- df %>%
    select(id, price, matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"))
  
  to_filter <- sapply(df_cleaned, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]
  

####################################
# add amenities to df
#
  # extracting key words
  list_of_amenities = c()
  for(i in df$amenities){
    for(k in str_extract_all(i, "\\w+")){
        list_of_amenities = c(list_of_amenities, k)
    }
  }
  list_of_amenities

  # checking unique occurrences
  x <- as.data.frame(list_of_amenities) %>%
    filter(str_detect(list_of_amenities, "[:upper:]")) %>% 
    filter(list_of_amenities != "7RE") %>% 
    group_by(list_of_amenities) %>% 
    summarise(n=n()) %>% 
    arrange(-n)
  
  # creating a separate df for amenities
  df_amenities <- as.data.frame(matrix(ncol = nrow(x), nrow = nrow(df)))
  # renaming columns
  colnames(df_amenities) <- x$list_of_amenities
  
  # creating dummy values: check if amenities are in the listing
  for(i in 1:length(df_amenities)){
    df_amenities[i] <- grepl(colnames(df_amenities)[i] , df$amenities)*1
  }
  
  rm(x, list_of_amenities)
  
  # adding to cleaned df
  df <- cbind(df_cleaned, df_amenities)
  
  # checking NAs
  to_filter <- sapply(df, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]
  #view(cbind(df$amenities, df$Zara))

########################
# MODELS with 4 levels of complexity

  # amenities
  amenities_2 <- paste(colnames(df_amenities)[1:20], collapse=" + ")
  amenities_3 <- paste(colnames(df_amenities)[1:100], collapse=" + ")
  amenities_4 <- paste(colnames(df_amenities), collapse=" + ")
  
  
  # factors
  factors_2 <- c("f_neighbourhood_cleansed",
                 "f_property_type")
  
  factors_3 <- c("f_flag_host_response_time",
                 "f_host_response_time",
                 "f_neighbourhood_cleansed",
                 "f_property_type",
                 "f_room_type",
                 "f_bathrooms_private",
                 "f_bedrooms")
  
  factors_4 <- c("f_flag_host_response_time",
                 "f_host_response_time",
                 "f_neighbourhood_cleansed",
                 "f_property_type",
                 "f_room_type",
                 "f_bathrooms_private",
                 "f_bedrooms",
                 "f_beds",
                 "f_minimum_nights" )
  
  dummies_2 <- c()
  
  dummies_3 <- c("d_host_local",
                 "d_host_is_superhost",
                 "d_host_has_profile_pic")
  
  dummies_4 <- c("d_host_local",
                 "d_host_is_superhost",
                 "d_host_has_profile_pic",
                 "d_has_availability")
  
  
  numerics_2 <- c(
    "n_accommodates",
    "n_bathrooms_guest_ratio")
  
  numerics_3 <- c(
    "n_accommodates",
    "n_number_of_reviews",
    "n_bathrooms_guest_ratio",
    "n_host_years" )
  
  numerics_4 <- c(
                "n_flag_review_scores_rating",
                "n_accommodates",
                "n_review_scores_rating",
                "n_flag_reviews_per_month",
                "n_reviews_per_month",
                "n_flag_review_scores_cleanliness",
                "n_review_scores_cleanliness",
                "n_flag_review_scores_accuracy",
                "n_review_scores_accuracy",
                "n_flag_review_scores_communication",
                "n_review_scores_communication",
                "n_flag_review_scores_checkin",
                "n_review_scores_checkin",
                "n_flag_review_scores_location",
                "n_review_scores_location",
                "n_flag_review_scores_value",
                "n_review_scores_value",
                "n_number_of_reviews",
                "n_bathrooms_guest_ratio",
                "n_host_years" )
  
  interactions_4  <- c("f_neighbourhood_cleansed*f_property_type",
                       "f_neighbourhood_cleansed*f_bathrooms_private",
                       "f_neighbourhood_cleansed*f_bedrooms",
                       "f_neighbourhood_cleansed*f_beds",
                       "f_neighbourhood_cleansed*n_accommodates",
                       "n_number_of_reviews*n_accommodates",
                       "n_number_of_reviews*n_review_scores_value",
                       "n_number_of_reviews*n_review_scores_rating")

  

  modellev1 <- " ~ n_accommodates"
  modellev2 <- paste0(" ~ ",paste(c(factors_2,dummies_2,numerics_2, amenities_2), collapse = " + "))
  modellev3 <- paste0(" ~ ",paste(c(factors_3,dummies_3,numerics_3, amenities_3), collapse = " + "))
  modellev4 <- paste0(" ~ ",paste(c(factors_4,dummies_4,numerics_4, amenities_4, interactions_4),collapse = " + "))
  
  ##############
  # OLS and 5-fold OLS
  #
  
  # create a holdout set (20% of observations)
  smp_size <- floor(0.2 * nrow(df))
  
  # Set the random number generator: It will make results reproducable
  set.seed(20180123)
  
  # create ids:
  # 1) seq_len: generate regular sequences
  # 2) sample: select random rows from a table
  holdout_ids <- sample( seq_len( nrow( df ) ) , size = smp_size )
  df$holdout <- 0
  df$holdout[holdout_ids] <- 1
  
  #Hold-out set Set
  data_holdout <- df %>% filter(holdout == 1)
  
  #Working data set
  data_work <- df %>% filter(holdout == 0)
  
  ## K = 5
  k_folds <- 5
  # Define seed value
  seed_val <- 20210117
  
  # Do the iteration
  for ( i in 1:4 ){
    print(paste0( "Estimating model: " ,i ))
    # Get the model name
    model_name <-  paste0("modellev",i)
    model_pretty_name <- paste0("M",i,"")
    # Specify the formula
    yvar <- "price"
    xvars <- eval(parse(text = model_name))
    formula <- formula(paste0(yvar,xvars))
    
    # Estimate model on the whole sample
    model_work_data <- fixest::feols( formula , data = data_work , vcov='hetero' )
    #  and get the summary statistics
    fs  <- fixest::fitstat(model_work_data,c('rmse','r2','bic'))
    BIC <- fs$bic
    r2  <- fs$r2
    rmse_train <- fs$rmse
    ncoeff <- length( model_work_data$coefficients )
    
    # Do the k-fold estimation
    set.seed(seed_val)
    cv_i <- train( formula, data_work, method = "lm", 
                   trControl = trainControl(method = "cv", number = k_folds))
    rmse_test <- mean( cv_i$resample$RMSE )
    
    # Save the results
    model_add <- tibble(Model=model_pretty_name, Coefficients=ncoeff,
                        R_squared=r2, BIC = BIC, 
                        Training_RMSE = rmse_train, Test_RMSE = rmse_test )
    if ( i == 1 ){
      model_results <- model_add
    } else{
      model_results <- rbind( model_results , model_add )
    }
  }
  
  # Check summary table
  model_results
  
  
  # RMSE training vs test graph
  colors = c("Training RMSE"="green","Test RMSE" = "blue")
  ggplot( data = model_results, aes( x = factor( Coefficients ) , group = 1 ) )+
    geom_line(aes( y = Training_RMSE , color = 'Training RMSE'), size = 1 ) +
    geom_line(aes( y = Test_RMSE , color = 'Test RMSE') , size = 1 )+
    labs(y='RMSE',x='Number of coefficients',color = "")+
    scale_color_manual(values = colors)+
    theme_bw()+
    theme(legend.position="top")
  

# 
# LASSO
#
  # take model 8 (and find observations where there is no missing data)
  vars_model_4 <- c("price",modellev4)
  
  # Set lasso tuning parameters:
  # a) basic setup
  train_control <- trainControl( method = "cv", number = k_folds)
  # b) tell the actual lambda (penalty parameter) to use for lasso
  tune_grid     <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))
  # c) create a formula
  formula <- formula(paste0("price ", paste(setdiff(vars_model_4, "price"), collapse = " + ")))
  
  # Run LASSO
  set.seed(seed_val)
  lasso_model <- caret::train(formula,
                              data = data_work,
                              method = "glmnet",
                              preProcess = c("center", "scale"),
                              trControl = train_control,
                              tuneGrid = tune_grid,
                              na.action=na.exclude)
  # Check the output
  lasso_model
  # Penalty parameters
  lasso_model$bestTune
  # Check th optimal lambda parameter
  lasso_model$bestTune$lambda
  # Check the RMSE curve
  plot(lasso_model)
  
  # One can get the coefficients as well
  lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    rename(coefficient = `s1`)  # the column has a name "1", to be renamed
  
  # Check the number of variables which actually has coefficients other than 0
  lasso_coeffs_nz<-lasso_coeffs %>%
    filter(coefficient!=0)
  print(nrow(lasso_coeffs_nz))
  
  # Get the RMSE of the Lasso model 
  #   Note you should compare this to the test RMSE
  lasso_fitstats <- lasso_model$results %>%
    filter(lambda == lasso_model$bestTune$lambda) 
  lasso_fitstats
  # Create an auxilary tibble
  lasso_add <- tibble(Model='LASSO M4', Coefficients=nrow(lasso_coeffs_nz),
                      R_squared=lasso_fitstats$Rsquared, BIC = NA, 
                      Training_RMSE = NA, Test_RMSE = lasso_fitstats$RMSE )
  # Add it to final results
  model_results <- rbind( model_results , lasso_add )
  model_results
  
  
##################
# Random forest

  
  # do 5-fold CV
  train_control <- trainControl(method = "cv",
                                number = 5,
                                verboseIter = FALSE)
  
  # set tuning
  tune_grid <- expand.grid(
    .mtry = c(8),              # select 10 variables
    .splitrule = "variance",   # MSE
    .min.node.size = c(10)     # observation number in one last final terminal node
  )
  
  
  # simpler model for model - using random forest
  set.seed(1234)
  system.time({
    rf_model_1 <- train(
      formula(paste0("price", modellev4)),
      data = data_work,
      method = "ranger",
      trControl = train_control,
      tuneGrid = tune_grid,
      importance = "impurity"   # MSE
    )
  })
  # check results
  rf_model_1
  rf_model_1$finalModel
  
  # rmse
  rf_model_1_p <- predict( rf_model_1 , newdata = data_work )
  rf_model_1_rmse <- RMSE(rf_model_1_p,data_work$price)

  
  # Create an auxilary tibble
  random_forest <- tibble(Model='RF M4', Coefficients=rf_model_1$finalModel$num.independent.variables,
                      R_squared=  rf_model_1$finalModel$r.squared, BIC = NA, 
                      Training_RMSE = NA, Test_RMSE = rf_model_1_rmse )
  # Add it to final results
  model_results <- rbind( model_results , random_forest )
  model_results
  
##############################################x
# evaluating on holdout set
  
  # train on whole working set
  
    # OLS (M4)
    ols_final <- fixest::feols( formula(paste0("price",modellev4)) , data = data_work, vcov = 'hetero' )
  
    # LASSO
    lasso_final <- caret::train(formula(paste0("price",modellev4)),
                              data = data_work,
                              method = "glmnet",
                              preProcess = c("center", "scale"),
                              trControl = trainControl( method = "none"),
                              tuneGrid = expand.grid("alpha" = lasso_fitstats$alpha, "lambda" = lasso_fitstats$lambda),
                              na.action=na.exclude)
    # RF
    rf_final <- rf_model_1
  
  # Make prediction for the hold-out sample with each models
    ols_final_p <- predict( ols_final , newdata = data_holdout )
    lasso_final_p <- predict( lasso_final , newdata = data_holdout )
    rf_final_p <- predict( rf_final , newdata = data_holdout )
    
  # Calculate the RMSE on hold-out sample
    ols_final_rmse <- RMSE(ols_final_p,data_holdout$price)
    lasso_final_rmse <- RMSE(lasso_final_p,data_holdout$price)
    rf_final_rmse <- RMSE(rf_final_p,data_holdout$price)
  
  # Create a table
    sum <- rbind(ols_final_rmse,lasso_final_rmse,rf_final_rmse)
    rownames(sum) <- c('OLS M4','LASSO M4','RF M4')
    colnames(sum) <- c('RMSE on hold-out sample')
    sum
    
    
    #########################################################################################
    # Variable Importance Plots -------------------------------------------------------
    #########################################################################################
    
    # variable importance plot
    # 1) full varimp plot, full
    # 2) varimp plot grouped
    # 3) varimp plot , top 10
    # 4) varimp plot  w copy, top 10
    
    
    rf_model_var_imp <- ranger::importance(rf_final$finalModel)/1000
    rf_model_var_imp_df <-
      data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
      arrange(desc(imp)) %>%
      mutate(imp_percentage = imp/sum(imp))
    
    
    ##############################
    # 1) full varimp plot, above a cutoff
    ##############################
    
    

    ggplot(rf_model_var_imp_df[1:10,],
           aes(x=reorder(varname, imp), y=imp_percentage)) +
      geom_point(color='red', size=1.5) +
      geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=1) +
      ylab("Importance (Percent)") +
      xlab("Variable Name") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_bw() +
      theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
            axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
    
    
    #########################################################################################
    # Partial Dependence Plots -------------------------------------------------------
    #########################################################################################
    
    # 1) Number of accommodates
    pdp_n_acc <- pdp::partial(rf_final, pred.var = "n_accommodates", 
                              pred.grid = distinct_(data_holdout, "n_accommodates"), 
                              train = data_work)
    
    pdp_n_acc %>%
      autoplot( ) +
      geom_point(color='red', size=2) +
      geom_line(color='red', size=1) +
      ylab("Predicted price") +
      xlab("Accommodates (persons)") +
      scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
      theme_bw()

    # 2) Room type
    pdp_n_roomtype <- pdp::partial(rf_final, pred.var = "f_property_type", 
                                   pred.grid = distinct_(data_holdout, "f_property_type"), 
                                   train = data_work)
    pdp_n_roomtype %>%
      autoplot( ) +
      geom_point(color='red', size=4) +
      ylab("Predicted price") +
      xlab("Room type") +
      scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
      theme_bw()
    
    data_holdout_w_prediction <- data_holdout %>%
      mutate(predicted_price = predict(rf_final, newdata = data_holdout))
    
    ggplot(data_holdout_w_prediction, aes(x=price, y=predicted_price)) + 
      geom_point() + 
      scale_x_continuous(limits=c(0,500)) + 
      scale_y_continuous(limits=c(0,500)) 
    
    
    c <- data_holdout_w_prediction %>%
      filter(f_property_type %in% c("Entire condominium (condo)", "Entire loft", "Entirerental unit", "Entire residential home", "Private room in condominium (condo)", "Private room in rental unit")) %>%
      group_by(f_property_type) %>%
      dplyr::summarise(
        rmse = RMSE(predicted_price, price),
        mean_price = mean(price),
        rmse_norm = rmse / mean_price
      )
    c
    