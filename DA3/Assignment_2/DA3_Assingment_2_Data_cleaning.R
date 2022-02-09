############################################
# Setting the scene
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


############################################
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
  df <- df[price<400]
  #getting histogram
  plot_price_hist <- ggplot(df) +
    geom_histogram(aes(x=price, y = ..density..), binwidth = 10, fill="navyblue", color="white") +
    scale_x_continuous( limits = c( 0 , 400 ) ) +
    labs( x = "Hourly earnings in USD" , y = 'Relative Frequency', fill = "female" ) +
    geom_segment( aes(x = mean( df$price , na.rm = T ), y = 0, xend = mean( df$price , na.rm = T ), yend = 0.016) , color = 'red', size = 0.5 ) + 
    annotate( "text" , size=3, x = mean( df$price , na.rm = T )+20 , y = 0.018 , label = 'Mean' , color = 'red') +
    annotate( "text" , size=3, x = mean( df$price , na.rm = T )+20 , y = 0.017 , label = round(mean( df$price , na.rm = T ),2) , color = 'red') +
    geom_segment( aes(x = median( df$price , na.rm = T ), y = 0, xend = median( df$price , na.rm = T ), yend = 0.016) , color = 'orange', size = 0.5 ) + 
    annotate( "text" , size=3, x = median( df$price , na.rm = T )-20 , y = 0.018 , label = 'Median' , color = 'orange') +
    annotate( "text" , size=3, x = median( df$price , na.rm = T )-20 , y = 0.017 , label = round(median( df$price , na.rm = T ),2) , color = 'orange') +  theme_bw()
  
  png('plot_price_hist.png', width=400, height=300)
  plot(plot_price_hist)
  dev.off()
  
############################################
# Dealing with NAs
#
  
  
# where do we have missing variables?
  to_filter <- sapply(df, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]


# Only a few NA in numeric:: replace with mode
  df <- df %>% mutate(
    host_since =  ifelse(is.na(host_since), mode(host_since), host_since), 
    host_location = ifelse(is.na(host_location),  mode(host_location), host_location),
    host_is_superhost = ifelse(is.na(host_is_superhost),  mode(host_is_superhost), host_is_superhost),
    host_has_profile_pic = ifelse(is.na(host_has_profile_pic),  mode(host_has_profile_pic), host_has_profile_pic),
    bathrooms_text = ifelse(is.na(bathrooms_text), mode(bathrooms_text), bathrooms_text),
    host_total_listings_count = ifelse(is.na(host_total_listings_count), mode(host_total_listings_count), host_total_listings_count)
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
  
############################################
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
  
############################################
#
# Factors
#
#
    
# neighborhood_cleansed
    # check values <- drop missing (only 16)
    table(df$neighbourhood_cleansed)
    df$f_neighbourhood_cleansed <- as.factor(df$neighbourhood_cleansed)
    datasummary( neighbourhood_cleansed*price ~ N + Percent() + Mean + Median + SD, data = df )
    
    table_descriptives_on_district <- df[, .(neighbourhood_cleansed, price)]
    write_csv(table_descriptives_on_district, file="table_descriptives_on_district.csv")
    
    
    
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
    labs(x = "Room type",y = "Price (EUR)") +
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
      
########################################
# Numeric variables
#

# listings count
    table(df$host_total_listings_count)
    df$n_host_total_listings <- as.numeric(df$host_total_listings_count)
    sum(is.na(df$n_host_total_listings))
    df <- df %>% mutate(
      n_host_total_listings = ifelse(is.na(n_host_total_listings), mean(n_host_total_listings, na.rm=TRUE), n_host_total_listings)
    )
    sum(is.na(df$n_host_total_listings))
    typeof(df$n_host_total_listings)
    df$n_host_total_listings <- as.numeric(df$n_host_total_listings)
    sum(is.na(df$n_host_total_listings))
        
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
# Amenities
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
  
  # adding to cleaned df
  data <- cbind(df_cleaned, df_amenities)
  
  # checking NAs
  to_filter <- sapply(data, function(x) sum(is.na(x)))
  to_filter[to_filter > 0]
  #view(cbind(df$amenities, df$Zara))
 
  write_csv(data, "data_cleaned.csv")
  write_csv(df_amenities, "data_amenities.csv")
  