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


data <- read_csv("https://raw.githubusercontent.com/DaniDataScience/DA_3/main/DA3/Assignment_2/data_cleaned.csv")
df_amenities <- read_csv("https://raw.githubusercontent.com/DaniDataScience/DA_3/main/DA3/Assignment_2/data_amenities.csv")
 
##############################################
# MODELS with levels of complexity
#
  
  # amenities
  amenities_2 <- paste(colnames(df_amenities)[1:10], collapse=" + ")
  amenities_3 <- paste(colnames(df_amenities)[1:50], collapse=" + ")
  amenities_4 <- paste(colnames(df_amenities), collapse=" + ")
  
  
  # factors
  property <- c("f_neighbourhood_cleansed",
                "f_property_type",
                "f_room_type"
                 )
  
  size <- c("f_bathrooms_private",
            "f_bedrooms",
            "f_beds",
            "n_accommodates",
            "n_bathrooms_guest_ratio")
  
  host <- c(
            "d_host_local",
            "d_host_is_superhost",
            "d_host_has_profile_pic",
            "n_host_years", 
            "n_host_total_listings")
  
  booking <- c("f_minimum_nights",
               "d_has_availability",
               "f_flag_host_response_time",
               "f_host_response_time")

  reviews <- c(
                "n_flag_review_scores_rating",
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
                "n_number_of_reviews" )
  
  interactions  <- c("f_neighbourhood_cleansed*n_accommodates",
                       "n_accommodates*f_property_type",
                       "f_property_type*f_room_type",
                       "n_accommodates*n_bathrooms_guest_ratio",
                       "n_number_of_reviews*n_accommodates",
                       "n_number_of_reviews*n_review_scores_value",
                       "n_number_of_reviews*n_review_scores_rating")


  modellev1 <- " ~ n_accommodates"
  modellev2 <- paste0(" ~ ",paste(c(size, property ), collapse = " + "))
  modellev3 <- paste0(" ~ ",paste(c(size, property, reviews, amenities_2 ), collapse = " + "))
  modellev4 <- paste0(" ~ ",paste(c(size, property, reviews, host, booking, amenities_3, interactions), collapse = " + "))
  modellev5 <- paste0(" ~ ",paste(c(size, property, reviews, host, booking, amenities_4, interactions),collapse = " + "))
 
  
#################################
# Manage different samples:
#
  
  #######
  # A) Create a separate hold-out set

  # create a holdout set (20% of observations)
  smp_size <- floor(0.2 * nrow(data))
  
  # Set the random number generator: It will make results reproducable
  set.seed(20180123)
  
  # create ids:
  # 1) seq_len: generate regular sequences
  # 2) sample: select random rows from a table
  holdout_ids <- sample( seq_len( nrow( data ) ) , size = smp_size )
  data$holdout <- 0
  data$holdout[holdout_ids] <- 1
  
  #Hold-out set Set
  data_holdout <- data %>% filter(holdout == 1)
  #Working data set
  data_work <- data %>% filter(holdout == 0)
  
####################################
# OLS
#   a) estimate measures on the whole working sample (R2,BIC,RMSE)
#   b) DO K-fold cross validation to get proper Test RMSE
  
  ## K = 5
  k_folds <- 5
  # Define seed value
  seed_val <- 20210117
    # Do the iteration
  for ( i in 1:5 ){
    print(paste0( "Estimating model: " ,i ))
    # Get the model name
    model_name <-  paste0("modellev",i)
    model_pretty_name <- paste0("OLS M",i,"")
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
  plot_trainings_vs_test_rmse <- 
    ggplot( data = model_results, aes( x = factor( Coefficients ) , group = 1 ) )+
    geom_line(aes( y = Training_RMSE , color = 'Training RMSE'), size = 1 ) +
    geom_line(aes( y = Test_RMSE , color = 'Test RMSE') , size = 1 )+
    labs(y='RMSE',x='Number of coefficients',color = "")+
    scale_color_manual(values = colors)+
    theme_bw()+
    theme(legend.position="top")
  
  png('plot_trainings_vs_test_rmse.png', width=400, height=300)
  plot(plot_trainings_vs_test_rmse)
  dev.off()
  
  # Model 5 is overfitted, good input for lasso
  # Model 4 should be the final OLS model
  formula <- formula(paste0("price",eval(parse(text = paste0("modellev",4)))))
  OLS_M4 <- train( formula, data_work, method = "lm", 
                   trControl = trainControl(method = "cv", number = k_folds))
  rmse_test <- mean( OLS_M4$resample$RMSE ) 
  rmse_test

#################################
# LASSO
#
  
  # Set lasso tuning parameters:
  # a) basic setup
  train_control <- trainControl( method = "cv", number = k_folds)
  # b) tell the actual lambda (penalty parameter) to use for lasso
  tune_grid     <- expand.grid("alpha" = c(1), "lambda" = seq(0.02, 1, by = 0.02))
  # c) create a formula
  formula <- formula(paste0("price", (setdiff(modellev5, "price"))))
  
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
  plot_lasso <- plot(lasso_model)
  png('plot_lasso.png', width=400, height=300)
  plot(plot_lasso)
  dev.off()
  
  
  # One can get the coefficients as well
  lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    rename(coefficient = `s1`)  # the column has a name "1", to be renamed
  
  print(lasso_coeffs)
  
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
  lasso_add <- tibble(Model='LASSO M5', Coefficients=nrow(lasso_coeffs_nz),
                      R_squared=lasso_fitstats$Rsquared, BIC = NA, 
                      Training_RMSE = NA, Test_RMSE = lasso_fitstats$RMSE )
  # Add it to final results
  model_results <- rbind( model_results , lasso_add )
  ols_lasso_model_results <- model_results
  write_csv(ols_lasso_model_results, file="exp_ols_lasso_model_results.csv")
  
#################################
# Use of random forest  
#  
  # set tuning 
  tune_grid <- expand.grid(
    .mtry = c(6, 8, 10),
    .splitrule = "variance",
    .min.node.size = c(5, 10, 15)
  )
  
  # based on model 4
  set.seed(111)
  system.time({
    rf_model_4 <- train(
      formula <- formula(paste0("price", (setdiff(modellev4, "price")))),
      data = data_work,
      method = "ranger",
      trControl = train_control,
      tuneGrid = tune_grid,
      importance = "impurity",
      .num.trees=500
    )
  })
  
rf_model_4
 
#################################
# Gradient boosting  
#
  gbm_grid <-  expand.grid(interaction.depth = 5, # complexity of the tree
                           n.trees = 250, # number of iterations, i.e. trees
                           shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                           n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
  )
  
  
  set.seed(1234)
  system.time({
    gbm_model <- train(formula(paste0("price", (setdiff(modellev4, "price")))),
                       data = data_work,
                       method = "gbm",
                       trControl = train_control,
                       verbose = FALSE,
                       tuneGrid = gbm_grid)
  })
  gbm_model
  
  
###########################################
#  Compare models
#
  
  final_models <-
    list("OLS M4" = OLS_M4,
         "LASSO M5" = lasso_model,
         "RF M4" = rf_model_4,
         "GBM"  = gbm_model)
  
  result_resample <- resamples(final_models) %>% summary()
  result_resample
  
  # Model selection is carried out on this CV RMSE
  result_CV <- imap(final_models, ~{
    mean(result_resample$values[[paste0(.y,"~RMSE")]])
  }) %>% unlist() %>% as.data.frame() %>%
    rename("CV RMSE" = ".")
  
  result_CV
 
  # evaluate preferred model on the holdout set -----------------------------
  result_holdout <- map(final_models, ~{
    RMSE(predict(.x, newdata = data_holdout), data_holdout[["price"]])
  }) %>% unlist() %>% as.data.frame() %>%
    rename("Holdout RMSE" = ".")
  result_holdout
  
  
  result <- cbind("Model" = c("OLS M4", "LASSO M5","RF M4", "GBM M4"), result_CV, result_holdout)
  result
  write_csv(result, file="exp_result_holdout.csv")
  
  
  #########################################################################################
  # Partial Dependence Plots for the best model; random forest with specified tuning parameters
  #########################################################################################
  # 1) Property Type
  pdp_f_property_type <- pdp::partial(gbm_model, pred.var = "f_property_type", 
                                      pred.grid = distinct_(data_holdout, "f_property_type"), 
                                      train = data_work)
  plot_pdp_property <- pdp_f_property_type %>%
    ggplot( aes(y=yhat, x=reorder(f_property_type,-yhat))) +
    geom_point(color='red', size=2) +
    ylab("Predicted price") +
    xlab("Number of accomodates") +
    scale_y_continuous(limits=c(70,110)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.grid.major = element_line(colour = "light grey"))
  
  png('plot_pdp_property.png', width=400, height=300)
  plot(plot_pdp_property)
  dev.off()

  # 2) Accomodates
  pdp_n_accommodates <- pdp::partial(gbm_model, pred.var = "n_accommodates", 
                                      pred.grid = distinct_(data_holdout, "n_accommodates"), 
                                      train = data_work)
  plot_pdp_n_accommodates <- pdp_n_accommodates %>%
    autoplot( ) +
    geom_point(color='red', size=2) +
    geom_line(color='red', size=1) +
    ylab("Predicted price") +
    xlab("Number of accomodates") +
    theme_bw()
  
  png('plot_pdp_n_accommodates.png', width=400, height=300)
  plot(plot_pdp_n_accommodates)
  dev.off()
  
 
  
  # 3) f_neighbourhood_cleansed
  pdp_f_neighbourhood_cleansed <- pdp::partial(gbm_model, pred.var = "f_neighbourhood_cleansed", 
                                     pred.grid = distinct_(data_holdout, "f_neighbourhood_cleansed"), 
                                     train = data_work)
  
  pdp_f_neighbourhood_cleansed <- data.table(pdp_f_neighbourhood_cleansed) %>% dplyr::arrange(-yhat)
  
  plot_pdp_f_neighbourhood_cleansed <- pdp_f_neighbourhood_cleansed %>%
    ggplot( aes(y=yhat, x=reorder(f_neighbourhood_cleansed,-yhat))) +
    geom_point(color='red', size=2) +
    ylab("Predicted price") +
    xlab("Number of accomodates") +
    scale_y_continuous(limits=c(70,100)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.grid.major = element_line(colour = "light grey"))
  
  png('plot_pdp_f_neighbourhood_cleansed.png', width=400, height=300)
  plot(plot_pdp_f_neighbourhood_cleansed)
  dev.off()
  
  
  #######################
  #Partial importance plot
  
  ##Variable Importance Plots rf_model
  rf_model_var_imp <- ranger::importance(rf_model_4$finalModel)/1000
  rf_model_var_imp_df <-
    data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
    mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
    mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
    arrange(desc(imp)) %>%
    mutate(imp_percentage = imp/sum(imp))
  rf_model_var_imp_df
  
  # have a version with top 10 vars only
  imp_top10  <- ggplot(rf_model_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color='red', size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.75) +
    ylab("Importance (Percent)") +
    xlab("Variable Name") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bw()
  
  png('imp_top10.png', width=800, height=600)
  plot(imp_top10)
  dev.off()

  # 2) varimp plot grouped

  varnames <- rf_model_4$finalModel$xNames

  groups <- list(size=size, property=property, property=property, host=host, booking=booking, amenities=amenities)
  
  # Need a function to calculate grouped varimp
  group.importance <- function(rf.obj, groups) {
    var.imp <- as.matrix(sapply(groups, function(g) {
      sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
    }))
    colnames(var.imp) <- "MeanDecreaseGini"
    return(var.imp)
  }
  rf_model_var_imp_grouped <- group.importance(rf_model_4$finalModel, groups)
  rf_model_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_var_imp_grouped),
                                            imp = rf_model_var_imp_grouped[,1])  %>%
    mutate(imp_percentage = imp/sum(imp))
  imp_grouped <- ggplot(rf_model_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color='red', size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.7) +
    ylab("Importance (Percent)") +   xlab("Variable Name") +
    coord_flip() +
    # expand=c(0,0),
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bw()
  
  png('imp_grouped.png', width=400, height=600)
  plot(imp_grouped)
  dev.off()
  
  # 3) zoom in amenities
  
  varnames <- rf_model_4$finalModel$xNames

  amenities <- grep("^[^n_]", varnames, value = TRUE)
  amenities <- grep("^[^f_]", amenities, value = TRUE)
  amenities <- grep("^[^d_]", amenities, value = TRUE)
  groups <- unlist(amenities)
  
  # Need a function to calculate grouped varimp
  group.importance <- function(rf.obj, groups) {
    var.imp <- as.matrix(sapply(groups, function(g) {
      sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
    }))
    colnames(var.imp) <- "MeanDecreaseGini"
    return(var.imp)
  }
  rf_model_var_imp_grouped <- group.importance(rf_model_4$finalModel, groups)
  rf_model_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_var_imp_grouped),
                                            imp = rf_model_var_imp_grouped[,1])  %>%
    mutate(imp_percentage = imp/sum(imp))
  imp_amen <- ggplot(rf_model_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color='red', size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.7) +
    ylab("Importance (Percent)") +   xlab("Variable Name") +
    coord_flip() +
    # expand=c(0,0),
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bw()
  
  png('imp_amen.png', width=400, height=800)
  plot(imp_amen)
  dev.off()
  
  