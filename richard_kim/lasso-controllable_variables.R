# library ####
library(dplyr)
library(ggplot2)
library(glmnet)

# call data ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('../Data/train_all.RData')
# load('../Data/test_all.RData')
load('../Data/test_all_v2.RData')

# remove SalePrice and ID####
# unnecessary for lm
hp_feat <- hp_feat %>% select(-SalePrice, -Id)

# select only the variables that houseowners can flip.
hp_feat_flip <- hp_feat %>% 
  select(OverallQual, OverallCond, MasVnrArea, ExterQual, BedroomAbvGr, KitchenAbvGr, 
         Fireplaces, WoodDeckSF, TotBaths, HasPool, HasDeck, HasPorch, HasStoneMas, 
         KitchenQual_Fa, KitchenQual_Gd, KitchenQual_TA, 
         BsmtCond_Gd, BsmtCond_None, BsmtCond_Po, BsmtCond_TA,
         HeatingQC_Fa, HeatingQC_Gd, HeatingQC_Po, HeatingQC_TA, 
         GarageType_Attchd, GarageType_Basment, GarageType_BuiltIn, GarageType_CarPort,
         GarageType_Detchd, GarageType_None,
         GarageFinish_None, GarageFinish_RFn, GarageFinish_Unf,
         SalePrice_log)

# prepping matricies ####
hp_feat_flip_pred <- model.matrix(SalePrice_log~. , hp_feat_flip)[,-1]
hp_feat_flip_resp <- hp_feat_flip$SalePrice_log

# lasso ####

# finding lambda for lasso ####
# find lambda thru cross validation glmnet
cv.lasso_flip <- cv.glmnet(x = hp_feat_flip_pred, 
                      y = hp_feat_flip_resp,
                      type.measure = 'mse',
                      alpha = 1 # 1 = lasso, 0 = ridge
                      # standardize.response = FALSE, #standardizing the response variable (Saleprice_log).
                      # trace.it = 1, #show progress bar
                      # intercept = TRUE
)

best_lambda_lasso_flip <- cv.lasso_flip$lambda.min

# building lasso ####
lasso_flip <- glmnet(x = hp_feat_flip_pred, 
                y = hp_feat_flip_resp,
                # family = 'gaussian',
                lambda = best_lambda_lasso_flip, #
                alpha = 1, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
)
# coeffs
coef(lasso_flip) 

# predicting/rmse
lasso_flip_predicted <- predict(lasso_flip, hp_feat_flip_pred)
ModelMetrics::rmse(hp_feat$SalePrice_log, lasso_flip_predicted)

