# library ####
library(dplyr)
library(ggplot2)
library(glmnet)

# call data ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('../Data/train_all.RData')

# remove SalePrice and ID####
# unnecessary for lm
hp_feat <- hp_feat %>% select(-SalePrice, -Id)

# standardized columns ###
# hp_feat_std <- hp_feat %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

#check if response variable is normal
ggplot(data = hp_feat, aes(x=SalePrice_log)) + geom_density()

plot(density(hp_feat$SalePrice_log))

# prepping matricies ####
hp_feat_pred <- model.matrix(SalePrice_log~. , hp_feat)[,-1]
hp_feat_resp <- hp_feat$SalePrice_log

# lasso ####

# finding lambda for lasso ####
# find lambda thru cross validation glmnet
cv.lasso <- cv.glmnet(x = hp_feat_pred, 
                      y = hp_feat_resp,
                      type.measure = 'mse',
                      alpha = 1 # 1 = lasso, 0 = ridge
                      # standardize.response = FALSE, #standardizing the response variable (Saleprice_log).
                      # trace.it = 1, #show progress bar
                      # intercept = TRUE
                      )

best_lambda_lasso <- cv.lasso$lambda.min

# building lasso ####
lasso <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                lambda = best_lambda_lasso, #
                alpha = 1, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
                )
# coeffs
coef(lasso) 

# ridge ####

# finding lambda for ridge ####
# find lambda thru cross validation glmnet
cv.ridge <- cv.glmnet(x = hp_feat_pred, 
                      y = hp_feat_resp,
                      type.measure = 'mse',
                      alpha = 0 # 1 = lasso, 0 = ridge
                      # standardize.response = FALSE, #standardizing the response variable (Saleprice_log).
                      # trace.it = 1, #show progress bar
                      # intercept = TRUE
)

best_lambda_ridge <- cv.ridge$lambda.min

# building ridge ####
ridge <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                lambda = best_lambda_ridge, #
                alpha = 0, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
)
# coeffs
coef(ridge)



