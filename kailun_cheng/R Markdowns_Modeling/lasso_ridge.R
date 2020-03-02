# library ####
library(dplyr)
library(ggplot2)
library(glmnet)

# call data ####
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('train_all.RData')
#load('../Data/test_all.RData')

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

# finding lambda for lasso ###
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

grid = 10^seq(1, -5, length = 100)
# building lasso for visualization first doing grid ###
lasso <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                lambda = grid, #
                alpha = 1, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
                )
# coeffs ###
coef(lasso) 

# visualization the coeffs ##########
plot(cv.lasso)
plot(lasso, xvar = "lambda", label=T)
abline(v=log(best_lambda_lasso), col="black", lwd=3, lty=2)
#axis(1, at=log(best_lambda_lasso),labels=format(round(log(best_lambda_lasso),1), nsmall=1))

# predicting/rmse ###
lasso_refit <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                lambda = best_lambda_lasso, #
                alpha = 1, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
)

lasso_predicted <- predict(lasso_refit, hp_feat_pred)
ModelMetrics::rmse(hp_feat$SalePrice_log, lasso_predicted)

# ridge ####

# finding lambda for ridge ###
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
grid2 = 10^seq(2, -5, length = 100)

# building ridge for visualization over the grid2 ###
ridge <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                lambda = grid2, #
                alpha = 0, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
)

# coeffs ###
coef(ridge)

# visualization the coeffs ##########
plot(cv.ridge)
plot(ridge, xvar = "lambda", label=T, main = "Ridge Regression Decreasing Coeffcients")
abline(v=log(best_lambda_ridge), col="black", lwd=3, lty=2)


# predicting/rmse ###
ridge_refit <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                lambda = best_lambda_ridge, #
                alpha = 0, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
)

ridge_predicted <- predict(ridge_refit, hp_feat_pred)
ModelMetrics::rmse(hp_feat$SalePrice_log, ridge_predicted)


