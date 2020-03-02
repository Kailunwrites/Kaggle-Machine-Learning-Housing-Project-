# library ####
library(dplyr)
library(ggplot2)
library(glmnet)

# call data ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('../Data/train_all.RData')
load('../Data/test_all_v2.RData')

# remove SalePrice and ID####
# unnecessary for lm
hp_feat <- hp_feat %>% dplyr::select(-SalePrice, -Id)
hp_feat_test <- hp_feat_test %>% dplyr::select(-Id)

# standardized columns ###
# hp_feat_std <- hp_feat %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

#check if response variable is normal
ggplot(data = hp_feat, aes(x=SalePrice_log)) + geom_density()

plot(density(hp_feat$SalePrice_log))

# prepping matricies ####
hp_feat_pred <- model.matrix(SalePrice_log~. , hp_feat)[,-1]
hp_feat_resp <- hp_feat$SalePrice_log

hp_feat_test_pred <- model.matrix(SalePrice_log~. , hp_feat)[,-1]
hp_feat_test_resp <- hp_feat_test$SalePrice_log

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
# building lasso ###
lasso <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                # lambda = best_lambda_lasso,
                lambda = grid, #
                alpha = 1, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
)


# coeffs ###
coef(lasso) 

# visualization the coeffs ##########
plot(lasso)
plot(lasso, xvar = "lambda", label=T)
abline(v=log(best_lambda_lasso), col="black", lwd=3, lty=2)


# predicting/rmse/plots ###
lasso_predicted <- predict(lasso, hp_feat_pred)
ModelMetrics::rmse(hp_feat$SalePrice_log, lasso_predicted)

lasso_residuals <- data.frame("res" = lasso_predicted-hp_feat$SalePrice_log)

ggplot(data = lasso_residuals, aes(x=s0)) + geom_density() +
  xlab('Residuals') + ylab('') +
  ggtitle('Density of Residuals from Lasso') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave(
#   'Density of Residuals from Lasso.png',
#   plot = last_plot(),
#   scale = 1,
#   width = 5,
#   height = 3,
#   units = c('in'),
#   dpi = 300
# )

plot(cv.lasso)
abline(v=log(best_lambda_lasso), col="black", lwd=3, lty=2)

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

plot(cv.ridge)
abline(v=log(best_lambda_ridge), col="black", lwd=3, lty=2)

grid = 10^seq(1, -5, length = 100)

# building ridge ###
ridge <- glmnet(x = hp_feat_pred, 
                y = hp_feat_resp,
                # family = 'gaussian',
                lambda = best_lambda_ridge, #
                # lambda = grid,
                alpha = 0, # 1 = lasso, 0 = ridge
                # standardize.response = TRUE, #standardizing the response variable (SalePrice_log).
                trace.it = 1#show progress bar
                # intercept = TRUE
)

plot(ridge)
plot(ridge, xvar = "lambda", label=T)
abline(v=log(best_lambda_ridge), col="black", lwd=3, lty=2)

# coeffs ###
coef(ridge)

# predicting/rmse ###
ridge_predicted <- predict(ridge, hp_feat_pred)
ModelMetrics::rmse(hp_feat$SalePrice_log, ridge_predicted)

ridge_residuals <- data.frame("res" = ridge_predicted-hp_feat$SalePrice_log)

ggplot(data = ridge_residuals, aes(x=s0)) + geom_density() +
  xlab('Residuals') + ylab('') +
  ggtitle('Density of Residuals from Ridge') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave(
#   'Density of Residuals from Ridge.png',
#   plot = last_plot(),
#   scale = 1,
#   width = 5,
#   height = 3,
#   units = c('in'),
#   dpi = 300
# )
