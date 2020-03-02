# library ####
library(dplyr)
library(MASS)
library(ggplot2)

# call data####
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('../Data/train_all.RData')

options(max.print=2000)

# str(hp_feat)
# colnames(hp_feat)

hp_feat <- hp_feat %>% dplyr::select(.,-Id, -SalePrice)

# baseline model####
baselinemodel <- lm(data = hp_feat, formula = SalePrice_log ~ .)
summary(baselinemodel)


model.empty = lm(SalePrice_log ~ 1, data = hp_feat) #The model with an intercept ONLY.
model.full = baselinemodel #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
# bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
# bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
summary(backwardAIC)
summary(bothAIC.empty) #same with forwardAIC
summary(bothAIC.full) #same with backwardAIC

extractAIC(forwardAIC)
extractAIC(backwardAIC)

# prepping matrix
hp_feat_pred <- model.matrix(SalePrice_log~. , hp_feat)[,-1]

# predicting/rmse
forwardAIC_predicted <- predict(forwardAIC, hp_feat)
ModelMetrics::rmse(hp_feat$SalePrice_log, forwardAIC_predicted)

backwardAIC_predicted <- predict(backwardAIC, hp_feat)
ModelMetrics::rmse(hp_feat$SalePrice_log, backwardAIC_predicted)


forwardAIC_residuals <- data.frame('res' = forwardAIC_predicted-hp_feat$SalePrice_log)
backwardAIC_residuals <- data.frame('res' = backwardAIC_predicted-hp_feat$SalePrice_log)

ggplot(data = forwardAIC_residuals, aes(x=res)) + geom_density()+
  xlab('Residuals') + ylab('') +
  ggtitle('Density of Residuals from forward AIC') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave(
#   'Density of Residuals from forward AIC.png',
#   plot = last_plot(),
#   scale = 1,
#   width = 5,
#   height = 3,
#   units = c('in'),
#   dpi = 300
# )

ggplot(data = backwardAIC_residuals, aes(x=res)) + geom_density()+
  xlab('Residuals') + ylab('') +
  ggtitle('Density of Residuals from backward AIC') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave(
#   'Density of Residuals from backward AIC.png',
#   plot = last_plot(),
#   scale = 1,
#   width = 5,
#   height = 3,
#   units = c('in'),
#   dpi = 300
# )

# residual scatterplots ####
resid.forward <- resid(forwardAIC)
resid.backward <- resid(backwardAIC)

plot(hp_feat$SalePrice_log, resid.forward,
     ylab="Residuals", xlab="log(SalePrice)", 
     main="Forward AIC Residuals")
abline(0, 0)

plot(hp_feat$SalePrice_log, resid.backward,
     ylab="Residuals", xlab="log(SalePrice)", 
     main="Backward AIC Residuals")
abline(0, 0)


