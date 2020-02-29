# Note on Vocab for transforming a column:
#   Normalize: pull the mean to zero 
#   Standardize: pull the mean to zero and set sd to one (get the z-score)

# library ####
library(dplyr)

# call data ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('../Data/train_all.RData')

# remove SalePrice and ID####
# unnecessary for lm
hp_feat <- hp_feat %>% select(-SalePrice, -Id)

# standardized columns ####
hp_feat_std <- hp_feat %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

# baseline model####
baselinemodel <- lm(data = hp_feat_std, formula = SalePrice_log ~ .)
summary(baselinemodel)

# setup empty/full models ####
model.empty = lm(SalePrice_log ~ 1, data = hp_feat) #The model with an intercept ONLY.
model.full = baselinemodel #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
summary(backwardAIC)
summary(bothAIC.empty) #same with forwardAIC
summary(bothAIC.full) #same with backwardAIC

extractAIC(forwardAIC)
extractAIC(backwardAIC)

