getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('../Data/train_all_feature.RData')

colnames(hp)
options(max.print=2000)

hp <- hp %>% mutate_at(ord_var, as.factor)

summary(lm(SalePrice_log_y ~ . - SalePrice_y - Id, data = hp))

# str(hp)

