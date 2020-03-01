load('train_all.RData')
original_data = read.csv2('train.csv', header=T, sep = ',')

impute_LotFrontage = original_data$LotFrontage
for (neigh in original_data$Neighborhood){
  idx = which(is.na(original_data$LotFrontage) & (original_data$Neighborhood==neigh))
  impute_LotFrontage[idx] =  mean(original_data$LotFrontage[original_data$Neighborhood==neigh], na.rm=T)
}

hp_feat$LotFrontage = impute_LotFrontage
hp_orig$LotFrontage = impute_LotFrontage

save(hp_feat, hp_orig, file = "train_all_v2.RData")


