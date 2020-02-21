#library ####
library(dplyr)
library(ggplot2)
library(tidyr)
# library(tidyimpute)

#read csv ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
hp <- read.csv2('../Data/train.csv', sep = ',')
hp2 <- read.csv2('../Data/train.csv', sep = ',')

# ID ####
#remove Id column
hp <- select(hp, -Id)

# MSSubClass ####
#Categorizing MSSubClass
hp$MSSubClass <- as.factor(hp$MSSubClass)

# MSZoning ####
#renaming MSZoning level 'C (all)' to 'C'

hp <- hp %>% mutate(MSZoning = recode_factor(MSZoning, 'C (all)' = 'C'))

# LotFrontage ####
#histogram of LotFrontage
ggplot(data = hp, aes(x=LotFrontage)) + geom_histogram()
sum(is.na(hp$LotFrontage)) #259 NA's

#imputing with zeroes
hp <- hp %>% replace_na(list(LotFrontage = 0))


# LotArea ####
#histogram of LotArea
ggplot(data = hp, aes(x=LotArea)) + geom_histogram()
sum(is.na(hp$LotArea)) #0 NA's


# Street ####
#nothing

hp$Street %>% levels()
table(hp$Street)
sum(is.na(hp$Street)) #0 NA's

# Alley ####
#NA's mean 'No Alley Access'

sum(is.na(hp$Alley)) #1369

#might as well recode it to 'No Alley'

hp$Alley = factor(hp$Alley, levels = c(levels(hp$Alley), 'No Alley'))
hp$Alley[is.na(hp$Alley)] = 'No Alley'

sum(is.na(hp$Alley)) #0

# LotShape ####
#converting to 1-4

hp <- hp %>% mutate(LotShape_mod = recode_factor(LotShape, 
                                                 'Reg' = 1, 
                                                 'IR1' = 2, 
                                                 'IR2' = 3, 
                                                 'IR3' = 4))
hp$LotShape_mod <- as.numeric(hp$LotShape_mod)

# mean(hp$LotShape_mod)

# LandContour ####
#nothing

# hp$LandContour %>% levels()
# sum(is.na(hp$LandContour)) #0

# Utilities ####
#nothing

# hp$Utilities %>% levels()
# table(hp$Utilities)
# sum(is.na(hp$Utilities)) #0

# LotConfig ####
#nothing

# hp$LotConfig %>% levels()
# table(hp$LotConfig)
# sum(is.na(hp$LotConfig)) #0

# LandSlope ####
# converting to 1-3

hp <- hp %>% mutate(LandSlope_mod = recode_factor(LandSlope, 
                                                  'Gtl' = 1, 
                                                  'Mod' = 2, 
                                                  'Sev' = 3
                                                  ))
hp$LandSlope_mod <- as.numeric(hp$LandSlope_mod)

# ggplot(data = hp, aes(x = LandSlope_mod, y = SalePrice)) + geom_point()

# Neighborhood ####
#nothing

# hp$Neighborhood %>% levels()
# sum(is.na(hp$Neighborhood)) #0

# Condition1,2 ####
hp$Condition1 %>% levels()
hp$Condition2 %>% levels()

#creating new column num_of_cond to count how many conditions exist

hp <- hp %>% mutate(num_of_cond = ifelse(Condition1 == 'Norm', 0,
                                         ifelse(Condition2 == 'Norm', 1, 2)))

# upon inspection, wherever num_of_cond == 2, Condition 1 and 2 happens to be the same
# recode it s.t. it accounts for that?

hp <- hp %>% mutate(num_of_cond = ifelse(Condition1 == 'Norm', 0,
                                         ifelse(Condition2 == 'Norm' || Condition2 == Condition1, 1, 2)))

# note: there are no rows that have more than 1 condition(?)

# BldgType ####
#convert 1-5
hp$BldgType %>% levels()
# ggplot(data = hp, aes(x= BldgType, y = SalePrice)) + geom_point()

hp <- hp %>% mutate(BldgType_mod = recode_factor(BldgType, 
                                                 '2fmCon' = 1,
                                                 'Duplex' = 2,
                                                 'Twnhs' = 3,
                                                 'TwnhsE' = 4,
                                                 '1Fam' = 5))
hp$BldgType_mod <- as.numeric(hp$BldgType_mod)

# mean(hp$BldgType_mod)

# ggplot(data = hp, aes(x= BldgType, y = SalePrice)) + geom_point()

# ggplot(data = hp, aes(x= BldgType_mod, y = SalePrice)) + geom_point()

# HouseStyle ####
#ordinal.....?

# hp$HouseStyle %>% levels()
# sum(is.na(hp$HouseStyle))
# table(hp$HouseStyle)

# ggplot(data = hp, aes(x = HouseStyle, y = SalePrice)) + geom_point()

# OverallQual ####
#nothing

# ggplot(data = hp, aes(x = OverallQual, y = SalePrice)) + geom_point()

# OverallCond ####
#nothing

# ggplot(data = hp, aes(x = OverallCond, y = SalePrice)) + geom_point()


# YearBuilt ####
#nothing

# ggplot(data = hp, aes(x = YearBuilt, y = SalePrice)) + geom_point()

# YearRemodAdd ####
#nothing

# ggplot(data = hp, aes(x = YearRemodAdd, y = SalePrice)) + geom_point()

#maybe add a boolean column if it were remodeled or not?
#boolean? 0 and 1?
#might not be useful

hp <- hp %>% mutate(isRemodeled = (YearBuilt != YearRemodAdd))

# ggplot(data = hp, aes(x = isRemodeled, y = SalePrice)) + geom_point()

# summary(lm(log(SalePrice) ~ isRemodeled, data = hp))
#negative beta, wtf


# RoofStyle ####
#nothing

# hp$RoofStyle %>% levels()
# table(hp$RoofStyle)
# sum(is.na(hp$RoofStyle)) #0

# RoofMatl ####

# hp$RoofMatl %>% levels()
# table(hp$RoofMatl) #dat distribution
# sum(is.na(hp$RoofMatl)) #0

# ggplot(data = hp, aes(x = RoofMatl, y = RoofStyle)) + geom_point()

#there seems to be no correlation b/w RoofMatl and RoofStyle? 
#most of them are CompShg/Gable anyway to have meaningful impact

# Exterior1st, Exterior2nd, ExterQual, ExterCond####

#fixing Exterior2nd
hp <- hp %>% mutate(Exterior2nd = recode_factor(Exterior2nd, 
                                                 'Brk Cmn' = 'BrkComm',
                                                 'CmentBd' = 'CemntBd',
                                                 'Wd Shng' = 'WdShing'))

#making num_of_ext

hp$Exterior1st <- as.character(hp$Exterior1st)
hp$Exterior2nd <- as.character(hp$Exterior2nd)

hp <- hp %>% mutate(num_of_ext = ifelse(Exterior1st == Exterior2nd, 1, 2))

hp$Exterior1st <- as.factor(hp$Exterior1st)
hp$Exterior2nd <- as.factor(hp$Exterior2nd)

#recoding ExterQualmod as ordinal
hp <- hp %>% mutate(ExterQual_mod = recode_factor(ExterQual,
                                                  'Po' = 1,
                                                  'Fa' = 2,
                                                  'TA' = 3,
                                                  'Gd' = 4,
                                                  'Ex' = 5
                                                  ))
hp$ExterQual_mod <- as.numeric(hp$ExterQual_mod)

#recoding ExterCond as ordinal
hp <- hp %>% mutate(ExterCond_mod = recode_factor(ExterCond,
                                                  'Po' = 1,
                                                  'Fa' = 2,
                                                  'TA' = 3,
                                                  'Gd' = 4,
                                                  'Ex' = 5
                                                  ))
hp$ExterCond_mod <- as.numeric(hp$ExterCond_mod)

# MasVnrType ####
hp$MasVnrType %>% levels()
sum(is.na(hp$MasVnrType)) #8

#impute 'none' for those NA's
hp$MasVnrType[is.na(hp$MasVnrType)] = 'None'

# MasVnrArea ####
#impute NA's with zero
hp <- hp %>% replace_na(list(MasVnrArea = 0))

ggplot(data = hp, aes(x = MasVnrArea, y = log(SalePrice))) + geom_point()

# Foundation ####
#nothing
table(hp$Foundation)
sum(is.na(hp$Foundation)) #0

# BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinSF1, BsmtFinType2, BsmtUnfSF, TotalBsmtSF####

#first make a boolean column isBasement - is there a basement
#make it 0 and 1 s.t. we can do isBsmt * BsmtQual kinda stuff

hp <- hp %>% mutate(isBsmt = !is.na(BsmtQual))

table(hp$isBsmt) #37, 1423

# BsmtQual

hp$BsmtQual %>% levels()
sum(is.na(hp$BsmtQual)) #37

#recode NA as a factor, and assign zero? (applied for all basement categories)
hp <- hp %>% mutate(BsmtQual_mod = recode_factor(BsmtQual,
                                                 'Po' = 1,
                                                 'Fa' = 2,
                                                 'TA' = 3,
                                                 'Gd' = 4,
                                                 'Ex' = 5,
                                                 ))
hp$BsmtQual_mod <- as.numeric(hp$BsmtQual_mod)
hp <- hp %>% replace_na(list(BsmtQual_mod = 0))

#BsmtCond
hp$BsmtCond %>% levels()
sum(is.na(hp$BsmtCond)) #37

hp <- hp %>% mutate(BsmtCond_mod = recode_factor(BsmtCond,
                                                 'Po' = 1,
                                                 'Fa' = 2,
                                                 'TA' = 3,
                                                 'Gd' = 4,
                                                 'Ex' = 5
                                                 ))

hp$BsmtCond_mod <- as.numeric(hp$BsmtCond_mod)
hp <- hp %>% replace_na(list(BsmtCond_mod = 0))

#BsmtExposure
hp$BsmtExposure %>% levels()
sum(is.na(hp$BsmtExposure)) #38

hp %>% filter(is.na(BsmtExposure) & !is.na(BsmtCond)) %>% View()
# there's one 'MISSING VALUE' in this column that's not 'No Basement'

hp <- hp %>% mutate(BsmtExposure_mod = recode_factor(BsmtExposure,
                                                     'No' = 1,
                                                     'Mn' = 2,
                                                     'Av' = 3,
                                                     'Gd' = 4
                                                     ))

hp$BsmtExposure_mod <- as.numeric(hp$BsmtExposure_mod)
# hp <- hp %>% replace_na(list(BsmtExposure_mod = 0))
#can't do this, cuz one of the zeroes is not truly 'zero' in that it's 'No Basement.'

#BsmtFinType1
hp$BsmtFinType1 %>% levels()
table(hp$BsmtFinType1)
sum(is.na(hp$BsmtFinType1)) #37

hp <- hp %>% mutate(BsmtFinType1_mod = recode_factor(BsmtFinType1,
                                                     'Unf' = 1,
                                                     'LwQ' = 2,
                                                     'Rec' = 3,
                                                     'BLQ' = 4,
                                                     'ALQ' = 5,
                                                     'GLQ' = 6
                                                     ))
hp$BsmtFinType1_mod <- as.numeric(hp$BsmtFinType1_mod)
hp <- hp %>% replace_na(list(BsmtFinType1_mod = 0))

#BsmtFinSF1
# ggplot(data = hp, aes(x = BsmtFinSF1, y = log(SalePrice))) + geom_point() 
# one outlier?

#BsmtFinType2
hp$BsmtFinType2 %>% levels()
table(hp$BsmtFinType2)
sum(is.na(hp$BsmtFinType2)) #38?!

hp %>% filter(is.na(BsmtFinType2) & !is.na(BsmtCond)) %>% View()
# there's one 'MISSING VALUE' in this column that's not 'No Basement'

hp <- hp %>% mutate(BsmtFinType2_mod = recode_factor(BsmtFinType2,
                                                     'Unf' = 1,
                                                     'LwQ' = 2,
                                                     'Rec' = 3,
                                                     'BLQ' = 4,
                                                     'ALQ' = 5,
                                                     'GLQ' = 6
                                                     ))
hp$BsmtFinType2_mod <- as.numeric(hp$BsmtFinType2_mod)

# hp <- hp %>% replace_na(list(BsmtFinType2_mod = 0))
#can't do this, cuz one of the zeroes is not truly 'zero' in that it's 'No Basement.'



#BsmtFinSF2
ggplot(data = hp, aes(x = BsmtFinSF2, y = log(SalePrice))) + geom_point() 

#BsmtUnfSF
ggplot(data = hp, aes(x = BsmtUnfSF, y = log(SalePrice))) + geom_point() 

#TotalBsmtSF
ggplot(data = hp, aes(x = TotalBsmtSF, y = log(SalePrice))) + geom_point() 

#sanity check
# hp %>% filter(BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF != TotalBsmtSF) %>% nrow() #0

# Heating####
#nothing
hp$Heating %>% levels()
sum(is.na(hp$Heating)) #0
table(hp$Heating)
ggplot(data = hp, aes(x = Heating, y = log(SalePrice))) + geom_point()

# HeatingQC####
#recoding HeatingQC_mod as ordinal
hp <- hp %>% mutate(HeatingQC_mod = recode_factor(HeatingQC,
                                                  'Po' = 1,
                                                  'Fa' = 2,
                                                  'TA' = 3,
                                                  'Gd' = 4,
                                                  'Ex' = 5
                                                  ))
hp$HeatingQC_mod <- as.numeric(hp$HeatingQC_mod)

# CentralAir####
hp$CentralAir %>% levels() # 95 1365
sum(is.na(hp$CentralAir)) #0

#converting to boolean
hp <- hp %>% transmute(CentralAir = (CentralAir == 'Y'))

table(hp$CentralAir) #95 1365





