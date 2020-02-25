# Description####

# Creating a baseline model where minimal manipulation is done (filling in NA)
# and ran on lm function

#library ####
library(dplyr)
library(ggplot2)
# library(tidyr)
# library(MASS)
# library(tidyimpute)

#read csv ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
hp <- read.csv2('../Data/train.csv', sep = ',')


#manip data ####
hp$MSSubClass <- as.factor(hp$MSSubClass)

hp$Alley = factor(hp$Alley, levels = c(levels(hp$Alley), 'No Alley'))
hp$Alley[is.na(hp$Alley)] = 'No Alley'

hp$BsmtQual = factor(hp$BsmtQual, levels = c(levels(hp$BsmtQual), 'No Bsmt'))
hp$BsmtQual[is.na(hp$BsmtQual)] = 'No Bsmt'

hp$BsmtCond = factor(hp$BsmtCond, levels = c(levels(hp$BsmtCond), 'No Bsmt'))
hp$BsmtCond[is.na(hp$BsmtCond)] = 'No Bsmt'

hp$BsmtExposure = factor(hp$BsmtExposure, levels = c(levels(hp$BsmtExposure), 'No Bsmt'))
hp$BsmtExposure[is.na(hp$BsmtExposure)] = 'No Bsmt'

hp$BsmtFinType1 = factor(hp$BsmtFinType1, levels = c(levels(hp$BsmtFinType1), 'No Bsmt'))
hp$BsmtFinType1[is.na(hp$BsmtFinType1)] = 'No Bsmt'

hp$BsmtFinType2 = factor(hp$BsmtFinType2, levels = c(levels(hp$BsmtFinType2), 'No Bsmt'))
hp$BsmtFinType2[is.na(hp$BsmtFinType2)] = 'No Bsmt'

hp$FireplaceQu = factor(hp$FireplaceQu, levels = c(levels(hp$FireplaceQu), 'No Fireplace'))
hp$FireplaceQu[is.na(hp$FireplaceQu)] = 'No Fireplace'

hp$GarageType = factor(hp$GarageType, levels = c(levels(hp$GarageType), 'No Garage'))
hp$GarageType[is.na(hp$GarageType)] = 'No Garage'

hp$GarageFinish = factor(hp$GarageFinish, levels = c(levels(hp$GarageFinish), 'No Garage'))
hp$GarageFinish[is.na(hp$GarageFinish)] = 'No Garage'

hp$GarageQual = factor(hp$GarageQual, levels = c(levels(hp$GarageQual), 'No Garage'))
hp$GarageQual[is.na(hp$GarageQual)] = 'No Garage'

hp$GarageCond = factor(hp$GarageCond, levels = c(levels(hp$GarageCond), 'No Garage'))
hp$GarageCond[is.na(hp$GarageCond)] = 'No Garage'

hp$PoolQC = factor(hp$PoolQC, levels = c(levels(hp$PoolQC), 'No Pool'))
hp$PoolQC[is.na(hp$PoolQC)] = 'No Pool'

hp$Fence = factor(hp$Fence, levels = c(levels(hp$Fence), 'No Fence'))
hp$Fence[is.na(hp$Fence)] = 'No Fence'

hp$MiscFeature = factor(hp$Fence, levels = c(levels(hp$MiscFeature), 'None'))
hp$MiscFeature[is.na(hp$MiscFeature)] = 'None'

hp$LotFrontage[is.na(hp$LotFrontage)] = 0
hp$MasVnrType[is.na(hp$MasVnrType)] = 'None'
hp$MasVnrArea[is.na(hp$MasVnrArea)] = 0

hp <- hp %>% mutate(GarageYrBlt = ifelse(GarageType == 'No Garage', YearBuilt, GarageYrBlt))

# building lm ####
baselinemodel <- lm(data = hp,
                    formula = log(SalePrice) ~ MSSubClass + 
                      MSZoning + 
                      LotFrontage + 
                      LotArea +
                      Street +
                      Alley +
                      LotShape + 
                      LandContour +
                      LotConfig +
                      LandSlope +
                      Neighborhood +
                      Condition1 +
                      Condition2 +
                      BldgType +
                      HouseStyle +
                      OverallQual +
                      OverallCond +
                      YearBuilt +
                      YearRemodAdd +
                      RoofStyle +
                      RoofMatl +
                      Exterior1st +
                      Exterior2nd +
                      MasVnrType +
                      MasVnrArea +
                      ExterQual +
                      ExterCond +
                      Foundation +
                      BsmtQual +
                      BsmtCond +
                      BsmtExposure +
                      BsmtFinType1 +
                      BsmtFinSF1 +
                      BsmtFinType2 +
                      BsmtFinSF2 +
                      BsmtUnfSF +
                      TotalBsmtSF +
                      Heating +
                      HeatingQC +
                      CentralAir +
                      Electrical +
                      X1stFlrSF +
                      X2ndFlrSF +
                      LowQualFinSF +
                      GrLivArea +
                      BsmtFullBath +
                      BsmtHalfBath +
                      FullBath +
                      HalfBath +
                      BedroomAbvGr +
                      KitchenAbvGr +
                      KitchenQual +
                      TotRmsAbvGrd +
                      Functional +
                      Fireplaces +
                      FireplaceQu +
                      GarageType +
                      GarageYrBlt +
                      GarageFinish +
                      GarageCars +
                      GarageArea +
                      GarageQual +
                      GarageCond +
                      PavedDrive +
                      WoodDeckSF +
                      OpenPorchSF +
                      EnclosedPorch +
                      X3SsnPorch +
                      ScreenPorch +
                      PoolArea +
                      PoolQC +
                      Fence +
                      #MiscFeature
                      MiscVal +
                      MoSold +
                      YrSold +
                      SaleType +
                      SaleCondition
                      )

options(max.print=2000)
summary(baselinemodel)




