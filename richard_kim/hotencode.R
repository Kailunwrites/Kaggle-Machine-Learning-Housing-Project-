#library ####
library(dplyr)

# read csv ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
hp <- read.csv2('../Data/train.csv', sep = ',')
# hp2 <-  read.csv2('../Data/train.csv', sep = ',')

# MSSubClass ####
hp$MSSubClass <- as.factor(hp$MSSubClass)
levels.MSSubClass <- levels(hp$MSSubClass)
hp$MSSubClass <- as.numeric(hp$MSSubClass)

# MSZoning ####
levels.MSZoning <- levels(hp$MSZoning)
hp$MSZoning <- as.numeric(hp$MSZoning)

# Street ####
levels.Street <- levels(hp$Street)
hp$Street <- as.numeric(hp$Street)

# Alley####
hp$Alley = factor(hp$Alley, levels = c(levels(hp$Alley), 'No Alley'))
hp$Alley[is.na(hp$Alley)] = 'No Alley'
levels.Alley <- levels(hp$Alley)
hp$Alley <- as.numeric(hp$Alley)

# LotShape ####
hp <- hp %>% mutate(LotShape = recode_factor(LotShape, 
                                             'Reg' = 1, 
                                             'IR1' = 2, 
                                             'IR2' = 3, 
                                             'IR3' = 4))
levels.LotShape <- c('Reg', 'IR1', 'IR2', 'IR3')
hp$LotShape <- as.numeric(hp$LotShape)

# LandContour ####
levels.LandContour <- levels(hp$LandContour)
hp$LandContour <- as.numeric(hp$LandContour)

# Utilities####
levels.Utilities <- levels(hp$Utilities)
hp$Utilities <- as.numeric(hp$Utilities)

# LotConfig####
levels.LotConfig <- levels(hp$LotConfig)
hp$LotConfig <- as.numeric(hp$LotConfig)

# LandSlope ####
hp <- hp %>% mutate(LandSlope = recode_factor(LandSlope, 
                                              'Gtl' = 1, 
                                              'Mod' = 2, 
                                              'Sev' = 3
))
levels.LandSlope = c('Gtl', 'Mod', 'Sev')
hp$LandSlope <- as.numeric(hp$LandSlope)

# Neighborhood ####
levels.Neighborhood <- levels(hp$Neighborhood)
hp$Neighborhood <- as.numeric(hp$Neighborhood)

# Condition1, 2
levels.Condition1 <- levels(hp$Condition1)
levels.Condition2 <- levels(hp$Condition1)

hp$Condition1 <- as.numeric(hp$Condition1)
hp$Condition2 <- as.numeric(hp$Condition2)

hp <- hp %>% mutate(Condition2, ifelse(Condition2 == 8, 9, Condition2))

# BldgType####
levels.BldgType <- levels(hp$BldgType)
hp$BldgType <- as.numeric(hp$BldgType)

# HouseStyle####
levels.HouseStyle <- levels(hp$HouseStyle)
hp$HouseStyle <- as.numeric(hp$HouseStyle)

# RoofStyle####
levels.RoofStyle <- levels(hp$RoofStyle)
hp$RoofStyle <- as.numeric(hp$RoofStyle)

# RoofMatl####
levels.RoofMatl <- levels(hp$RoofMatl)
hp$RoofMatl <- as.numeric(hp$RoofMatl)

# Exterior1st, 2nd ####
hp <- hp %>% mutate(Exterior2nd = recode_factor(Exterior2nd, 
                                                'Brk Cmn' = 'BrkComm',
                                                'CmentBd' = 'CemntBd',
                                                'Wd Shng' = 'WdShing'))

hp$Exterior2nd <- factor(hp$Exterior2nd, levels = levels.Exterior1st)

levels.Exterior1st <- levels(hp$Exterior2nd)
levels.Exterior2nd <- levels(hp$Exterior2nd)

hp$Exterior1st <- as.numeric(hp$Exterior1st)
hp$Exterior2nd <- as.numeric(hp$Exterior2nd)

# MasVnrType ####
hp$MasVnrType[is.na(hp$MasVnrType)] = 'None'

levels.MasVnrType <- levels(hp$MasVnrType)
hp$MasVnrType <- as.numeric(hp$MasVnrType)

# ExterQual ####

hp$ExterQual <- factor(hp$ExterQual, levels = c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
levels.ExterQual <- levels(hp$ExterQual)

hp$ExterQual <- as.numeric(hp$ExterQual)

# ExterCond ####
hp$ExterCond <- factor(hp$ExterCond, levels = c('Po', 'Fa', 'TA', 'Gd', 'Ex'))
levels.ExterCond <- levels(hp$ExterCond)

hp$ExterCond <- as.numeric(hp$ExterCond)

# Foundation ####
levels.Foundation <- levels(hp$Foundation)
hp$Foundation <- as.numeric(hp$Foundation)


# hp_rk ####
hp_rk <- hp %>% dplyr::select(Id, MSZoning, Street, Alley, LotShape, LandContour,
                              Utilities, LotConfig, LandSlope, Neighborhood,
                              Condition1, Condition2, BldgType, HouseStyle, RoofStyle,
                              RoofMatl, Exterior1st, Exterior2nd, MasVnrType, ExterQual,
                              ExterCond, Foundation)

# write.csv(hp_rk, file = 'rk_hotencode.csv')
