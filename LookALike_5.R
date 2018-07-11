#### Load Library and Read in Data ####
library(RevoScaleR)
library(stringr)
library(dummies)
library(dplyr)
library(caret)

setwd('/users/teams/nad/conapx/LookALike')
DF <- rxReadXdf("/users/shared/nad/DS/projects/advisorSegmentation/discovery/input/advisorUniverseUpdated_v3_MM0413_RETMT_Vars.XDF")
Feature = read.csv('Featureset.csv',header = TRUE, stringsAsFactors = FALSE)
StratClass = read.csv('Advisor_StrategicClassification.csv', header = TRUE, stringsAsFactors = TRUE)
StratClass[is.na(StratClass)] = 0
StratClass$AEID = as.character(StratClass[,1])
DF = DF %>% left_join(StratClass, c("CJ_AE_SEQ_NUM" = "AEID"))

# for advisors not in StratClass, group to others
DF$Other[is.na(DF$Bank) & is.na(DF$Base) & is.na(DF$Core_Plus) & is.na(DF$Core) & is.na(DF$Other) & is.na(DF$Regional) & is.na(DF$Strategic) & is.na(DF$Wirehouses)] = 1
DF$Bank[is.na(DF$Bank)] = 0
DF$Base[is.na(DF$Base)] = 0
DF$Core[is.na(DF$Core)] = 0
DF$Core_Plus[is.na(DF$Core_Plus)] = 0
DF$Regional[is.na(DF$Regional)] = 0
DF$Strategic[is.na(DF$Strategic)] = 0
DF$Wirehouses[is.na(DF$Wirehouses)] = 0

#### Self Defined Functions ####
get_title_groups <- function(x){
  if (is.na(x)) {
    gp = 'Unknown'
  } else if (str_detect(x,'President') | str_detect(x,'Principal') | str_detect(x,'Partner') | str_detect(x,'Director') |
             str_detect(x,'Chief')) {
    gp = 'Senior'

  } else if (str_detect(x,'Advisor')) {
    gp = 'Advisor'
  } else if (str_detect(x,'Consultant')) {
    gp = 'Consultant'
  } else if (str_detect(x,'Portfolio Manager')) {
    gp = 'Portfolio Manager'
  } else if (str_detect(x,'Branch Manager')) {
    gp = 'Branch Manager'
  } else if (str_detect(x,'Analyst') | str_detect(x,'Agent') | str_detect(x,'Representative') | str_detect(x,'Associate') |
             str_detect(x,'Relationship') | str_detect(x, 'Compliance')| str_detect(x,'Assistant')) {
    gp = 'Support'
  } else if (str_detect(x,'Manager')) {
    gp = 'Manager'
  } else 
    gp = 'Other'
  return(gp)
}

checkNA <- function(vec){
  return(any(is.na(vec)))
}

checkZero <- function(vec){
  return(any(vec == 0))
}

checkTrue <- function(vec){
  return(any(vec))
}

colStats <- function(data){
  res = cbind(sapply(data, max, na.rm = TRUE),sapply(data, min, na.rm = TRUE),sapply(data, median, na.rm = TRUE),
              sapply(data, checkNA))
  colnames(res) = c('max','min','median','NA')
  return(res)
}

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

#### Transform #### 750
X = DF[,dput(Feature$Colnames)]

# Group visits and calls
X$DW_Visit_2016 = X$MS_DW_GRP_visits_2016 + X$MS_DW_INDV_visits_2016 
X$NA_Visit_2016 = X$MS_NA_GRP_visits_2016 + X$MS_NA_INDV_visits_2016
X$RPC_visit_2016 = X$MS_RPC_GRP_visits_2016 + X$MS_RPC_INDV_visits_2016
X$WLSR_Visit_2016 = X$MS_WLSR_GRP_visits_2016 + X$MS_WLSR_INDV_visits_2016
X$Others_Visit_2016 = X$MS_Others_GRP_visits_2016 + X$MS_Others_INDV_visits_2016
X$Visit_2016 = X$Others_Visit_2016+ X$WLSR_Visit_2016 + X$RPC_visit_2016 + X$NA_Visit_2016 + X$DW_Visit_2016
X$Joint_Visit_2016 = X$CJ_Joint_1on1_2016 + X$CJ_Joint_Group_2016
X$RPC_Visit_2016 = X$CJ_RPC_1on1_2016 + X$CJ_RPC_Group_2016 + X$CJ_RPC_MTG_2016
X$WLSR2_Visit_2016 = X$CJ_WLSR_1on1_2016 + X$CJ_WLSR_Group_2016 + X$CJ_WLSR_MTG_2016

X$ASR_calls_2016 = X$MS_ASR_Inbound_calls_2016 + X$MS_ASR_Others_calls_2016 + X$MS_ASR_Outbound_calls_2016
X$Others_calls_2016 = X$MS_Others_Inbound_calls_2016 + X$MS_Others_Others_calls_2016 + X$MS_Others_Outbound_calls_2016
X$RPSC_calls_2016 = X$MS_RPSC_Inbound_calls_2016 + X$MS_RPSC_Others_calls_2016 + X$MS_RPSC_Outbound_calls_2016
X$Calls_2016 = X$ASR_calls_2016 + X$Others_calls_2016 + X$RPSC_calls_2016

X$DW_Visit_2017 = X$MS_DW_GRP_visits_2017 + X$MS_DW_INDV_visits_2017 
X$NA_Visit_2017 = X$MS_NA_GRP_visits_2017 + X$MS_NA_INDV_visits_2017
X$RPC_visit_2017 = X$MS_RPC_GRP_visits_2017 + X$MS_RPC_INDV_visits_2017
X$WLSR_Visit_2017 = X$MS_WLSR_GRP_visits_2017 + X$MS_WLSR_INDV_visits_2017
X$Others_Visit_2017 = X$MS_Others_GRP_visits_2017 + X$MS_Others_INDV_visits_2017
X$Visit_2017 = X$Others_Visit_2017+ X$WLSR_Visit_2017 + X$RPC_visit_2017 + X$NA_Visit_2017 + X$DW_Visit_2017
X$Joint_Visit_2017 = X$CJ_Joint_1on1_2017 + X$CJ_Joint_Group_2017
X$RPC_Visit_2017 = X$CJ_RPC_1on1_2017 + X$CJ_RPC_Group_2017 + X$CJ_RPC_MTG_2017
X$WLSR2_Visit_2017 = X$CJ_WLSR_1on1_2017 + X$CJ_WLSR_Group_2017 + X$CJ_WLSR_MTG_2017


X$ASR_calls_2017 = X$MS_ASR_Inbound_calls_2017 + X$MS_ASR_Others_calls_2017 + X$MS_ASR_Outbound_calls_2017
X$Others_calls_2017 = X$MS_Others_Inbound_calls_2017 + X$MS_Others_Others_calls_2017 + X$MS_Others_Outbound_calls_2017
X$RPSC_calls_2017 = X$MS_RPSC_Inbound_calls_2017 + X$MS_RPSC_Others_calls_2017 + X$MS_RPSC_Outbound_calls_2017
X$Calls_2017 = X$ASR_calls_2017 + X$Others_calls_2017 + X$RPSC_calls_2017

# Create broader title categories
X$Title_Group = sapply(X$DIS_Title, get_title_groups)

# Calculating sales % [exist] MS_sls_perc = MS_sls / MS_slsTotal
# Calculating asset % [exist]: MS_Asset_perc = MS_Asset / Total MS_Asset

# Calculating office level book of size, and asset allocation %
# NOT using (OFFICE LEVEL BROADRIDGE DATA) because don't know which year they are
# X$Office_AF_as_Total_AUM = X$RB_AFAUM / X$RB_IndustryAUM
# X$Office_Allocation_Perc_AUM = X$RB_Allocation_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM
# X$Office_Alternative_Perc_AUM = X$RB_Alternative_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM
# X$Office_Commodities_Perc_AUM = X$RB_Commodities_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM
# X$Office_FixedIncome_Perc_AUM = X$RB_Fixed.Income_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM
# X$Office_Equity_Perc_AUM = X$RB_Equity_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM
# X$Office_MoneyMarket_Perc_AUM = X$RB_Money.Market_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM
# X$Office_Other_Perc_AUM = X$RB_Other_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM
# X$Office_TaxPreferred_Perc_AUM = X$RB_Tax.Preferred_Open.End.Fund_IndustryAUM / X$RB_openEnd_IndustryAUM

# X$Office_AF_as_Total_AUM[X$RB_IndustryAUM == 0] = NaN
# X$Office_AF_as_Total_AUM[X$Office_AF_as_Total_AUM > 1] = NaN

# NOT calculating 2016 because no 2016
# X$MS_slsRtmt2017_perc[is.infinite(X$MS_slsRtmt2017_perc)] = NaN
# X$MS_slsRetail2017_perc[is.infinite(X$MS_slsRetail2017_perc)] = NaN
# X$MS_slsIRA2017_perc [is.infinite(X$MS_slsIRA2017_perc )] = NaN
# X$MS_slsRtmt2017_perc[X$MS_slsRtmt2017_perc > 1 | X$MS_slsRtmt2017_perc < 0] = NaN
# X$MS_slsRetail2017_perc[X$MS_slsRetail2017_perc > 1 | X$MS_slsRetail2017_perc < 0] = NaN
# X$MS_slsIRA2017_perc[X$MS_slsIRA2017_perc > 1 | X$MS_slsIRA2017_perc < 0] = NaN

# Calculate asset class sales percentage
X$MS_SLS_2016_ALLOCATION_perc = X$MS_SLS_2016_ALLOCATION / X$MS_AGG_SLS_2016
X$MS_SLS_2016_INTL_EQUITY_perc = X$MS_SLS_2016_INTL_EQUITY / X$MS_AGG_SLS_2016
X$MS_SLS_2016_MUNI_BOND_perc = X$MS_SLS_2016_MUNI_BOND / X$MS_AGG_SLS_2016
X$MS_SLS_2016_TAXABLE_BOND_perc = X$MS_SLS_2016_TAXABLE_BOND / X$MS_AGG_SLS_2016
X$MS_SLS_2016_US_EQUITY_perc = X$MS_SLS_2016_US_EQUITY / X$MS_AGG_SLS_2016


X$MS_SLS_2016_ALLOCATION_perc[X$MS_SLS_2016_ALLOCATION_perc > 1 | X$MS_SLS_2016_ALLOCATION_perc < 0] = NaN
X$MS_SLS_2016_INTL_EQUITY_perc[X$MS_SLS_2016_INTL_EQUITY_perc > 1 | X$MS_SLS_2016_INTL_EQUITY_perc < 0] = NaN
X$MS_SLS_2016_MUNI_BOND_perc[X$MS_SLS_2016_MUNI_BOND_perc > 1 | X$MS_SLS_2016_MUNI_BOND_perc < 0] = NaN
X$MS_SLS_2016_TAXABLE_BOND_perc[X$MS_SLS_2016_TAXABLE_BOND_perc > 1 | X$MS_SLS_2016_TAXABLE_BOND_perc < 0] = NaN
X$MS_SLS_2016_US_EQUITY_perc[X$MS_SLS_2016_US_EQUITY_perc > 1 | X$MS_SLS_2016_US_EQUITY_perc < 0] = NaN

# Calculate asset share class percentage (not doing the same for sales right now)
X$MS_ASSET_Perc_529_2016 = X$MS_ASSET_VAL2016_529/X$MS_ASSET_2016
X$MS_ASSET_Perc_ABC_2016 = X$MS_ASSET_VAL2016_ABC/X$MS_ASSET_2016
X$MS_ASSET_Perc_R_2016 = X$MS_ASSET_VAL2016_R/X$MS_ASSET_2016
X$MS_ASSET_Perc_F_2016 = X$MS_ASSET_VAL2016_F/X$MS_ASSET_2016

# Retirement related metrics
X$isAF_Top1_TDF = (X$CJ_Top1_TDF_FromSFDC == 'American Funds')
X$isAF_Top2_TDF = (X$CJ_Top2_TDF_FromSFDC == 'American Funds')
X$isAF_Top1_RK = (X$CJ_Top1_RK_FromSFDC == 'American Funds')
X$isAF_Top2_RK = (X$CJ_Top2_RK_FromSFDC == 'American Funds')
X$isAF_Top3_RK = (X$CJ_Top3_RK_FromSFDC == 'American Funds')

# Complexity
X$Asset_Complexity_Group[is.na(X$ASSET_COMPLEXITY2)] = 'N/A'
X$Asset_Complexity_Group[!is.na(X$ASSET_COMPLEXITY2) & X$ASSET_COMPLEXITY2 <= summary(X$ASSET_COMPLEXITY2)['1st Qu.']] = '1-Quartile'
X$Asset_Complexity_Group[!is.na(X$ASSET_COMPLEXITY2) 
                         & X$ASSET_COMPLEXITY2 <= summary(X$ASSET_COMPLEXITY2)['Median'] 
                         & X$ASSET_COMPLEXITY2 > summary(X$ASSET_COMPLEXITY2)['1st Qu.']] = '2-Quartile'
X$Asset_Complexity_Group[!is.na(X$ASSET_COMPLEXITY2) 
                         & X$ASSET_COMPLEXITY2 <= summary(X$ASSET_COMPLEXITY2)['3rd Qu.'] 
                         & X$ASSET_COMPLEXITY2 > summary(X$ASSET_COMPLEXITY2)['Median']] = '3-Quartile'

X$Asset_Complexity_Group[!is.na(X$ASSET_COMPLEXITY2) 
                         & X$ASSET_COMPLEXITY2 > summary(X$ASSET_COMPLEXITY2)['3rd Qu.']] = '4-Quartile Above'



#### Replace bad value with NaN ####
X$DIS_IndustryExp2018[X$DIS_IndustryExp2018 < 0] = NaN
X$DIS_CurrentFirmExp2018[X$DIS_CurrentFirmExp2018 < 0] = NaN



ColToBeCat = c('MS_ASSET_Perc_529_2016'
               ,'MS_ASSET_Perc_ABC_2016'
               ,'MS_ASSET_Perc_R_2016'
               ,'MS_ASSET_Perc_F_2016'
               ,'MS_SLS_2016_ALLOCATION_perc'
               ,'MS_SLS_2016_INTL_EQUITY_perc'
               ,'MS_SLS_2016_MUNI_BOND_perc'
               ,'MS_SLS_2016_TAXABLE_BOND_perc'
               ,'MS_SLS_2016_US_EQUITY_perc'
               ,'ASSET_COMPLEXITY2'
               ,'MS_AGG_SLS_2016'
               ,'MS_ASSET_2016'
)

ColOutputCat = c('MS_ASSET_Perc_529_Group'
               ,'MS_ASSET_Perc_ABC_Group'
               ,'MS_ASSET_Perc_R_Group'
               ,'MS_ASSET_Perc_F_Group'
               ,'MS_SLS_2016_ALLOCATION_perc_Group'
               ,'MS_SLS_2016_INTL_EQUITY_perc_Group'
               ,'MS_SLS_2016_MUNI_BOND_perc_Group'
               ,'MS_SLS_2016_TAXABLE_BOND_perc_Group'
               ,'MS_SLS_2016_US_EQUITY_perc_Group'
               ,'ASSET_COMPLEXITY2_Group'
               ,'MS_AGG_SLS_2016_Group'
               ,'MS_ASSET_2016_Group'
)

for (c in 1:length(ColToBeCat)){
  inpCol = ColToBeCat[c]
  outCol = ColOutputCat[c]
  print(paste('Processing: ', inpCol))
  
  if (any(is.na(X[,inpCol]))){
    print(paste(inpCol, ': NA Found'))
    X[is.na(X[,inpCol]),outCol] = 'N/A'
  }
  
  tmp = quantile(X[,inpCol], na.rm = TRUE)
  X[X[,inpCol] <= tmp['25%'] & !is.na(X[,inpCol]),outCol] = '0% - 25%'
  X[X[,inpCol] <= tmp['75%'] & X[,inpCol] > tmp['25%'] & !is.na(X[,inpCol]), outCol] = '25% - 75%'
  X[X[,inpCol] > tmp['75%'] & !is.na(X[,inpCol]), outCol] = '75%+'
}

#### More retirement related ####
# plan_sold  #bucket: neg (NA), 1, 2-4, 5+
# DCIO, DC
# •	Redeption, NCF
# •	Momentum # only use IRA, Retail, Retirement momentum
# •	Months since max Sales, since max NCF

# CJ_DCIO_AUM_2017                                                        # not used yet
# CJ_DCIO_Sales_2017                                                      # not used yet
# DCIO_DCSales_perc                                                       # not used yet
# DCIO_DCAUM_perc quantile(DF$DCIO_DCAUM_perc, seq(0,1,.05)) # 10% binary # not used yet
# CJ_DCProp_AUM_2017                                                      # not used yet
# CJ_DC_AUM_2017 # bucket: zero (NA), < 1M, > 1M
# CJ_DC_Sales_2017 # bucket: negative (NA), <10K, 10-50K, 50-200K, 200K+
# CJ_Total_DC_AMT # bucket: negative (NA), <1M, 1-10M, 10-50M, 50M+       # not used yet
# CJ_Total_DC_NUM # bucket: 0, 1-2, 3-4, 5+                               # not used yet
# DCProp_DCSales_perc quantile(DF$DCProp_DCSales_perc, seq(0,1,.1))       # not used yet
# DCProp_DCAUM_perc                                                       # not used yet
# plan_cnt_industry_DC_LT_1M_2017 # binary 1-0
# plan_cnt_industry_DC_1M_10M_2017  # binary 1-0
# plan_cnt_industry_DC_10M_100M_2017 # binary 1-0
# plan_cnt_industry_DC_100M_250M_2017 # not useful - NA or 0
# plan_cnt_industry_DC_gt_250M_2017 # not useful - NA or 0
# plan_amt_industry_DC_LT_1M_2017 #binary 1-0
# plan_amt_industry_DC_1M_10M_2017 # binary 1-0
# plan_amt_industry_DC_10M_100M_2017 # binary 1-0
# plan_amt_industry_DC_100M_250M_2017 # not useful - NA or 0
# plan_amt_industry_DC_gt_250M_2017 # not useful - NA or 0



#### Create Categorical Variables ####
# plan_sold
X$Plan_Sold_Group[is.na(X$Plan_Sold) | X$Plan_Sold == 0] = '0'
X$Plan_Sold_Group[!is.na(X$Plan_Sold) & X$Plan_Sold == 1] = '1'
X$Plan_Sold_Group[!is.na(X$Plan_Sold) & X$Plan_Sold > 1 & X$Plan_Sold <= 4] = '2-4'
X$Plan_Sold_Group[!is.na(X$Plan_Sold) & X$Plan_Sold > 4] = '5+'

# retirement momentum
X$MS_Retirement_momentum_12mon_Dir[X$MS_Retirement_momentum_12mon.y >= 0] = 1
X$MS_Retirement_momentum_12mon_Dir[X$MS_Retirement_momentum_12mon.y < 0] = 0

X$MS_Retirement_momentum_24mon_Dir[X$MS_Retirement_momentum_24mon.y >= 0] = 1
X$MS_Retirement_momentum_24mon_Dir[X$MS_Retirement_momentum_24mon.y < 0] = 0

X$MS_Retirement_momentum_3mon_Dir[X$MS_Retirement_momentum_3mon.y >= 0] = 1
X$MS_Retirement_momentum_3mon_Dir[X$MS_Retirement_momentum_3mon.y < 0] = 0

X$MS_Retirement_momentum_6mon_Dir[X$MS_Retirement_momentum_6mon.y >= 0] = 1
X$MS_Retirement_momentum_6mon_Dir[X$MS_Retirement_momentum_6mon.y < 0] = 0

# IRA momentum
X$MS_IRA_momentum_12mon_Dir[X$MS_IRA_momentum_12mon.y >= 0] = 1
X$MS_IRA_momentum_12mon_Dir[X$MS_IRA_momentum_12mon.y < 0] = 0

X$MS_IRA_momentum_24mon_Dir[X$MS_IRA_momentum_24mon.y >= 0] = 1
X$MS_IRA_momentum_24mon_Dir[X$MS_IRA_momentum_24mon.y < 0] = 0

X$MS_IRA_momentum_3mon_Dir[X$MS_IRA_momentum_3mon.y >= 0] = 1
X$MS_IRA_momentum_3mon_Dir[X$MS_IRA_momentum_3mon.y < 0] = 0

X$MS_IRA_momentum_6mon_Dir[X$MS_IRA_momentum_6mon.y >= 0] = 1
X$MS_IRA_momentum_6mon_Dir[X$MS_IRA_momentum_6mon.y < 0] = 0

# Retail momentum
X$MS_Retail_momentum_12mon_Dir[X$MS_Retail_momentum_12mon.y >= 0] = 1
X$MS_Retail_momentum_12mon_Dir[X$MS_Retail_momentum_12mon.y < 0] = 0

X$MS_Retail_momentum_24mon_Dir[X$MS_Retail_momentum_24mon.y >= 0] = 1
X$MS_Retail_momentum_24mon_Dir[X$MS_Retail_momentum_24mon.y < 0] = 0

X$MS_Retail_momentum_3mon_Dir[X$MS_Retail_momentum_3mon.y >= 0] = 1
X$MS_Retail_momentum_3mon_Dir[X$MS_Retail_momentum_3mon.y < 0] = 0

X$MS_Retail_momentum_6mon_Dir[X$MS_Retail_momentum_6mon.y >= 0] = 1
X$MS_Retail_momentum_6mon_Dir[X$MS_Retail_momentum_6mon.y < 0] = 0

# DC AUM 2017
X$CJ_DC_AUM_2017_Group[is.na(X$CJ_DC_AUM_2017) | X$CJ_DC_AUM_2017 == 0] = 'N/A'
X$CJ_DC_AUM_2017_Group[!is.na(X$CJ_DC_AUM_2017) & X$CJ_DC_AUM_2017 <= 1e6] = '<=1M'
X$CJ_DC_AUM_2017_Group[!is.na(X$CJ_DC_AUM_2017) & X$CJ_DC_AUM_2017 > 1e6] = '>1M'

# DC Sales 2017 # bucket: negative (NA), <10K, 10-50K, 50-200K, 200K+
X$CJ_DC_Sales_2017_Group[is.na(X$CJ_DC_Sales_2017) | X$CJ_DC_Sales_2017 == 0] = 'N/A'
X$CJ_DC_Sales_2017_Group[!is.na(X$CJ_DC_Sales_2017) & X$CJ_DC_Sales_2017 <= 10000] = '<=10K'
X$CJ_DC_Sales_2017_Group[!is.na(X$CJ_DC_Sales_2017) & X$CJ_DC_Sales_2017 > 10000 & X$CJ_DC_Sales_2017 <= 50000] = '10-50K'
X$CJ_DC_Sales_2017_Group[!is.na(X$CJ_DC_Sales_2017) & X$CJ_DC_Sales_2017 > 50000 & X$CJ_DC_Sales_2017 <= 200000] = '50-200K'
X$CJ_DC_Sales_2017_Group[!is.na(X$CJ_DC_Sales_2017) & X$CJ_DC_Sales_2017 > 200000] = '200K+'

# Plan count
X$plan_cnt_industry_DC_LT_1M_2017_Group[!is.na(X$plan_cnt_industry_DC_LT_1M_2017) & X$plan_cnt_industry_DC_LT_1M_2017 > 0] = 1
X$plan_cnt_industry_DC_LT_1M_2017_Group[is.na(X$plan_cnt_industry_DC_LT_1M_2017) | X$plan_cnt_industry_DC_LT_1M_2017 == 0] = 0

X$plan_cnt_industry_DC_1M_10M_2017_Group[!is.na(X$plan_cnt_industry_DC_1M_10M_2017) & X$plan_cnt_industry_DC_1M_10M_2017 > 0] = 1
X$plan_cnt_industry_DC_1M_10M_2017_Group[is.na(X$plan_cnt_industry_DC_1M_10M_2017) | X$plan_cnt_industry_DC_1M_10M_2017 == 0] = 0

X$plan_cnt_industry_DC_10M_100M_2017_Group[!is.na(X$plan_cnt_industry_DC_10M_100M_2017) & X$plan_cnt_industry_DC_10M_100M_2017 > 0] = 1
X$plan_cnt_industry_DC_10M_100M_2017_Group[is.na(X$plan_cnt_industry_DC_10M_100M_2017) | X$plan_cnt_industry_DC_10M_100M_2017 == 0] = 0

X$plan_amt_industry_DC_LT_1M_2017_Group[!is.na(X$plan_amt_industry_DC_LT_1M_2017) & X$plan_amt_industry_DC_LT_1M_2017 > 0] = 1
X$plan_amt_industry_DC_LT_1M_2017_Group[is.na(X$plan_amt_industry_DC_LT_1M_2017) | X$plan_amt_industry_DC_LT_1M_2017 == 0] = 0

X$plan_amt_industry_DC_1M_10M_2017_Group[!is.na(X$plan_amt_industry_DC_1M_10M_2017) & X$plan_amt_industry_DC_1M_10M_2017 > 0] = 1
X$plan_amt_industry_DC_1M_10M_2017_Group[is.na(X$plan_amt_industry_DC_1M_10M_2017) | X$plan_amt_industry_DC_1M_10M_2017 == 0] = 0

X$plan_amt_industry_DC_10M_100M_2017_Group[!is.na(X$plan_amt_industry_DC_10M_100M_2017) & X$plan_amt_industry_DC_10M_100M_2017 > 0] = 1
X$plan_amt_industry_DC_10M_100M_2017_Group[is.na(X$plan_amt_industry_DC_10M_100M_2017) | X$plan_amt_industry_DC_10M_100M_2017 == 0] = 0

#### Asset Complexity ####

summary(DF$ASSET_COMPLEXITY2)
quantile(DF$ASSET_COMPLEXITY2, seq(0,1,.1), na.rm = TRUE)


#### Bucket demographic ####
# age
X$Age_Group[X$DIS_Age2018_imputed <= 40] = '<= 40'
X$Age_Group[X$DIS_Age2018_imputed > 40 & X$DIS_Age2018_imputed <= 60] = '41 to 60'
X$Age_Group[X$DIS_Age2018_imputed > 60] = '> 60'

# industry tenure
X$IndustryTenure_Group[is.na(X$DIS_IndustryExp2018)] = 'N/A'
X$IndustryTenure_Group[X$DIS_IndustryExp2018 <= 10 & (!is.na(X$DIS_IndustryExp2018))] = '<= 10'
X$IndustryTenure_Group[X$DIS_IndustryExp2018 > 10 & X$DIS_IndustryExp2018 <= 20] = '11 to 20'
X$IndustryTenure_Group[X$DIS_IndustryExp2018 > 20] = '> 20'

# current firm tenure
quantile(X$DIS_CurrentFirmExp2018, seq(0,1,.1), na.rm = TRUE)
X$CurrentFirmTenure_Group[is.na(X$DIS_CurrentFirmExp2018)] = 'N/A'
X$CurrentFirmTenure_Group[X$DIS_CurrentFirmExp2018 <= 4 & (!is.na(X$DIS_CurrentFirmExp2018))] = '<= 4'
X$CurrentFirmTenure_Group[X$DIS_CurrentFirmExp2018 > 4 & X$DIS_CurrentFirmExp2018 <= 10] = '5 to 10'
X$CurrentFirmTenure_Group[X$DIS_CurrentFirmExp2018 >= 10] = '> 10'

# numPriorFirms
quantile(X$numPriorFirms, seq(0,1,.1))
X$numPriorFirms_Group[is.na(X$numPriorFirms)] = 'N/A'
X$numPriorFirms_Group[X$numPriorFirms <= 2] = '<= 2'
X$numPriorFirms_Group[X$numPriorFirms > 2] = '> 2'

# number firm associations (what is this?)

# visit and calls
X$hasSFVisit[X$Visit_2017 == 0] = 'N'
X$hasSFVisit[X$Visit_2017 > 0] = 'Y'

X$hasSFCalls[X$Calls_2017 == 0] = 'N'
X$hasSFCalls[X$Calls_2017 > 0] = 'Y'

quantile(X$WLSR2_Visit_2017, seq(0,1,.1), na.rm = TRUE)
X$WLSR2_Visit_Group[is.na(X$WLSR2_Visit_2017) | X$WLSR2_Visit_2017 == 0] = 'No Visit'
X$WLSR2_Visit_Group[X$WLSR2_Visit_2017 > 0 & X$WLSR2_Visit_2017 <= 2] = '<= 2'
X$WLSR2_Visit_Group[X$WLSR2_Visit_2017 > 2] = '> 2'

# AUM level
X$AUM_level_Cat[X$AUM_LVL == 1] = '1'
X$AUM_level_Cat[X$AUM_LVL == 2] = '2'
X$AUM_level_Cat[X$AUM_LVL == 3] = '3'
X$AUM_level_Cat[X$AUM_LVL == 4] = '4'
X$AUM_level_Cat[X$AUM_LVL == 5] = '5'

# fund count
quantile(X$MS_DISTINCT_FUND_CNT_ASSET_2017, seq(0,1,.1))
X$DistFund_Cnt[X$MS_DISTINCT_FUND_CNT_ASSET_2017 == 0] = '0'
X$DistFund_Cnt[X$MS_DISTINCT_FUND_CNT_ASSET_2017 <= 20 & X$MS_DISTINCT_FUND_CNT_ASSET_2017 > 0] = '1 to 20'
X$DistFund_Cnt[X$MS_DISTINCT_FUND_CNT_ASSET_2017 > 20] = '> 20'

# share class count
quantile(X$MS_DISTINCT_SHARE_GRP_ASSET_2017, seq(0,1,.1))
X$DistShrClass_Cnt[X$MS_DISTINCT_SHARE_GRP_ASSET_2017 == 0] = '0'
X$DistShrClass_Cnt[X$MS_DISTINCT_SHARE_GRP_ASSET_2017 <= 3 & X$MS_DISTINCT_SHARE_GRP_ASSET_2017 > 0] = '1 to 3'
X$DistShrClass_Cnt[X$MS_DISTINCT_SHARE_GRP_ASSET_2017 > 3] = '> 3'

#### One Hot ####

fea_col = c(#'MS_PARNT_ORG_NM',
            'DIS_Designations_CFA',
            'DIS_Designations_CFP',
            'DIS_Designations_CPA',
            'DIS_Designations_ChFC',
            'DIS_Designations_CLU',
            'MS_RP_SEGMENT',
            'SFDC_RETAIL_SEGMENTATION__C',
            'SFDC_RP_SEGMENT__C',
            #'DIS_Age2018_imputed',
            #'DIS_IndustryExp2018',
            #'DIS_CurrentFirmExp2018',
            #'numPriorFirms',
            #'DIS_NumberFirmAssociations',
            'MS_PRMRY_CHANNEL_NM',
            'MS_TERR_NM',
            'Title_Group',
            #'DW_Visit_2017',
            #'NA_Visit_2017',
            #'RPC_visit_2017',
            #'WLSR_Visit_2017',
            #'Others_Visit_2017',
            #'Visit_2017',
            #'ASR_calls_2017',
            #'Others_calls_2017',
            #'RPSC_calls_2017',
            #'Calls_2017',
            # 'Office_AF_as_Total_AUM',
            # 'Office_Allocation_Perc_AUM',
            # 'Office_Alternative_Perc_AUM',
            # 'Office_Commodities_Perc_AUM',
            # 'Office_FixedIncome_Perc_AUM',
            # 'Office_Equity_Perc_AUM',
            # 'Office_MoneyMarket_Perc_AUM',
            # 'Office_Other_Perc_AUM',
            # 'Office_TaxPreferred_Perc_AUM',
            # 'MS_ASSET_Perc_529',
            # 'MS_ASSET_Perc_ABC',
            # 'MS_ASSET_Perc_R',
            # 'MS_ASSET_Perc_F',
            # 'MS_ASSET_2017_TAXABLE_BOND_perc',
            # 'MS_ASSET_2017_MUNI_BOND_perc',
            # 'MS_ASSET_2017_US_EQUITY_perc',
            # 'MS_ASSET_2017_ALLOCATION_perc',
            # 'MS_ASSET_2017_INTL_EQUITY_perc',
            #'AUM_LVL',
            # 'MS_SLS_2017_ALLOCATION_perc',
            # 'MS_SLS_2017_INTL_EQUITY_perc',
            # 'MS_SLS_2017_MUNI_BOND_perc',
            # 'MS_SLS_2017_TAXABLE_BOND_perc',
            # 'MS_SLS_2017_US_EQUITY_perc',
            # 'MS_slsRtmt2017_perc',
            # 'MS_slsRetail2017_perc',
            # 'MS_slsIRA2017_perc',
            #'MS_DISTINCT_FUND_CNT_ASSET_2017',
            #'MS_DISTINCT_SHARE_GRP_ASSET_2017',
            #'Joint_Visit_2017',
            #'RPC_Visit_2017',
            #'WLSR2_Visit_2017'
            # DCIO_DCSales_perc
            # DCProp_DCSales_perc
            # DCIO_DCAUM_perc
            # DCProp_DCAUM_perc
            # isAF_Top1_TDF
            # isAF_Top2_TDF
            # isAF_Top1_RK
            # isAF_Top2_RK
            # isAF_Top3_RK
            # CJ_RP_Segment_rank
            # CJ_RP_rating_rank
            'DistShrClass_Cnt',
            'DistFund_Cnt',
            'AUM_level_Cat',
            'WLSR2_Visit_Group',
            'hasSFCalls',
            'hasSFVisit',
            'numPriorFirms_Group',
            'CurrentFirmTenure_Group',
            'IndustryTenure_Group',
            'Age_Group',
            'Office_AF_as_Total_AUM_Group',
            'Office_Allocation_Perc_AUM_Group',
            'Office_Alternative_Perc_AUM_Group',
            'Office_Commodities_Perc_AUM_Group',
            'Office_FixedIncome_Perc_AUM_Group',
            'Office_Equity_Perc_AUM_Group',
            'Office_MoneyMarket_Perc_AUM_Group',
            'Office_Other_Perc_AUM_Group',
            'Office_TaxPreferred_Perc_AUM_Group',
            'MS_ASSET_Perc_529_Group',
            'MS_ASSET_Perc_ABC_Group',
            'MS_ASSET_Perc_R_Group',
            'MS_ASSET_Perc_F_Group',
            'MS_SLS_2017_ALLOCATION_perc_Group',
            'MS_SLS_2017_INTL_EQUITY_perc_Group',
            'MS_SLS_2017_MUNI_BOND_perc_Group',
            'MS_SLS_2017_TAXABLE_BOND_perc_Group',
            'MS_SLS_2017_US_EQUITY_perc_Group',
            'MS_slsRtmt2017_perc_Group',
            'MS_slsRetail2017_perc_Group',
            'MS_slsIRA2017_perc_Group',
            # retirement related:
            'Plan_Sold_Group',
            'MS_IRA_momentum_12mon_Dir',
            'MS_IRA_momentum_24mon_Dir',
            'MS_IRA_momentum_3mon_Dir',
            'MS_IRA_momentum_6mon_Dir',
            'MS_Retail_momentum_12mon_Dir',
            'MS_Retail_momentum_24mon_Dir',
            'MS_Retail_momentum_3mon_Dir',
            'MS_Retail_momentum_6mon_Dir',
            'MS_Retirement_momentum_12mon_Dir',
            'MS_Retirement_momentum_24mon_Dir',
            'MS_Retirement_momentum_3mon_Dir',
            'MS_Retirement_momentum_6mon_Dir',
            'plan_cnt_industry_DC_LT_1M_2017_Group',
            'plan_cnt_industry_DC_1M_10M_2017_Group',
            'plan_cnt_industry_DC_10M_100M_2017_Group',
            'plan_amt_industry_DC_LT_1M_2017_Group',
            'plan_amt_industry_DC_1M_10M_2017_Group',
            'plan_amt_industry_DC_10M_100M_2017_Group',
            'CJ_DC_AUM_2017_Group',
            'CJ_DC_Sales_2017_Group',
            'Bank',
            'Base',
            'Core',
            'Core_Plus',
            'Other',
            'Regional',
            'Strategic',
            'Wirehouses',
            'ASSET_COMPLEXITY2_Group',
            'MS_slsTotal2017_Group',
            'MS_ASSET_2017_Group'
            
)

X_one_hot <- dummy.data.frame(X[,dput(fea_col)])

# UNCOMMENT parent org in feature col set BEFORE you RUN!!
#X_one_hot_wOrg <- dummy.data.frame(X[,dput(fea_col)])

#### Sanity check ####
# shouldn't return anything because all variables are categorized
X_stats = colStats(X_one_hot)
X_stats[X_stats[,2] < 0,]
X_stats[X_stats[,1] > 1,]
names(X_stats[X_stats[,4] == 1,4])

saveRDS(X_one_hot, file = "X_one_hot.rds")
readRDS(file = "X_one_hot.rds")

#### PCA ####

# Iter 1: all records
PCA1 <- princomp(X_one_hot, scale = F, center = F, scores = TRUE)

s1 = summary(PCA1)
cumvar = cumsum(PCA1$sdev^2 / sum(PCA1$sdev^2))
pc.index<-min(which(cumvar>0.8))
plot(cumsum(PCA1$sdev^2 / sum(PCA1$sdev^2)))
abline(v = pc.index, h = cumvar[pc.index], col = "gray60")

write.table(PCA1$loadings,'PCA1_Loading_Class_DC_Momentum_Complexity2.csv',sep = "|")

ldM = abs(PCA1$loadings[,1:pc.index]) > 0.1
idx = apply(ldM,MARGIN = 1,any)
tmp = PCA1$loadings[idx,1:pc.index]
write.table(tmp,'PCA1_Loading_Lgr01.csv',sep = "|")

# PCA1_cluster10 = kmeans(as.data.frame(PCA1$scores), 10)
# PCA1_cluster15 = kmeans(as.data.frame(PCA1$scores), 15)
# PCA1_cluster20 = kmeans(as.data.frame(PCA1$scores), 20)


# PCA1_cluster10_topN = kmeans(as.data.frame(PCA1$scores[,1:pc.index]), 10)
set.seed(42);PCA1_cluster15_topN = kmeans(as.data.frame(PCA1$scores[,1:pc.index]), 15) 
# PCA1_cluster20_topN = kmeans(as.data.frame(PCA1$scores[,1:pc.index]), 20)
# PCA1_cluster25_topN = kmeans(as.data.frame(PCA1$scores[,1:pc.index]), 25)

# 15 clusters

PCA1$PCA1_cluster15_topN = DF$PCA1_cluster15_topN = X_one_hot$PCA1_cluster15_topN = X$PCA1_cluster15_topN = cbind(PCA1_cluster15_topN$cluster)

#### cluster level stats and distance ####
PCA1_cluster15_topN$noeng_dist2eng_Ec = list()
PCA1_cluster15_topN$noeng_dist2eng = list()

for (i in 1:15){
  start_time <- Sys.time()
  print(paste('Processing cluster: ', as.character(i)))
  PCA1_cluster15_topN$covM[[i]] = cov(PCA1$scores[PCA1$PCA1_cluster15_topN == i, 1:pc.index])
  
  eng = DF$CEI_CEI_2017 >= 90 & DF$PCA1_cluster15_topN == i
  noeng = DF$CEI_CEI_2017 == 0 & DF$PCA1_cluster15_topN == i
  print(paste('# Engaged advisor in cluster ', as.character(i), ':', as.character(sum(eng)), '| Non-Engaged :', as.character(sum(noeng))))

  # calculate distances
  eng_Score = PCA1$scores[eng, 1:pc.index]
  rownames(eng_Score) = DF$CJ_AE_SEQ_NUM[eng]
  noeng_Score = PCA1$scores[noeng, 1:pc.index]
  rownames(noeng_Score) = DF$CJ_AE_SEQ_NUM[noeng]

  covMatrix =  PCA1_cluster15_topN$covM[[i]]
 
  # calculate distance of a non-engaged vector to all top engaged vectors
  tmpFn2 = function(v){

    # INPUT: vector (1 x nPC), a nonengager's features
    # DESC: calculate mahalanobis distance of v w.r.t. all vectors in engaged matrix
    # OUTPUT: vector (1 x # rows in engaged matrix)

    V = matrix(rep(v,each=dim(eng_Score)[1]), ncol=dim(eng_Score)[2])
    # mahalanobis distance
    v_dist2M = diag((V - eng_Score) %*% solve(covMatrix) %*% t(V - eng_Score))

    return(v_dist2M)
  }
  
  tmpFn1 = function(v){
    
    # INPUT: vector (1 x nPC), a nonengager's features
    # DESC: calculate mahalanobis distance of v w.r.t. all vectors in engaged matrix
    # OUTPUT: vector (1 x # rows in engaged matrix)
    
    V = matrix(rep(v,each=dim(eng_Score)[1]), ncol=dim(eng_Score)[2])
    # mahalanobis distance
    v_dist2M = diag((V - eng_Score) %*% t(V - eng_Score))
    
    return(v_dist2M)
  }
  
  # create list based on non-engaged matrix
  noeng_M = lapply(seq_len(nrow(noeng_Score)), function(j) noeng_Score[j,])
  
  # for each row in non-engaged matrix, calculate distince with each row in engaged matrix
  noeng_dist2M = do.call(rbind,lapply(noeng_M, tmpFn2))
  noeng_dist2M_Ec = do.call(rbind,lapply(noeng_M, tmpFn1))
  rownames(noeng_dist2M) = rownames(noeng_dist2M_Ec) = DF$CJ_AE_SEQ_NUM[noeng]
  colnames(noeng_dist2M) = colnames(noeng_dist2M_Ec) = DF$CJ_AE_SEQ_NUM[eng]
  rownames(noeng_dist2M_Ec) = DF$CJ_AE_SEQ_NUM[noeng]
  colnames(noeng_dist2M_Ec) = DF$CJ_AE_SEQ_NUM[eng]
  
  PCA1_cluster15_topN$noeng_dist2eng[[i]] = noeng_dist2M
  PCA1_cluster15_topN$noeng_dist2eng_Ec[[i]] = noeng_dist2M_Ec
  print(Sys.time() - start_time)
}

#save(PCA1_cluster15_topN, file = "PCA1_cluster15_topN")

#### distance to center, a measure of how spreadout a cluster is ####
dist2Ctr = function(i){
  # v: center(vector) of a cluster
  v = PCA1_cluster15_topN$centers[i,]
  V = matrix(rep(v,each=sum(PCA1_cluster15_topN$cluster == i)), ncol=length(v))
  
  # M: all points in the cluster
  M = PCA1$scores[PCA1_cluster15_topN$cluster == i,1:pc.index]
  
  # euclidean distance
  v_dist2M_Mah = diag((V - M) %*% solve(PCA1_cluster15_topN$covM[[i]]) %*% t(V - M))
  #v_dist2M = diag((V - M) %*% t(V - M))
  return(quantile(v_dist2M_Mah, seq(0,1,.1)))
}

clusterMahDist2Ctr_distribution = do.call(rbind,lapply(1:15, dist2Ctr))
write.csv(clusterMahDist2Ctr_distribution,'clusterMahDist2Ctr_distribution.csv')

#### get nearest neighbor ####
getNearestPoint = function(v) {
  
  pairs = matrix(0,nrow = 1, ncol = 3)
  j <- which.min(v)
  
  pairs[1,1] = as.integer(names(v)[j])
  pairs[1,2] = v[j]
  pairs[1,3] = mean(v)

  return(pairs)
}

#### flatten the list and create a nearest neighbord matrix ####
PCA1_cluster15_topN$nearestNeighbor_Ec = list()
PCA1_cluster15_topN$nearestNeighbor = list()
for (i in 1:15){
  X_Ec = PCA1_cluster15_topN$noeng_dist2eng_Ec[[i]]
  X_Mah = PCA1_cluster15_topN$noeng_dist2eng[[i]]
  
  X_Ec_list = lapply(seq_len(nrow(X_Ec)), function(j) X_Ec[j,])
  X_list = lapply(seq_len(nrow(X)), function(j) X_Mah[j,])
  
  PCA1_cluster15_topN$nearestNeighbor_Ec[[i]] = cbind(do.call(rbind,lapply(X_Ec_list, getNearestPoint)),rep(i, dim(X_Ec)[1]))
  PCA1_cluster15_topN$nearestNeighbor[[i]] = cbind(do.call(rbind,lapply(X_list, getNearestPoint)),rep(i, dim(X)[1]))
  
  rownames(PCA1_cluster15_topN$nearestNeighbor[[i]]) = 
    rownames(PCA1_cluster15_topN$nearestNeighbor_Ec[[i]]) = rownames(PCA1_cluster15_topN$noeng_dist2eng_Ec[[i]])
  colnames(PCA1_cluster15_topN$nearestNeighbor[[i]]) =
    colnames(PCA1_cluster15_topN$nearestNeighbor_Ec[[i]]) = c('NearestNeighborAEID','Dist2NearestNeighbor','MeanDist2TopEngagers','Cluster')
  
}

ec_dist = do.call(rbind,PCA1_cluster15_topN$nearestNeighbor_Ec)
dist = do.call(rbind,PCA1_cluster15_topN$nearestNeighbor)
write.csv(ec_dist,'PCA1_cluster_15_topN_nearestNeighbor_w_distance_Ec.csv')
write.csv(dist,'PCA1_cluster_15_topN_nearestNeighbor_w_distance.csv')
write.csv(cbind(ec_dist, dist),'NNPairs_ec_mah_dist.csv')

#### examples ####
outCol = c(fea_col,'PCA1_cluster15_topN')
getNearestNeighborAttri = function(i){
  
  v = PCA1_cluster15_topN$nearestNeighbor[[i]][,'Dist2NearestNeighbor']
  xq = quantile(v)
  idx = sapply(xq, function(y)which.min(abs(v - y))) 
  
  res = matrix(NA,nrow = 10, ncol = 2)
  
  colnames(res) = c('CJ_AE_SEQ_NUM','Dist2NearestNeighbor')
  engID = PCA1_cluster15_topN$nearestNeighbor[[i]][idx,'NearestNeighborAEID']
  dist2eng = PCA1_cluster15_topN$nearestNeighbor[[i]][idx,c('Dist2NearestNeighbor')]
  
  res[,1] = c(engID, names(engID))
  res[,2] = c(dist2eng,dist2eng)
  
  res1 = DF[DF$CJ_AE_SEQ_NUM %in% c(engID, names(engID)), 
            c('CJ_AE_SEQ_NUM','CEI_CEI_2017','ASSET_COMPLEXITY2','CJ_DC_Sales_2017','CJ_DC_AUM_2017',
              'MS_Retirement_momentum_3mon.y','MS_Retirement_momentum_6mon.y','MS_Retirement_momentum_12mon.y','MS_Retirement_momentum_24mon.y',
              'MS_Retail_momentum_3mon.y','MS_Retail_momentum_6mon.y','MS_Retail_momentum_12mon.y','MS_Retail_momentum_24mon.y',
              'MS_IRA_momentum_3mon.y','MS_IRA_momentum_6mon.y','MS_IRA_momentum_12mon.y','MS_IRA_momentum_24mon.y','Plan_Sold',
              'MS_ASSET_2017','MS_slsTotal2017')]
  res2 = X[DF$CJ_AE_SEQ_NUM %in% c(engID, names(engID)),dput(outCol)]
  res3 = cbind(res1, res2)
  return(merge(x = res, y = res3, by = "CJ_AE_SEQ_NUM", all.x = TRUE))
}


write.csv(do.call(rbind,lapply(1:15,getNearestNeighborAttri)),'NNPairs_example.csv')



#### some stats ####
pop = matrix(0,nrow = 15, ncol = 3)
for (i in 1:15){
 
  pop[i,1] = sum(DF$CEI_CEI_2017 >= 90 & DF$PCA1_cluster15_topN == i)
  pop[i,2] = sum(DF$CEI_CEI_2017 == 0 & DF$PCA1_cluster15_topN == i)
  pop[i,3] = sum(DF$PCA1_cluster15_topN == i)
}

#### get examples ####
# pIdx = DF$CJ_AE_SEQ_NUM == 6140689 | DF$CJ_AE_SEQ_NUM == 6129818
# View(PCA1$scores[pIdx,1:42])
# View(t(DF[pIdx,]))
# View(t(X_one_hot[pIdx,]))



PCA1_Cluster_Stats = DF %>% group_by(PCA1_cluster15_topN) %>%
  summarise(AF_Asset_2017_median = median(MS_ASSET_2017, na.rm = T),
            AF_Asset_2017_mean = mean(MS_ASSET_2017, na.rm = T),
            AF_Sales_2017_median = median(MS_slsTotal2017, na.rm = T),
            AF_Sales_2017_mean = mean(MS_slsTotal2017, na.rm = T))



by_cluster <- DF %>% group_by(PCA1_cluster15_topN)
by_cluster %>% summarise_each(funs(median(., na.rm = TRUE)))

#write.csv(Cluster1_summary,'PCA1_cluster15_topN_summary_stats.csv')

median2 = function(data){
  return(median(data, na.rm = TRUE))
}



#### Pairwise Distance (no clustering) ####
overallCov = cov(PCA1$scores[, 1:pc.index])

overallEng = DF$CEI_CEI_2017 >= 90 
overallNoeng = DF$CEI_CEI_2017 == 0 

overallEngAEID = DF$CJ_AE_SEQ_NUM[overallEng]
overallNonengAEID = DF$CJ_AE_SEQ_NUM[overallNoeng]

noneng_Score = PCA1$scores[overallNoeng,1:pc.index]
eng_Score = PCA1$scores[overallEng,1:pc.index]

# create list based on engager matrix
bSize = ceiling(sum(overallEng) / 100)
noneng_M = lapply(seq_len(nrow(noneng_Score)), function(j) noneng_Score[j,])

noneng_dist2_eng = list()
for ( i in 1:100){
  print(paste('Processing batch: ', as.character(i)))
  
  eng_Score_batch = eng_Score[(bSize * (i - 1) + 1):min((bSize * i),dim(eng_Score)[1]),]

  dist2Engager = function(v){
    
    V = matrix(rep(v,each=dim(eng_Score_batch)[1]), ncol=dim(eng_Score_batch)[2])
    v_dist2M = diag((V - eng_Score_batch) %*% overallcov_inv %*% t(V - eng_Score_batch))
    
    return(v_dist2M)
  }
  
  noneng_dist2_eng[[i]] = do.call(rbind,lapply(noneng_M, dist2Engager))
  
  rownames(noneng_dist2_eng[[i]]) = overallNonengAEID
  colnames(noneng_dist2_eng[[i]]) = overallEngAEID[(bSize * (i - 1) + 1):min((bSize * i),dim(eng_Score)[1])]
  
  
}

#saveRDS(noneng_dist2_eng, file = "noneng_dist2_eng")

findNNs = function(distM){
  
  M = matrix(0,nrow = dim(distM)[2], ncol = 3)
  NN1Idx = apply(X = distM,FUN = which.min,MARGIN = 2)
  M[,3] = diag(distM[NN1Idx,])
  M[,2] = as.integer(rownames(distM[NN1Idx,]))
  M[,1] = as.integer(colnames(distM))
  return(M)
}

output = do.call(rbind, lapply(noneng_dist2_eng, findNNs))
colnames(output) = c('AEID_TopEngager','AEID_NonEngager','Distance')
output[,4] = 1:dim(output)[1]
engagerPaired1 = DF[DF$CJ_AE_SEQ_NUM %in% c(output[,1], output[,2]), 
          c('CJ_AE_SEQ_NUM','CEI_CEI_2017','ASSET_COMPLEXITY2','CJ_DC_Sales_2017','CJ_DC_AUM_2017',
            'MS_Retirement_momentum_3mon.y','MS_Retirement_momentum_6mon.y','MS_Retirement_momentum_12mon.y','MS_Retirement_momentum_24mon.y',
            'MS_Retail_momentum_3mon.y','MS_Retail_momentum_6mon.y','MS_Retail_momentum_12mon.y','MS_Retail_momentum_24mon.y',
            'MS_IRA_momentum_3mon.y','MS_IRA_momentum_6mon.y','MS_IRA_momentum_12mon.y','MS_IRA_momentum_24mon.y','Plan_Sold',
            'MS_ASSET_2017','MS_slsTotal2017')]
engagerPaired2 = X[DF$CJ_AE_SEQ_NUM %in% c(output[,1], output[,2]),dput(fea_col)]

engagerPaired3 = cbind(engagerPaired1, engagerPaired2)

DIM = matrix(cbind(c(output[,1], output[,2]),rep(1:dim(output)[1],2)),nrow = length(c(output[,1], output[,2])),2)

colnames(DIM) = c('CJ_AE_SEQ_NUM','PairID')

finalOut = merge(x = DIM, y = engagerPaired3, by = "CJ_AE_SEQ_NUM", all.x = TRUE)
write.csv(finalOut,'eng2NonEngPair_noCluster.csv')
write.csv(output, 'eng2NonEngPair_noCluster_List.csv')


