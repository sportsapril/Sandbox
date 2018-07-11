#rm(list=ls())
library(pROC)
#options(digits=6)
##########DEFINE FUNCTIONS##########
fn_mean <- function(x){
  return(mean(x, na.rm=TRUE) )
}

fn_sd <- function(x){
  return(sd(x, na.rm=TRUE) )
}

library(randomForest)
library(ROCR)
#library(reprtree)
########## LOAD DATA ############
setwd("/users/teams/nad/conapx/Lincoln/")
LincolnList <- read.csv("Loyalist_Lost_Binary.csv", header = T, na.strings = c("","NA"))
DiscoveryList <- read.csv("Discovery_FA_Dataset_Processed.csv", header = T, na.strings = c("","NA"))
factor_col = c('Segmentation.CY','FIRM','Firm_2','Zip','City','State','BFH_AFFI_TOPSELLERS','TITLE_GROUP','TITLECATEGORIES',
               'PriorFirm1_Name','Firm_Affiliated','isBFHAFFI','Seg','PriorFirm_2','hasMPCES','hasDigitalEngagemen')
factor_col_disc = c('isBFHAFFI','hasMPCES','hasDigitalEngagemen')

########## CONVERT TO FACTORS ############
F_LincolnList = LincolnList

for (f in factor_col){
  F_LincolnList[[f]] = as.factor(LincolnList[[f]])
}


X = setdiff(colnames(F_LincolnList), c('Segmentation.CY','CRD','X2017.Sales','X2018.Sales','AUM','FIRM','BDFirmName'
                                       ,'MODELPORTFOLIO_CES','PORTFOLIO_TOOL_CES','PORTFOLIO_EMAIL_CES','PORTFOLIO_LIT_CES'
                                       ,'VolatilityContent','Sessions','Tax_Others','RI','AP'))
X_fac = c('Firm_2','City','State','Zip','PriorFirm1_Name','PriorFirm_2','Firm_Affiliated','isBFHAFFI','BFH_AFFI_TOPSELLERS','TITLE_GROUP','TITLECATEGORIES','hasMPCES','hasDigitalEngagemen')
X_cont = setdiff(X,c(X_fac,'Seg'))

########## BINNING ############
# For factors, using woe.binning
# ACCEPTED RANGE:
# min.perc.total: [0.01, 0.2]
# min.perc.class: [0,0.2]
min_perc_total = seq(from = 0.05, to = 0.2, by = 0.01)
min_perc_class = seq(from = 0.05, to = 0.2, by = 0.01)
iv_grid = matrix(data = NA,nrow = length(min_perc_total),ncol = length(min_perc_class))
for (t in 1:length(min_perc_total)){
  for (c in 1:length(min_perc_class)){
    iv_grid[t,c] = woe.binning(F_LincolnList, 'Segmentation.CY', 'TITLE_GROUP',
                       min.perc.total = min_perc_total[t], min.perc.class=min_perc_class[c], 
                       stop.limit=0.01, event.class = 'Loyalist')[[3]]
    
  }
}
max(iv_grid)
min(iv_grid)
which(iv_grid == max(iv_grid))[1]

woe(Data = F_LincolnList, 'PriorFirm_2', FALSE, 'Seg', 10, Bad = 0, Good = 1)
tmp = woe.binning(F_LincolnList, 'Segmentation.CY', 'PriorFirm_2', min.perc.total = 0.01, min.perc.class=0.01, stop.limit=0.01, event.class = 'Loyalist')
print(tmp[[2]])
print(unique(tmp[[2]][,'Group.2']))
tmp[[3]]

########## BINNING CONTINUOUS ############
# For continuous values, use woe
bin_n = c(4:15)
iv_grid = matrix(NA,nrow = 1,ncol = length(bin_n))
for (b in 1:length(bin_n)){
    tmp = woe(Data = F_LincolnList, 'OfficeBook_Shares_EQ_Pcnt', TRUE, 'Seg', bin_n[b], Bad = 0, Good = 1)
    iv_grid[b] = sum(tmp$IV)

}


w = woe(Data = F_LincolnList, 'AF_Shares_Pcnt_of_OfficeBook', TRUE, 'Seg',7, Bad = 0, Good = 1)
sum(w$IV)
print(w)

woe.binning(F_LincolnList, 'Segmentation.CY', 'AF_AUM_Pcnt_of_OfficeBook', min.perc.total = 0.02, min.perc.class= 0.02, stop.limit=0.1, event.class = 'Loyalist')[[3]]

########## FINAL BINNING ############
fac1 = woe.binning(F_LincolnList, 'Segmentation.CY', 'State', min.perc.total = 0.02, min.perc.class= 0.02, stop.limit=0.01, event.class = 'Loyalist')
fac2 = woe.binning(F_LincolnList, 'Segmentation.CY', 'Firm_2', min.perc.total = 0.01, min.perc.class= 0.001, stop.limit=0.005, event.class = 'Loyalist')
woe(Data = F_LincolnList, 'Firm_Affiliated_num', TRUE, 'Seg', 4, Bad = 0, Good = 1)  #Just use the raw
woe(Data = F_LincolnList, 'NumYr_FA', TRUE, 'Seg', 13, Bad = 0, Good = 1) #Just use the raw
woe(Data = F_LincolnList, 'NumYr_Firm', TRUE, 'Seg', 14, Bad = 0, Good = 1) #use raw
woe(Data = F_LincolnList, 'Munis_SalesP12', TRUE, 'Seg',4, Bad = 0, Good = 1) # 0, (0, 16239.55], >16239.55
woe(Data = F_LincolnList, 'Taxable_SalesP12', TRUE, 'Seg',8, Bad = 0, Good = 1) # 0, (0, 5995], >...
woe(Data = F_LincolnList, 'OfficeBook_Shares_FI_Pcnt', TRUE, 'Seg', 11, Bad = 0, Good = 1) #use raw
woe(Data = F_LincolnList, 'Sales_P12Mo', TRUE, 'Seg', 17, Bad = 0, Good = 1) #Just use the raw

# [overfitting]: ZIP, City, PriorFirm_2 # don't use
# [no binning]: isBFHAFFI, hasMPCES, hasDigitalEngagemen
# [low IV]: TITLE related
# [use raw]: age, FI_SalesP12, Firm_Affiliated_num, NumYr_FA, NumYr_Firm, Sales_P12Mo
# [don't use]: Firm (training set firms is just a subset of all)



########## DEPLOY BINNING ############
F_LincolnList_binned <- woe.binning.deploy(F_LincolnList, fac1, add.woe.or.dum.var = 'woe')    
F_LincolnList_binned <- woe.binning.deploy(F_LincolnList_binned, fac2, add.woe.or.dum.var = 'woe')
F_LincolnList_binned[,'Firm_Affiliated_Cat'] <- as.factor(ifelse(F_LincolnList_binned[,'Firm_Affiliated_num'] == 1, "1", 
                                                                ifelse(F_LincolnList_binned[,'Firm_Affiliated_num'] <= 3, "2-3","3+")))
F_LincolnList_binned[,'Munis_SalesP12_Cat'] <- as.factor(ifelse(F_LincolnList_binned[,'Munis_SalesP12'] <= 16240, "L","H"))
F_LincolnList_binned[,'Taxable_SalesP12_Cat'] <- as.factor(ifelse(F_LincolnList_binned[,'Taxable_SalesP12'] == 0, "None",
                                                                  ifelse(F_LincolnList_binned[,'Taxable_SalesP12'] <= 6000, "L", "H")))

########## normalize the dataset ############
num_col = c('AF_AUM_Pcnt_of_OfficeBook',
            'AF_Shares_Pcnt_of_OfficeBook',
            'OfficeBook_Shares_FI_Pcnt',
            'OfficeBook_Shares_EQ_Pcnt',
            'OfficeBook_AUM_FI_Pcnt',
            'OfficeBook_AUM_EQ_Pcnt',
            'NumYr_FA',
            'NumYr_Firm',
            'FI_SalesP12',
            'Sales_P12Mo')
df_norm_raw = scale(F_LincolnList_binned[,num_col], center = TRUE, scale = TRUE)
mean_group = apply(X = F_LincolnList_binned[,num_col], MARGIN = 2, fn_mean)
sd_group = apply(X = F_LincolnList_binned[,num_col], MARGIN = 2, fn_sd)

########## append categorical variables ############
df_norm = cbind(F_LincolnList_binned[,c('Segmentation.CY',
                                        'Seg',
                               'TITLE_GROUP',
                               'isBFHAFFI',
                               'hasMPCES',
                               'hasDigitalEngagemen',
                               'PORTFOLIO_TOOL_CES',
                               'PORTFOLIO_EMAIL_CES',
                               'PORTFOLIO_LIT_CES',
                               'State.binned',
                               #'Firm_2.binned',
                               'Firm_Affiliated_Cat',
                               'Munis_SalesP12_Cat',
                               'Taxable_SalesP12_Cat',
                               'CRD')], df_norm_raw)
df_norm2 = cbind(F_LincolnList_binned[,c('Segmentation.CY',
                                        'Seg',
                                        'TITLE_GROUP',
                                        'isBFHAFFI',
                                        'hasMPCES',
                                        'hasDigitalEngagemen',
                                        'PORTFOLIO_TOOL_CES',
                                        'PORTFOLIO_EMAIL_CES',
                                        'PORTFOLIO_LIT_CES',
                                        'State.binned',
                                        #'Firm_2.binned',
                                        'Firm_Affiliated_Cat',
                                        'Munis_SalesP12_Cat',
                                        'Taxable_SalesP12_Cat',
                                        'CRD')], df_norm_raw)

df_norm = as.data.frame(df_norm)
n = dim(df_norm)[1]
k = dim(df_norm)[2]

#df_norm[,'hasMPCES'] = as.factor(df_norm[,'hasMPCES'])
#df_norm[,'hasDigitalEngagemen'] = as.factor(df_norm[,'hasDigitalEngagemen'])

########## split into training and testing ############
# over sample the minority class
df_norm_os = ovun.sample(Segmentation.CY ~., data = df_norm, method = 'both', N = n, seed = 1)$data

# with CRD for later checks
df_norm_os2 = ovun.sample(Segmentation.CY ~., data = df_norm2, method = 'both', N = n, seed = 1)$data
set.seed(123);train_ind = sample(n, size = floor(0.8 * n))
train_df = df_norm_os2[train_ind,]
test_df = df_norm_os2[-train_ind,]



#options(digits=2)
#train_df_os = train_df
#train_df_os = ovun.sample(Segmentation.CY ~., data = train_df, method = 'both', N = floor(0.8 * n))$data

# remove Seg and CRD so that model can run
train_df_os2 = train_df[,setdiff(colnames(train_df), c('Seg','CRD'))]

# test if we can drop some variables that has NA
# 1. Age


#rf1 = randomForest(Segmentation.CY ~ ., as.data.frame(train_df_os2), ntree = 500, importance = TRUE)
rf1 = randomForest(Segmentation.CY ~ ., as.data.frame(train_df_os2), ntree = 500, importance = TRUE)
varImpPlot(rf1)
#reprtree:::plot.getTree(rforest = rf1, k = 2, depth = 6)
#plot(rf1, log="y")

###################PLOT###################
# DON'T USE YET
# rename variables so that it displays better
#format(round(x, 2), nsmall = 2)
tree <- randomForest::getTree(rf1, 
                              k = 2, 
                              labelVar = TRUE) %>%
  tibble::rownames_to_column() %>%
  # make leaf split points to NA, so the 0s won't get plotted
  mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))


graph_frame <- data.frame(from = rep(rownames(tree), 2),
                          to = c(tree$`left daughter`, tree$`right daughter`))

graph <- graph_from_data_frame(graph_frame) %>%  delete_vertices("0")

V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
V(graph)$leaf_label <- as.character(tree$prediction)
V(graph)$split <- as.character(round(tree$`split point`, digits = 2))

plot <- ggraph(graph, 'dendrogram') + 
  theme_bw() +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
  geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
  geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                  repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 18))


#################PREDICT on TRAIN ################
pred_train = predict(rf1,train_df_os2, type = 'prob')
pred_train_response = predict(rf1,train_df_os2, type = 'response')
pred2_train = prediction(as.vector(pred_train[,'Loyalist']),as.vector(train_df_os2[,'Segmentation.CY'] == 'Loyalist'))
perf_train <- performance( pred2_train, "tpr", "fpr" )
plot(perf_train)

#################PREDICT on TEST and new #########
pred = predict(rf1,test_df, type = 'prob')
pred_response = predict(rf1,test_df, type = 'response')
tb = table(observed = test_df[,'Segmentation.CY'], predicted = pred_response)
tb['Loyalist','Loyalist']/sum(tb[,'Loyalist']) #precision

# plot ROC 
pred2 = prediction(as.vector(pred[,'Loyalist']),as.vector(test_df[,'Seg']))
perf <- performance( pred2, "tpr", "fpr")
plot(perf)

write.table(as.vector(perf@x.values[[1]]),'perf_x_values.csv')
write.table(as.vector(perf@y.values[[1]]),'perf_y_values.csv')

########## real prediction ############
F_DiscoveryList = DiscoveryList

for (f in factor_col_disc){
  F_DiscoveryList[[f]] = as.factor(F_DiscoveryList[[f]])
}

pred_col = setdiff(colnames(test_df),c('Segmentation.CY','Seg','Age'))
disc_norm_raw = scale(F_DiscoveryList[,num_col], center = TRUE, scale = TRUE)
mm = matrix(rep(mean_group, each = dim(F_DiscoveryList)[1], times = 1),nrow = dim(F_DiscoveryList)[1])
ss = matrix(rep(sd_group, each = dim(F_DiscoveryList)[1], times = 1),nrow = dim(F_DiscoveryList)[1])
disc_norm_raw = (F_DiscoveryList[,num_col] - mm) / ss
F_DiscoveryList[,'Firm_Affiliated_Cat'] = as.factor(ifelse(F_DiscoveryList[,'Firm_Affiliated_num'] == 1, "1", 
                                                           ifelse(F_DiscoveryList[,'Firm_Affiliated_num'] <= 3, "2-3","3+")))
F_DiscoveryList[,'Munis_SalesP12_Cat'] <- as.factor(ifelse(F_DiscoveryList[,'Munis_SalesP12'] <= 16240, "L","H"))
F_DiscoveryList[,'Taxable_SalesP12_Cat'] <- as.factor(ifelse(F_DiscoveryList[,'Taxable_SalesP12'] == 0, "None",
                                                             ifelse(F_DiscoveryList[,'Taxable_SalesP12'] <= 6000, "L", "H")))



F_DiscoveryList_norm = cbind(F_DiscoveryList[,setdiff(pred_col,colnames(disc_norm_raw))],
                             disc_norm_raw)

levels(F_DiscoveryList_norm[,'State.binned']) = levels(train_df_os2[,'State.binned'])

pred_response_disc = predict(rf1,F_DiscoveryList_norm, type = 'response')

Disc_FA_Loyalist = DiscoveryList[pred_response_disc == 'Loyalist' & !is.na(pred_response_disc),]
table(is.na(Disc_FA_Loyalist[,'CRD'] ))

write.table(Disc_FA_Loyalist[,setdiff(colnames(Disc_FA_Loyalist),c('Firm_2','FIRM','Street_Address'))], "Discovery_Predicted_Loyalist_0604.csv", sep="\t", row.names=FALSE)

#rf1 = randomForest(x = x, y = y, xtest = xtest, ytest = ytest, ntree = 500, importance = TRUE, keep.forest = TRUE, na.omit(c(NA)))

########## TRY: LOGISTIC ############
LogitMod <- glm(Seg ~ TITLE_GROUP
                                  + isBFHAFFI
                                  + hasMPCES
                                  + NumYr_Firm
                                  + NumYr_FA
                                  + hasDigitalEngagemen
                                  + State.binned
                                  + Firm_2.binned
                                  + Age
                                  + MODELPORTFOLIO_CES 
                                  + FI_SalesP12 
                                  + Munis_SalesP12_Cat
                                  + Taxable_SalesP12_Cat 
                                  + Sales_P12Mo
                                  + AF_Shares_Pcnt_of_OfficeBook 
                                  + OfficeBook_Shares_FI_Pcnt 
                                  + OfficeBook_Shares_EQ_Pcnt
                                  + AF_AUM_Pcnt_of_OfficeBook
                                  + OfficeBook_AUM_FI_Pcnt
                                  + OfficeBook_AUM_EQ_Pcnt
                , data=train_df_os, family=binomial(link="logit"))
summary(LogitMod)
rsq(LogitMod, adj=TRUE)




########## T-Test on Group Average (numeric value) ############
ttest_col = c(X_cont,c('MODELPORTFOLIO_CES','PORTFOLIO_TOOL_CES','PORTFOLIO_EMAIL_CES','PORTFOLIO_LIT_CES','VolatilityContent',
                       'Sessions','Tax_Others','RI','AP'))
ind = LincolnList[,'Seg'] == 1
pVal = matrix(NA,nrow = length(ttest_col), ncol = 1)
for (i in 1:length(ttest_col)){
  pVal[i] = t.test(LincolnList[ind,ttest_col[i]],LincolnList[ind == FALSE,ttest_col[i]])$p.value
}
ttest_col[pVal <=0.05]
t.test(LincolnList[ind,'Sales_P12Mo'],LincolnList[ind == FALSE,'Sales_P12Mo'])
t.test(LincolnList[ind,'Firm_Affiliated_num'],LincolnList[ind == FALSE,'Firm_Affiliated_num'])
t.test(LincolnList[ind,'MODELPORTFOLIO_CES'],LincolnList[ind == FALSE,'MODELPORTFOLIO_CES'])
t.test(LincolnList[ind,'PORTFOLIO_LIT_CES'],LincolnList[ind == FALSE,'PORTFOLIO_LIT_CES'])
t.test(LincolnList[ind,'Tax_Others'],LincolnList[ind == FALSE,'Tax_Others'])

########## Prop-Test on by Categorical Vars (State) ############
#fac1 = woe.binning(F_LincolnList, 'Segmentation.CY', 'State', min.perc.total = 0.02, min.perc.class= 0.02, stop.limit=0.01, event.class = 'Loyalist')
#fac2 = woe.binning(F_LincolnList, 'Segmentation.CY', 'Firm_2', min.perc.total = 0.01, min.perc.class= 0.001, stop.limit=0.005, event.class = 'Loyalist')

# test one vs. rest
u_state_gp = unique(fac1[[2]][,'Group.2'])
pVal_state = matrix(NA,nrow = length(u_state_gp), ncol = 1)
for (i in 1:length(u_state_gp)){
  # find the states belonging to this state group
  tmp_state = fac1[[2]][fac1[[2]][,'Group.2'] == u_state_gp[i],'Group.1']
  x = NULL
  for (j in 1:length(tmp_state)){
    x = c(x,which(LincolnList[,'State'] == as.character(tmp_state[j])))
  }
  
  tmpTB = matrix(NA,2,2)
  tmpTB[1,1] = sum(LincolnList[x,'Seg'])
  tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
  tmpTB[2,1] = sum(LincolnList[-x,'Seg'])
  tmpTB[2,2] = sum(LincolnList[-x,'Seg'] == 0)
  colnames(tmpTB) = c('Loyalist','Lost')
  rownames(tmpTB) = c('Group 1', 'Others')
  pVal_state[i] = prop.test(tmpTB)$p.value
}

sig_u_state_gp = u_state_gp[pVal_state <= 0.05]
pVal_state = matrix(NA,nrow = length(sig_u_state_gp), ncol = length(sig_u_state_gp))
#for (i in c(1)){
for (i in 1:length(sig_u_state_gp)){
  # find the states belonging to this state group
  tmp_state = fac1[[2]][fac1[[2]][,'Group.2'] == sig_u_state_gp[i],'Group.1']
  print(tmp_state)
  x = NULL
  for (z in 1:length(tmp_state)){
    x = c(x,which(LincolnList[,'State'] == as.character(tmp_state[z])))
  }
  
  #for (j in c(3)){
  for (j in min(i+1, length(sig_u_state_gp)):length(sig_u_state_gp)){
    tmp_state2 = fac1[[2]][fac1[[2]][,'Group.2'] == sig_u_state_gp[j],'Group.1']
    print(tmp_state2)
    y = NULL
    for (w in 1:length(tmp_state2)){
      y = c(y,which(LincolnList[,'State'] == as.character(tmp_state2[w])))
    }
    tmpTB = matrix(NA,2,2)
    tmpTB[1,1] = sum(LincolnList[x,'Seg'])
    tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
    tmpTB[2,1] = sum(LincolnList[y,'Seg'])
    tmpTB[2,2] = sum(LincolnList[y,'Seg'] == 0)
    colnames(tmpTB) = c('Loyalist','Lost')
    rownames(tmpTB) = c('Group 1', 'Group 2')
    pVal_state[i,j] = prop.test(tmpTB)$p.value
  }
}
# group the first three into one, and the last two into another ones
tmp_state = fac1[[2]][fac1[[2]][,'Group.2'] ==  sig_u_state_gp[1] | fac1[[2]][,'Group.2'] ==  sig_u_state_gp[2] | fac1[[2]][,'Group.2'] ==  sig_u_state_gp[3],'Group.1']
x = NULL
for (z in 1:length(tmp_state)){
  x = c(x,which(LincolnList[,'State'] == as.character(tmp_state[z])))
}

tmp_state = fac1[[2]][fac1[[2]][,'Group.2'] ==  sig_u_state_gp[4] | fac1[[2]][,'Group.2'] ==  sig_u_state_gp[5],'Group.1']
y = NULL
for (z in 1:length(tmp_state)){
  y = c(y,which(LincolnList[,'State'] == as.character(tmp_state[z])))
}
tmpTB = matrix(NA,2,2)
tmpTB[1,1] = sum(LincolnList[x,'Seg'])
tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
tmpTB[2,1] = sum(LincolnList[y,'Seg'])
tmpTB[2,2] = sum(LincolnList[y,'Seg'] == 0)
colnames(tmpTB) = c('Loyalist','Lost')
rownames(tmpTB) = c('Group 1', 'Group 2')
prop.test(tmpTB)$p.value

write.table(unique(LincolnList[x,'State']), "tmp_state_1.csv", sep="\t", row.names=FALSE)
write.table(unique(LincolnList[y,'State']), "tmp_state_2.csv", sep="\t", row.names=FALSE)


########## Prop-Test on by Categorical Vars (Firm) ############
# test one vs. rest
u_firm_gp = unique(fac2[[2]][,'Group.2'])
pVal_firm = matrix(NA,nrow = length(u_firm_gp), ncol = 1)
for (i in 1:length(u_firm_gp)){
  # find the states belonging to this state group
  tmp_state = fac2[[2]][fac2[[2]][,'Group.2'] == u_firm_gp[i],'Group.1']
  x = NULL
  for (j in 1:length(tmp_state)){
    x = c(x,which(LincolnList[,'Firm_2'] == as.character(tmp_state[j])))
  }
  
  tmpTB = matrix(NA,2,2)
  tmpTB[1,1] = sum(LincolnList[x,'Seg'])
  tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
  tmpTB[2,1] = sum(LincolnList[-x,'Seg'])
  tmpTB[2,2] = sum(LincolnList[-x,'Seg'] == 0)
  colnames(tmpTB) = c('Loyalist','Lost')
  rownames(tmpTB) = c('Group 1', 'Others')
  pVal_firm[i] = prop.test(tmpTB)$p.value
}

sig_u_firm_gp = u_firm_gp[pVal_firm <= 0.05]
pVal_firm = matrix(NA,nrow = length(sig_u_firm_gp), ncol = length(sig_u_firm_gp))
# pair-wise prop-test
#for (i in c(1)){
for (i in 1:length(sig_u_firm_gp)){
  # find the states belonging to this state group
  tmp_state = fac2[[2]][fac2[[2]][,'Group.2'] == sig_u_firm_gp[i],'Group.1']
  print(tmp_state)
  x = NULL
  for (z in 1:length(tmp_state)){
    x = c(x,which(LincolnList[,'Firm_2'] == as.character(tmp_state[z])))
  }
  
  #for (j in c(3)){
  for (j in min(i+1, length(sig_u_firm_gp)):length(sig_u_firm_gp)){
    tmp_state2 = fac2[[2]][fac2[[2]][,'Group.2'] == sig_u_firm_gp[j],'Group.1']
    print(tmp_state2)
    y = NULL
    for (w in 1:length(tmp_state2)){
      y = c(y,which(LincolnList[,'Firm_2'] == as.character(tmp_state2[w])))
    }
    tmpTB = matrix(NA,2,2)
    tmpTB[1,1] = sum(LincolnList[x,'Seg'])
    tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
    tmpTB[2,1] = sum(LincolnList[y,'Seg'])
    tmpTB[2,2] = sum(LincolnList[y,'Seg'] == 0)
    colnames(tmpTB) = c('Loyalist','Lost')
    rownames(tmpTB) = c('Group 1', 'Group 2')
    pVal_firm[i,j] = prop.test(tmpTB)$p.value
  }
}

# further group 1,2,3 into one group, 4 into a second, and 5 into a third
tmp_state = fac2[[2]][fac2[[2]][,'Group.2'] ==  sig_u_firm_gp[1] | fac2[[2]][,'Group.2'] ==  sig_u_firm_gp[2] | fac2[[2]][,'Group.2'] ==  sig_u_firm_gp[3],'Group.1']
x = NULL
for (z in 1:length(tmp_state)){
  x = c(x,which(LincolnList[,'Firm_2'] == as.character(tmp_state[z])))
}

tmp_state = fac2[[2]][fac2[[2]][,'Group.2'] ==  sig_u_firm_gp[4],'Group.1']
y = NULL
for (z in 1:length(tmp_state)){
  y = c(y,which(LincolnList[,'Firm_2'] == as.character(tmp_state[z])))
}

tmp_state = fac2[[2]][fac2[[2]][,'Group.2'] ==  sig_u_firm_gp[5],'Group.1']
w = NULL
for (z in 1:length(tmp_state)){
  w = c(w,which(LincolnList[,'Firm_2'] == as.character(tmp_state[z])))
}

tmpTB = matrix(NA,2,2)
tmpTB[1,1] = sum(LincolnList[x,'Seg'])
tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
tmpTB[2,1] = sum(LincolnList[y,'Seg'])
tmpTB[2,2] = sum(LincolnList[y,'Seg'] == 0)
colnames(tmpTB) = c('Loyalist','Lost')
rownames(tmpTB) = c('Group 1', 'Group 2')
prop.test(tmpTB)$p.value

tmpTB = matrix(NA,2,2)
tmpTB[1,1] = sum(LincolnList[x,'Seg'])
tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
tmpTB[2,1] = sum(LincolnList[w,'Seg'])
tmpTB[2,2] = sum(LincolnList[w,'Seg'] == 0)
colnames(tmpTB) = c('Loyalist','Lost')
rownames(tmpTB) = c('Group 1', 'Group 2')
prop.test(tmpTB)$p.value

tmpTB = matrix(NA,2,2)
tmpTB[1,1] = sum(LincolnList[y,'Seg'])
tmpTB[1,2] = sum(LincolnList[y,'Seg'] == 0)
tmpTB[2,1] = sum(LincolnList[w,'Seg'])
tmpTB[2,2] = sum(LincolnList[w,'Seg'] == 0)
colnames(tmpTB) = c('Loyalist','Lost')
rownames(tmpTB) = c('Group 1', 'Group 2')
prop.test(tmpTB)$p.value

tmpTB = matrix(NA,3,2)
tmpTB[1,1] = sum(LincolnList[x,'Seg'])
tmpTB[1,2] = sum(LincolnList[x,'Seg'] == 0)
tmpTB[2,1] = sum(LincolnList[y,'Seg'])
tmpTB[2,2] = sum(LincolnList[y,'Seg'] == 0)
tmpTB[3,1] = sum(LincolnList[w,'Seg'])
tmpTB[3,2] = sum(LincolnList[w,'Seg'] == 0)
colnames(tmpTB) = c('Loyalist','Lost')
rownames(tmpTB) = c('Group 1', 'Group 2','Group 3')

write.table(unique(LincolnList[x,'Firm_2']), "tmp_x.csv", sep="\t", row.names=FALSE)
write.table(unique(LincolnList[y,'Firm_2']), "tmp_y.csv", sep="\t", row.names=FALSE)
write.table(unique(LincolnList[w,'Firm_2']), "tmp_w.csv", sep="\t", row.names=FALSE)


write.table(as.vector(fac2[[2]][fac2[[2]][,'Group.2'] == 'misc. level pos.','Group.1']), "misc_level_pos.csv", sep="\t", row.names=FALSE)
write.table(as.vector(fac2[[2]][fac2[[2]][,'Group.2'] == 'misc. level neg.','Group.1']), "misc_level_neg.csv", sep="\t", row.names=FALSE)
write.table(as.matrix(fac1[[2]]), "state_bin.csv", sep="\t", row.names=FALSE)


