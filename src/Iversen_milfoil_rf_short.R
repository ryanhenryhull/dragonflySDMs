library(sf)
library(ranger)
library(rcompanion)
library(kernelshap)
library(shapviz)
library(caret)
library(ggplot2)
library(viridis)
library(gridExtra)
library(dplyr)
library(lessR)
library(visreg)
library(randomForest)

qc_map <- read_sf(file.choose())##should be QC_data_updated.gpkg file

a

qc_map<-qc_map[-which(is.na(qc_map$bicarb_mean)),]#remove areas that has no bicarbonate information


####generate prediction models based on QC milfoil data (these will produce different delineations of the realized niche of the species)

#the models should have a provincial focus and be trained and evaluated on the QC landscape via a balanced sampling design.

#the code below sample pseudo absence in QC and presence observations based on QC data.


#split into milfoil and non milfoil sites

milfoil_qc<-qc_map[which(qc_map$"MyrSpic"==1),]

no_milfoil<-qc_map[-which(qc_map$"MyrSpic"==1),]

###create pseudo-absence data selected using gbif weights and combine into one data frame
set.seed(9345)

#the weighted sampling can be done using all watersheds or only watersheds that contains some macrophyte info. 
no_milfoil$prob<-no_milfoil$GBIF_Species/sum(no_milfoil$GBIF_Species)

# all watersheeds: no_milfoil$prob<-(no_milfoil$GBIF_Species+1)/(sum(no_milfoil$GBIF_Species+1))

pseudo<-sample(1:nrow(no_milfoil),size=nrow(milfoil_qc), prob=no_milfoil$prob)

rf_dat<-as.data.frame(rbind(no_milfoil[pseudo,-ncol(no_milfoil)], milfoil_qc))

###Random forest predictions

#####first split the data into a training and test data set
set.seed(12)

## 75% of the sample size

smp_size <- floor(0.75 * nrow(rf_dat))

## split the dataset based on random row selection
train_ind <- sample(seq_len(nrow(rf_dat)), size = smp_size)

train <- rf_dat[train_ind, ]
test <- rf_dat[-train_ind, ]


train_qc<-train

#using this you can develop RF models via the randomForest or ranger functions.

# Here are models with default settings for both fucntions (notice that most of the variable names are hydroatlas referneces).

fit_m1 <- randomForest(factor(MyrSpic)~dis_m3_pyr+dis_m3_pmx+riv_tc_ssu +riv_tc_usu+rev_mc_usu+ lkv_mc_usu+ele_mt_sav+tmp_dc_syr+ tmp_dc_smx+pre_mm_syr+
                        for_pc_sse+for_pc_use+crp_pc_sse+crp_pc_use+pst_pc_sse+pst_pc_use +ppd_pk_sav+ppd_pk_uav+urb_pc_sse+urb_pc_use+rdd_mk_sav+rdd_mk_uav+wet_extent+bicarb_mean,
                      data = train_qc)

fit_m2 <- ranger(factor(MyrSpic)~dis_m3_pyr+dis_m3_pmx+riv_tc_ssu +riv_tc_usu+rev_mc_usu+ lkv_mc_usu+ele_mt_sav+tmp_dc_syr+ tmp_dc_smx+pre_mm_syr+
                for_pc_sse+for_pc_use+crp_pc_sse+crp_pc_use+pst_pc_sse+pst_pc_use +ppd_pk_sav+ppd_pk_uav+urb_pc_sse+urb_pc_use+rdd_mk_sav+rdd_mk_uav+wet_extent+bicarb_mean,
              data = train_qc,importance = "impurity",)


#from these we can estimate the accuracy using a binary classification split at 0.5
#this should be based on the test data
predict(fit_m1,test)
 #or
predict(fit_m2,test)$predictions
#from this we can also see that the two models are identical:
pred$predictions==predict(fit_m1,test)

#accuarcy can be estimated as the proportion of correct classifications:
sum(pred$predictions==test$MyrSpic)/length(test$MyrSpic)

#false negative rate
sum(as.numeric(pred$predictions[which(test$MyrSpic==1)])-1)/length(which(test$MyrSpic==1))

#false positive rate
sum(as.numeric(pred$predictions[which(test$MyrSpic==0)])-1)/length(which(test$MyrSpic==0))


####you can tune the model by comparing performance across compare a range of node size and split rule values

tgrid <- expand.grid(
  mtry = c(2,3,4,5,15,20),
  splitrule = "gini",
  min.node.size = c(5,7,10)
)


train(factor(MyrSpic)~dis_m3_pyr+dis_m3_pmx+riv_tc_ssu +riv_tc_usu+rev_mc_usu+ lkv_mc_usu+ele_mt_sav+tmp_dc_syr+ tmp_dc_smx+pre_mm_syr+
        for_pc_sse+for_pc_use+crp_pc_sse+crp_pc_use+pst_pc_sse+pst_pc_use +ppd_pk_sav+ppd_pk_uav+urb_pc_sse+urb_pc_use+rdd_mk_sav+rdd_mk_uav+wet_extent+bicarb_mean,
      data = train_qc,
      method = "ranger",
      tuneGrid = tgrid,
      num.trees = 500,
      importance = "impurity")



#some differences in optimal configurations but all seems to perform ok under mtry = 3 and node size = 5


######prediction models
#next we estimate accuracy, false positive and negative rates, and predictions of the RF model.
#we do this through several iterations in order to integrate sampling uncertainties generated when subsetting the original datasets.

#empty data frames
#accuracy

accuracy <- data.frame(accuracy=double(),fn=double(),fp=double(),
                 data=character(),
                 stringsAsFactors=FALSE)


accuracy_qc<-accuracy


#predictions

pred_frame<-as.data.frame(qc_map)[,1:2]
pred_frame[,2]<-NA

pred_frame_qc<-pred_frame


#data frame for variable importance (only qc model)

imp_qc<-data.frame(importance=double(),varnames=character(), stringsAsFactors=FALSE)

#ideally run this through 1000 iteration, for the moment we will just do 10
for(i in 1:10){
  
#first part is repeating the sampling procedures without a set seed
  no_milfoil$prob<-no_milfoil$GBIF_Species/sum(no_milfoil$GBIF_Species)

  pseudo<-sample(1:nrow(no_milfoil),size=nrow(milfoil_qc), prob=no_milfoil$prob)
  
  rf_dat<-as.data.frame(rbind(no_milfoil[pseudo,-ncol(no_milfoil)],milfoil_qc))
  

  #####first split the data into a training and test data set

  ## 75% of the sample size
  
  smp_size <- floor(0.75 * nrow(rf_dat))
  
  ## split the dataset based on random row selection
  train_ind <- sample(seq_len(nrow(rf_dat)), size = smp_size)
  
  train <- rf_dat[train_ind, ]
  test <- rf_dat[-train_ind, ]
  
  
  ###create training data based on different milfoil datasets 
  train_qc<-train
  
  #replace the milfoil observation with milfoil observation from the other datasets
  
  
#second part is running prediction models for each training set
  

  
fit <- ranger(factor(MyrSpic)~dis_m3_pyr+dis_m3_pmx+riv_tc_ssu +riv_tc_usu+rev_mc_usu+ lkv_mc_usu+ele_mt_sav+tmp_dc_syr+ tmp_dc_smx+pre_mm_syr+
                for_pc_sse+for_pc_use+crp_pc_sse+crp_pc_use+pst_pc_sse+pst_pc_use +ppd_pk_sav+ppd_pk_uav+urb_pc_sse+urb_pc_use+rdd_mk_sav+rdd_mk_uav+wet_extent+bicarb_mean,
              data = train_qc,importance = "impurity", mtry = 3,min.node.size=5)

pred<-predict(fit,test)

#accuracy
accuracy_qc[i,1]<-sum(pred$predictions==test$MyrSpic)/length(test$MyrSpic)
accuracy_qc[i,4]<-"QC"

#false negative rate
accuracy_qc[i,2]<-sum(as.numeric(pred$predictions[which(test$MyrSpic==1)])-1)/length(which(test$MyrSpic==1))

#false positive rate
accuracy_qc[i,3]<-sum(as.numeric(pred$predictions[which(test$MyrSpic==0)])-1)/length(which(test$MyrSpic==0))

##var importance

imp <- as.data.frame(fit$variable.importance)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
colnames(imp)[1]<-"importance"

imp_qc<-rbind(imp_qc,imp)

#estimate probability of presence

prob_fit<- ranger(factor(MyrSpic)~dis_m3_pyr+dis_m3_pmx+riv_tc_ssu +riv_tc_usu+rev_mc_usu+ lkv_mc_usu+ele_mt_sav+tmp_dc_syr+ tmp_dc_smx+pre_mm_syr+
                    for_pc_sse+for_pc_use+crp_pc_sse+crp_pc_use+pst_pc_sse+pst_pc_use +ppd_pk_sav+ppd_pk_uav+urb_pc_sse+urb_pc_use+rdd_mk_sav+rdd_mk_uav+wet_extent+bicarb_mean,
                  data = train_qc,importance = "impurity", mtry = 3,min.node.size=5,probability = TRUE)

spatial_pred<-predict(prob_fit,as.data.frame(qc_map))

pred_frame_qc<-cbind(pred_frame_qc,spatial_pred$predictions[,2])

}

total_accuracy<-accuracy_qc

#estimate mean accuracy and confidence limits

m<-lm(accuracy~1,data=total_accuracy)

#mean
coef(m)

#95% confidence intervals
confint(m)


#false negatives

#estimate mean accuracy and confidence limits

m<-lm(fn~1,data=total_accuracy)

#mean
coef(m)

#95% confidence intervals
confint(m)

#false positives

#estimate mean accuracy and confidence limits

m<-lm(fp~1,data=total_accuracy)

#mean
coef(m)

#95% confidence intervals
confint(m)



####predictions and projections

qc_map$myr_qcpred<-rowMeans(pred_frame_qc[,3:ncol(pred_frame_qc)])


#estimate proportion of total area suited for the species

mean_area<-as.data.frame(colSums(qc_map$SUB_AREA.x*pred_frame_qc[,3:ncol(pred_frame_qc)]))
colnames(mean_area)<-"dist"
mean_area$scene<-"present"


total_area<-rbind( mean_area)
sum(qc_map$SUB_AREA.x)
head(total_area)
m<-lm(dist~1,data=total_area)
coef(m)
confint(m)

#%
coef(m)/1536415
confint(m)/1536415


total_area$scene<-factor(total_area$scene, levels = c("present" ,"126_70" , "126_100" ,"370_70" , "370_100", "585_70" , "585_100") )


#plot probabilities
m1<-ggplot()+
geom_sf(data = qc_map, aes(fill = myr_qcpred), colour = NA) +
  scale_fill_viridis_c(limits = c(0, 1),name ="QC") +
  theme_bw()+
  theme(legend.position = c(0.87, 0.8))
m1

####variable importance

imp_p<-as.data.frame(coef(lm(importance~-1+varnames,data = imp_qc)))
imp_p$varnames <- rownames(imp_p) # row names to column
rownames(imp_p) <- NULL  
colnames(imp_p)[1]<-"Importance"


b<-ggplot(imp_p, aes(x=reorder(varnames, Importance), weight=Importance)) + 
  geom_bar() +
  ylab("Variable Importance") +
  xlab("Variable Name")+
  coord_flip()+ theme(legend.position = "none")+
  scale_fill_viridis(discrete=TRUE)+
  theme_classic()+ theme(legend.position = "none")
b

###partial residual plots

fit_m <- randomForest(MyrSpic~dis_m3_pyr+dis_m3_pmx+riv_tc_ssu +riv_tc_usu+rev_mc_usu+ lkv_mc_usu+ele_mt_sav+tmp_dc_syr+ tmp_dc_smx+pre_mm_syr+
                  for_pc_sse+for_pc_use+crp_pc_sse+crp_pc_use+pst_pc_sse+pst_pc_use +ppd_pk_sav+ppd_pk_uav+urb_pc_sse+urb_pc_use+rdd_mk_sav+rdd_mk_uav+wet_extent+bicarb_mean,
                data = rf_dat, probability = TRUE,mtry = 3,min.node.size=5)
v1<- visreg(fit_m, "tmp_dc_smx", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()
  
v2<- visreg(fit_m, "tmp_dc_syr", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()
  
v3<- visreg(fit_m, "pre_mm_syr",  partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()
  
v4<- visreg(fit_m, "ppd_pk_uav", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()
  
v5<- visreg(fit_m, "ppd_pk_sav", partial=FALSE, rug=FALSE, gg=TRUE)+
  theme_classic()
  
grid.arrange(v1,v2,v3,v4,v5,ncol=1)


