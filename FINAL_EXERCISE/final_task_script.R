rm(list=ls())
setwd("C:/Users/oyeda/Desktop/APPLIED_SPATIAL_STAT/FINAL_EXERCISE")

#load required modules
# install.packages("spdep")

library(spdep)
library(foreign)
library(corrplot)
library(MASS)

#file path
popHel_fp<-"C:/Users/oyeda/Desktop/APPLIED_SPATIAL_STAT/FINAL_EXERCISE/Helsinki_Area.dbf"


#read data
popHel <- read.dbf(file=popHel_fp)

#create a new ID column
popHel$ID<-c(1:nrow(popHel))
write.dbf(popHel, "popHel_ID")

#attach data to memory
attach(popHel)

#inspect data
names(popHel)
summary(popHel)
summary(HE_VAKIY)
hist(HE_VAKIY, breaks = 5)



#load spatial weights
geoda_weights <- read.gal("Helsinki_queen.gal")
#try
geoda_weights <- read.gwt2nb("Helsinki_knn.gwt")
#visualize connectivity
plot.nb(geoda_weights,cbind(EUREF_X,EUREF_Y),col="red")

# the imported weights are in fact something called a "neighbors list". R understands a
# different version called "listw", and so you need to make this conversion. To achieve that for the
# two weight files imported above, type:
#convert weights to R-workable format 
# weights <- nb2listw(geoda_weights)
weights <- nb2listw(geoda_weights, zero.policy = T)

#for extra explorations:
#make a sparse spatial weights matrix
W <- as(as_dgRMatrix_listw(weights), "CsparseMatrix")
#display the matrix
W
image(W)




#help file of R's linear regression command 
help(lm)


toupper("he_vakiy", "he_miehet", "he_naiset", "he_kika", "ko_perus", 
        "ko_koul", "ko_yliop", "ko_ammat" , "ko_al_kork", "ko_yl_kork", 
        "hr_tuy","hr_ktu", "hr_mtu", "hr_ovy" ,"te_eil_np","te_aik", 
        "te_elak", "te_omis_as" , "te_vuok_as", "te_muu_as" , "tr_ovy", 
        "tr_ktu" , "tr_mtu","tr_pi_tul" , "tr_ke_tul" , "tr_hy_tul" , 
        "pt_opisk" , "pt_elakel")


#change the predictors to upper case
pred_lower<-(toupper("he_vakiy, he_miehet , he_naiset , he_kika, ko_perus, ko_koul , ko_yliop , ko_ammat , ko_al_kork, ko_yl_kork , hr_tuy , hr_ktu , hr_mtu , hr_ovy , te_eil_np ,te_aik, te_elak, te_omis_as , te_vuok_as , te_muu_as , tr_ovy, tr_ktu , tr_mtu,tr_pi_tul , tr_ke_tul , tr_hy_tul , pt_opisk , pt_elakel "))
#replace the comma(,) with addition sign(+)
pred_upper<-gsub(",", "+", pred_lower)

popHel2<- popHel

#run a non-spatial OLS (Ordinary Least Squares) model 
OLS <- lm(HR_KTU ~  HE_KIKA+ HR_OVY+ KO_KOUL+ PT_TYOTT+ PT_TYOLL+
          TP_ALKU_A+TP_PALV_GU+
          TP_B_KAIV+TP_H_KULJ+TP_O_JULK+
          PT_TYOVY + TE_NUOR+ TE_EIL_NP + KO_AL_KORK+
            KO_YL_KORK+KO_AMMAT + HE_MIEHET+HE_NAISET+
            KO_PERUS+ TR_PI_TUL + TR_KE_TUL + TR_HY_TUL)

OLS <- lm(HR_KTU ~ HE_KIKA + HR_OVY + KO_KOUL + PT_TYOTT + 
            TP_PALV_GU + TP_O_JULK + KO_YL_KORK + KO_AMMAT + HE_MIEHET + 
            KO_PERUS)
summary(OLS)
stepAIC(OLS)


OLS_NUOR <- lm( TE_NUOR~  HE_KIKA+ HR_OVY+ KO_KOUL+ PT_TYOTT+ PT_TYOLL+
            TP_ALKU_A+TP_PALV_GU+
            TP_B_KAIV+TP_H_KULJ+TP_O_JULK+
            PT_TYOVY + TE_EIL_NP + KO_AL_KORK+
            KO_YL_KORK+KO_AMMAT + HE_MIEHET+HE_NAISET+
            KO_PERUS+ TR_PI_TUL + TR_KE_TUL + TR_HY_TUL)
OLS_NUOR <- lm(formula = TE_NUOR ~ KO_KOUL + PT_TYOTT + PT_TYOLL +  
       KO_YL_KORK + KO_AMMAT + HE_MIEHET + 
     HE_NAISET + KO_PERUS + TR_PI_TUL + TR_KE_TUL + TR_HY_TUL)
summary(OLS_NUOR)
stepAIC(OLS_NUOR)












# data <- popHel[ ,c("HE_KIKA","TR_KTU",
#         "TP_ALKU_A","TP_PALV_GU",
#         "TP_B_KAIV","TP_H_KULJ","TP_O_JULK", 
#         "PT_TYOVY")]
# 
# corHel<-cor(data, method = "spearman")
# corHel
# 
# corrplot(corHel, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# # install.packages("Hmisc")
# library(Hmisc)
# corHel2 <- rcorr(as.matrix(data))
# corHel2
# 
# # Insignificant correlation are crossed
# corrplot(corHel2$r, type="upper", order="hclust", 
#          p.mat = corHel2$P, sig.level = 0.05, insig = "pch")
# # Insignificant correlations are left blank
# corrplot(corHel2$r, type="upper", order="hclust", 
#          p.mat = corHel2$P, sig.level = 0.05, insig = "blank")
# cor(KO_AL_KORK, TE_ELAK)
# #HR_KTU, TR_KTU are highly correlated. so, I'll remove one of it.

# install.packages("PerformanceAnalytics")
# library(PerformanceAnalytics)
# chart.Correlation(popHel3, histogram=TRUE, pch=19)


























#summarize the OLS model
summary(OLS)

stepAIC(OLS, direction = "both")

#After applying sptewise regression
OLS2<-lm(PT_TYOTT ~ HE_MIEHET + KO_PERUS + KO_KOUL + KO_YLIOP + 
           KO_AMMAT + KO_AL_KORK + HR_KTU + HR_OVY + TE_EIL_NP + TE_ELAK + 
           TE_VUOK_AS + TR_OVY + TR_KTU + TR_PI_TUL + PT_OPISK + PT_ELAKEL + 
           TE_MUU_AS)
library(gbm)
sales_gbm1<-gbm(formula = HR_KTU ~ HE_KIKA + HR_OVY + KO_KOUL + PT_TYOTT + 
                  TP_PALV_GU + TP_O_JULK + KO_YL_KORK + KO_AMMAT + HE_MIEHET + 
                  KO_PERUS, data=popHel,
                distribution = "gaussian",n.trees = 2300, shrinkage = 0.001, interaction.depth = 6,
                bag.fraction = 0.75, verbose = F)

summary(sales_gbm1)
# OLS2<-lm(PT_TYOTT ~KO_KOUL + TR_KTU)
OLS2<-lm(PT_TYOLL ~KO_KOUL)
plot(PT_TYOTT, KO_KOUL)
summary(OLS2)

#reduce further by checking least significant one by one
OLS2<-lm(PT_TYOTT ~  KO_KOUL + KO_YLIOP + 
            KO_AL_KORK + HR_KTU + HR_OVY +  TE_ELAK + 
           TE_VUOK_AS + TR_OVY + TR_KTU + TR_PI_TUL + PT_ELAKEL)
summary(OLS2)
anova(OLS2, test="chisq")



#check multicollinearity
popHel3<-popHel[,c("KO_KOUL","KO_YLIOP",
  "KO_AL_KORK", "HR_KTU", "HR_OVY", "TE_ELAK",
  "TE_VUOK_AS", "TR_OVY", "TR_KTU", "TR_PI_TUL", "PT_ELAKEL")]
popHel3<-popHel[,c("KO_KOUL","TR_KTU")]



# corHel<-cor(popHel3, method = "spearman")
# corHel
# 
# corrplot(corHel, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# # install.packages("Hmisc")
# library(Hmisc)
# corHel2 <- rcorr(as.matrix(popHel3))
# corHel2
# 
# # Insignificant correlation are crossed
# corrplot(corHel2$r, type="upper", order="hclust", 
#          p.mat = corHel2$P, sig.level = 0.05, insig = "pch")
# # Insignificant correlations are left blank
# corrplot(corHel2$r, type="upper", order="hclust", 
#          p.mat = corHel2$P, sig.level = 0.05, insig = "blank")
# cor(KO_AL_KORK, TE_ELAK)
# #HR_KTU, TR_KTU are highly correlated. so, I'll remove one of it.


# 
# 
# install.packages("PerformanceAnalytics")
# library(PerformanceAnalytics)
# chart.Correlation(popHel3, histogram=TRUE, pch=19)
# 
# 
# 
# 
# #run a spatial Durbin model
# #first create a sparse matrix and calculate the traces of powers - can be useful for large samples
# W <- as(as_dgRMatrix_listw(weights), "CsparseMatrix")
# tr <- trW(W, type="MC")






