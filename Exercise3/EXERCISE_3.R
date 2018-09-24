setwd("C:/Users/oyeda/Desktop/APPLIED_SPATIAL_STAT/Exercise3")

#load required modules
install.packages("spdep")

library(spdep)
library(foreign)

#boston file path
boston_fp<-"C:/Users/oyeda/Desktop/APPLIED_SPATIAL_STAT/Exercise3/boston/boston.dbf"


#read data
boston <- read.dbf(file=boston_fp)

#attach data to memory
attach(boston)

#inspect data
names(boston)
summary(boston)
summary(AGE)
hist(AGE)
plot(CRIM, CMEDV, main="median house value vs crime")

#load spatial weights
geoda_weights <- read.gal("boston_queen.gal")
#visualize connectivity
plot.nb(geoda_weights,cbind(x,y),col="red")

# the imported weights are in fact something called a "neighbors list". R understands a
# different version called "listw", and so you need to make this conversion. To achieve that for the
# two weight files imported above, type:
#convert weights to R-workable format 
weights <- nb2listw(geoda_weights)

#for extra explorations:
#make a sparse spatial weights matrix
W <- as(as_dgRMatrix_listw(weights), "CsparseMatrix")
#display the matrix
W
image(W)


#help file of R's linear regression command 
help(lm)

#run a non-spatial OLS (Ordinary Least Squares) model 
OLS <- lm(CMEDV ~ CRIM + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT)
#summarize the OLS model
summary(OLS)

#Lagrange multiplier tests for selecting spatial regression model
LMtests <- lm.LMtests(OLS, weights, test="all")
print(LMtests)

#run a spatial error model
sp.err <- errorsarlm(CMEDV ~ CRIM + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT, data=boston, weights)

#summarize the results
sum.err <- summary(sp.err, Nagelkerke=TRUE)
print(sum.err, signif.stars=TRUE)

#run a spatial Durbin model
#first create a sparse matrix and calculate the traces of powers - can be useful for large samples
W <- as(as_dgRMatrix_listw(weights), "CsparseMatrix")
tr <- trW(W, type="MC")

sp.durb <- lagsarlm(CMEDV ~ CRIM + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT, data=boston, weights,
                    type="mixed", method="MC", trs=tr)

#test the common factor hypothesis
CFH <- LR.sarlm(sp.durb, sp.err)
print(CFH)

#preliminary summary 
summary(sp.durb)

#simulate spatial impacts
impacts(sp.durb, listw=weights)
#more detail (by simulating also the p-values of the spatial impacts and by using the traces of the spatial weights matrix)
impacts.durb <- impacts(sp.durb, tr=tr, R=100, zstats=TRUE)
summary(impacts.durb, zstats=TRUE)

#assume we want to run a spatial lag model, regardless of the LM tests reults
sp.lag <- lagsarlm(CMEDV ~ CRIM + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT, data=boston, weights)

#summarize the results
sum.lag <- summary(sp.lag, Nagelkerke=TRUE)
print(sum.lag, signif.stars=TRUE)

#spatial impacts
impacts.lag <- impacts(sp.lag, tr=tr, R=100, zstats=TRUE)
summary(impacts.lag, zstats=TRUE)


#SAC/SARMA and weights explorations
#import and convert three alternative weight files
# geoda_5nn <- read.gwt2nb("boston_5nn.gwt", region.id=POLY_ID) #this does not work with the POLY_ID
geoda_5nn <- read.gwt2nb("boston_5nn.gwt")
# geoda_10nn <- read.gwt2nb("boston_10nn.gwt", region.id=POLY_ID)
geoda_10nn <- read.gwt2nb("boston_10nn.gwt")

weights.5nn <- nb2listw(geoda_5nn)
weights.10nn <- nb2listw(geoda_10nn)

#correlogram for median value (note that this command needs the nb weights and also to set a zero neighbors policy)
plot(sp.correlogram(geoda_5nn, CMEDV, order=5, method = "I", zero.policy = T))
plot(sp.correlogram(geoda_10nn, CMEDV, order=5, method = "I", zero.policy = T)) 

#estimate a SAC model (just for demonstrating the syntax, but note that it will also need spatial impacts simulation)
SAC <- sacsarlm(CMEDV ~ CRIM + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT, 
                data=boston, weights.5nn,  weights.10nn)
#MyExtraNote:the weights.5nn,  weights.10nn used in the above parameters are
#controversial and maybe not necessary.

