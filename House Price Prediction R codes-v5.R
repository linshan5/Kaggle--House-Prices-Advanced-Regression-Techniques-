library(car)
library(mice)
library(estimatr)
library(tidyverse)
library(dplyr)
library(ggplot2)


train<-read.csv(file.choose(), header=TRUE, sep=",") #load the data into the train dataframe
prediction<-read.csv(file.choose(), header=TRUE, sep=",") #load the data into the prediction dataframe


                                            #PArt 1: EDA: 
str(train) #show the structure of (data types in) the train dataframe. Can see there are mix of integers and factor types across all variables
head(train, 5) #show the first 5 rows in the train dataframe
tail(train,5) #show the last 5 rows in the train dataframe
summary(train) 

prediction$SalePrice<-NA  #create a new column called "SalePrice" for the prediction dataframe, which is the variable need to be predicted
str(prediction)
data<-rbind(train,prediction)
summary(data) #we found that the following 34 variables (exclude NA in the column SalePrice from test dataframe) contain NA need to be taken care of: LotFrontage, Alley, Utilities, Exterior1st, Exterior2nd, MasVnrType, MasVnrArea, BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, Electrical,BsmtFullBath, BsmtHalfBath, KitchenQual, Functional, GarageType,GarageYrBlt,GarageCars, GarageArea, GarageFinish,GarageQual,GarageCond,PoolQc,Fence,MiscFeature, SaleType,  MSZoning, FireplaceQu

md.pattern(data) #as shown on the md.pattern graph, there are 34 variables contain missing values (NAs), as also indicated earlier through summary(data) function

ggplot(train,aes(x=train$SalePrice))+geom_histogram() #this graph shows the salepricesin train dataframe are right skewed. Most people purchase less expensive houses





#NA records replacement for variables with NA:

##replace NA values in column LotFrontage to the median value:
hist(data$LotFrontage)
summary(data$LotFrontage) #the statistics summary indicated the median value of data$LotFrontage is 68


data$LotFrontage[is.na(data$LotFrontage)] <- 68
summary(data$LotFrontage)
any(is.na(data$LotFrontage))

##replace NA values in column Alley to "No_Alley" to represent those houses with no alley access:
data$Alley<-as.character(data$Alley)
data$Alley[is.na(data$Alley)]<- "No_Alley"
data$Alley<-as.factor(data$Alley)

any(is.na(data$Alley))

##replace 2 NA values in column Utilies to the most common utilities type according to the summary() function:
summary(data$Utilities) #we see from the summary statistic that AllPub are the majority 

data$Utilities<-as.character(data$Utilities)
data$Utilities[is.na(data$Utilities)]<- "AllPub"
data$Utilities<-as.factor(data$Utilities)

any(is.na(data$Utilities))

##replace NA values in Exterior Variables Exterior1st & Exterior2nd with the most common type according to the summary() functions:
summary(data$Exterior1st) #we see that VinylSd is the most common one (count 1025)
data$Exterior1st<-as.character(data$Exterior1st)
data$Exterior1st[is.na(data$Exterior1st)]<- "VinylSd"   #replace NA with "VinylSd"
data$Exterior1st<-as.factor(data$Exterior1st)  
any(is.na(data$Exterior1st))


summary(data$Exterior2nd) #we see that VinylSd is the most common one (count 1014)
data$Exterior2nd<-as.character(data$Exterior2nd)
data$Exterior2nd[is.na(data$Exterior2nd)]<- "VinylSd"
data$Exterior2nd<-as.factor(data$Exterior2nd)
any(is.na(data$Exterior2nd))

##replace NA values from the column MasVnrType with "None": we anticipate those are houses with no Masonry veneer type
data$MasVnrType<-as.character(data$MasVnrType)
data$MasVnrType[is.na(data$MasVnrType)]<- "None"
data$MasVnrType<-as.factor(data$MasVnrType)

any(is.na(data$MasVnrType))

##replace NA values from the column MasVnrArea with 0: we anticipate those are houses with no Masonry veneer type and therefore the Masonry veneer area in square feet is 0
data$MasVnrArea[is.na(data$MasVnrArea)] <- 0
summary(data$MasVnrArea)
any(is.na(data$MasVnrArea))

##replace NA values in Basement related variables: BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, BsmtFullBath, BsmtHalfBath
data$BsmtQual<-as.character(data$BsmtQual)
data$BsmtQual[is.na(data$BsmtQual)]<- "None"
data$BsmtQual<-as.factor(data$BsmtQual)

any(is.na(data$BsmtQual))   ##replaced all NA in column BsmtQual (height of basement) with "None" since those are houses without basement

#to scale the column content into ordinal order according to different quality level:
data$BsmtQual <- recode(data$BsmtQual, "Ex"=5,"Gd"=4,"TA"=3,"Fa"=2,"Po"=1,"None"=0)
summary(data$BsmtQual)  

data$BsmtCond<-as.character(data$BsmtCond)
data$BsmtCond[is.na(data$BsmtCond)]<- "None"
data$BsmtCond<-as.factor(data$BsmtCond)
any(is.na(data$BsmtCond))  ##replaced all NA in column BsmtCond (general condition of basement) with "None" since those are houses without basement

#to scale the column content into ordinal order according to different quality level:
data$BsmtCond <- recode(data$BsmtCond, "Ex"=5,"Gd"=4,"TA"=3,"Fa"=2,"Po"=1,"None"=0)
summary(data$BsmtCond)


data$BsmtExposure<-as.character(data$BsmtExposure)
data$BsmtExposure[is.na(data$BsmtExposure)]<- "None"
data$BsmtExposure<-as.factor(data$BsmtExposure)

any(is.na(data$BsmtExposure)) ##replaced all NA in column BsmtExposure (explosure to walkout or garden level walls) with "None" since those are houses without basement

#to scale the column content into ordinal order according to different quality level:
data$BsmtExposure <- recode(data$BsmtExposure, "Gd"=4,"Av"=3,"Mn"=2,"No"=1,"None"=0)
summary(data$BsmtExposure)


data$BsmtFinType1<-as.character(data$BsmtFinType1)
data$BsmtFinType1[is.na(data$BsmtFinType1)]<- "None"
data$BsmtFinType1<-as.factor(data$BsmtFinType1)

any(is.na(data$BsmtFinType1))  ##replaced all NA in column BsmtFinType1 (Rating of basement finished area) with "None" since those are houses without basement
#to scale the column content into ordinal order according to different quality level:
data$BsmtFinType1 <- recode(data$BsmtFinType1, "GLQ"=6,"ALQ"=5,"BLQ"=4,"Rec"=3,"LwQ"=2,"Unf"=1,"None"=0)
summary(data$BsmtFinType1)


data$BsmtFinType2<-as.character(data$BsmtFinType2)
data$BsmtFinType2[is.na(data$BsmtFinType2)]<- "None"
data$BsmtFinType2<-as.factor(data$BsmtFinType2)

any(is.na(data$BsmtFinType2))  ##replaced all NA in column BsmtFinType2 (Rating of basement finished area (if multiple types)) with "None" since those are houses without basement
#to scale the column content into ordinal order according to different quality level:
data$BsmtFinType2 <- recode(data$BsmtFinType2, "GLQ"=6,"ALQ"=5,"BLQ"=4,"Rec"=3,"LwQ"=2,"Unf"=1,"None"=0)
summary(data$BsmtFinType2)


data$BsmtFinSF1[is.na(data$BsmtFinSF1)] <- 0
summary(data$BsmtFinSF1)
any(is.na(data$BsmtFinSF1)) ##replaced all NA in column BsmtFinSF1 (Type 1 finished square feet) with value 0 since those are houses without basement

data$BsmtFinSF2[is.na(data$BsmtFinSF2)] <- 0
summary(data$BsmtFinSF2)
any(is.na(data$BsmtFinSF2)) ##replaced all NA in column BsmtFinSF2 (Type 2 finished square feet) with value 0 since those are houses without basement

data$BsmtUnfSF[is.na(data$BsmtUnfSF)] <- 0
summary(data$BsmtUnfSF)
any(is.na(data$BsmtUnfSF)) ##replaced all NA in column BsmtUnfSF (unfinished square feet of basement area) with value 0 since those are houses without basement

data$TotalBsmtSF[is.na(data$TotalBsmtSF)] <- 0
summary(data$TotalBsmtSF)
any(is.na(data$TotalBsmtSF)) ##replaced all NA in column TotalBsmtSF (Total square feet of basement area) with value 0 since those are houses without basement

data$BsmtFullBath[is.na(data$BsmtFullBath)] <- 0
summary(data$BsmtFullBath)
any(is.na(data$BsmtFullBath)) ##replaced all NA in column BsmtFullBath (Basement full bathrooms) with value 0 since those are houses without basement

data$BsmtHalfBath[is.na(data$BsmtHalfBath)] <- 0
summary(data$BsmtHalfBath)
any(is.na(data$BsmtHalfBath)) ##replaced all NA in column BsmtHalfBath (Basement half bathrooms) with value 0 since those are houses without basement

#replace 1 NA in the column Electrical with the most common type according to summary() function:
summary(data$Electrical) #we see that SBrkr is the most common type (counts as 2671)
data$Electrical<-as.character(data$Electrical)
data$Electrical[is.na(data$Electrical)]<- "SBrkr" #replace NA to "SBrkr"
data$Electrical<-as.factor(data$Electrical)
any(is.na(data$Electrical)) 


#replace NA from the column KitchenQual with the average level indicted TA (typical/average):
data$KitchenQual<-as.character(data$KitchenQual)
data$KitchenQual[is.na(data$KitchenQual)]<- 'TA'
data$KitchenQual<-as.factor(data$KitchenQual)
any(is.na(data$KitchenQual)) 
#to scale the column content into ordinal order according to different quality level:
data$KitchenQual <- recode(data$KitchenQual, "Ex"=5,"Gd"=4,"TA"=3,"Fa"=2,"Po"=1)
summary(data$KitchenQual)


#replace NA from the column Functional with "Typ" (Assume typical unless deductions are warranted):
data$Functional<-as.character(data$Functional)
data$Functional[is.na(data$Functional)]<- 'Typ'
data$Functional<-as.factor(data$Functional)
any(is.na(data$Functional)) 

#replace NA from Garage Related variables:GarageType,GarageYrBlt,GarageCars, GarageArea, GarageFinish,GarageQual,GarageCond

data$GarageType<-as.character(data$GarageType)
data$GarageType[is.na(data$GarageType)]<- 'None'
data$GarageType<-as.factor(data$GarageType)
any(is.na(data$GarageType)) ##replace NA from the column GarageType with "None" since thouse houses do not have garage.

data$GarageYrBlt[is.na(data$GarageYrBlt)] <- data$YearBuilt[is.na(data$GarageYrBlt)]
summary(data$GarageYrBlt)
any(is.na(data$GarageYrBlt)) ##replace NA from the column GarageYrBlt with YearBuilt info from the same years when NA exist in the GarageYrBlt column

data$GarageCars[is.na(data$GarageCars)] <- 0
summary(data$GarageCars)
any(is.na(data$GarageCars)) ##replaced all NA in the column GarageCars (Size of garage in car capacity) with value 0 since those are houses without garage

data$GarageArea[is.na(data$GarageArea)] <- 0
summary(data$GarageArea)
any(is.na(data$GarageArea)) ##similarly, replaced all NA in the column GarageArea (Size of garage in square feet) with value 0 since those are houses without garage

data$GarageFinish<-as.character(data$GarageFinish)
data$GarageFinish[is.na(data$GarageFinish)]<- 'None'
data$GarageFinish<-as.factor(data$GarageFinish)
any(is.na(data$GarageFinish)) ##replace NA from the column GarageFinish (Interior finish of the garage) with "None" since thouse houses do not have garage.
#to scale the column content into ordinal order according to different quality level:
data$GarageFinish <- recode(data$GarageFinish, "Fin"=3,"RFn"=2,"Unf"=1,"None"=0)
summary(data$GarageFinish)

data$GarageQual<-as.character(data$GarageQual)
data$GarageQual[is.na(data$GarageQual)]<- 'None'
data$GarageQual<-as.factor(data$GarageQual)
any(is.na(data$GarageQual)) ##replace NA from the column GarageQual (Garage quality) with "None" since thouse houses do not have garage.
#to scale the column content into ordinal order according to different quality level:
data$GarageQual <- recode(data$GarageQual, "Ex"=5,"Gd"=4,"TA"=3,"Fa"=2,"Po"=1,"None"=0)
summary(data$GarageQual)


data$GarageCond<-as.character(data$GarageCond)
data$GarageCond[is.na(data$GarageCond)]<- 'None'
data$GarageCond<-as.factor(data$GarageCond)
any(is.na(data$GarageCond)) ##replace NA from the column GarageCond (Garage condition) with "None" since thouse houses do not have garage.
#to scale the column content into ordinal order according to different condition level:
data$GarageCond <- recode(data$GarageCond, "Ex"=5,"Gd"=4,"TA"=3,"Fa"=2,"Po"=1,"None"=0)
summary(data$GarageCond)


#replace NA from the column PoolQC with "None" as those are houses without a poor:
data$PoolQC<-as.character(data$PoolQC)
data$PoolQC[is.na(data$PoolQC)]<- 'None'
data$PoolQC<-as.factor(data$PoolQC)
any(is.na(data$PoolQC)) 
#to scale the column content into ordinal order according to different quality level:
data$PoolQC <- recode(data$PoolQC, "Ex"=4,"Gd"=3,"TA"=2,"Fa"=1,"None"=0)
summary(data$PoolQC)  #we can see from the summary table that mostly are small numbers below 1, as its uncommon to have pool for most houses


#replace NA from the column Fence with "None" as those are houses without a fence:
data$Fence<-as.character(data$Fence)
data$Fence[is.na(data$Fence)]<- 'None'
data$Fence<-as.factor(data$Fence)
any(is.na(data$Fence)) 

#replace NA from the column MiscFeature (Miscellaneous feature not covered in other categories) with "None":
data$MiscFeature<-as.character(data$MiscFeature)
data$MiscFeature[is.na(data$MiscFeature)]<- 'None'
data$MiscFeature<-as.factor(data$MiscFeature)
any(is.na(data$MiscFeature)) 

#replace NA from the column SaleType with the most common saletype according to the statistic summary by using summary() function:
summary(data$SaleType) ##we see that WD is the most common saletype as it has the highest count of 2525
data$SaleType<-as.character(data$SaleType)
data$SaleType[is.na(data$SaleType)]<- 'WD'
data$SaleType<-as.factor(data$SaleType)
any(is.na(data$SaleType)) #replace NA with "WD"

#replace NA from the column  MSZoning with the most common type according to summary()function:
summary(data$MSZoning) #we see that  RL is the most common type (counts 2265)
data$MSZoning<-as.character(data$MSZoning)
data$MSZoning[is.na(data$MSZoning)]<- 'RL'
data$MSZoning<-as.factor(data$MSZoning)
any(is.na(data$MSZoning)) #replace NA with "RL"


#replace NA from the column FireplaceQu with "None":
data$FireplaceQu<-as.character(data$FireplaceQu)
data$FireplaceQu[is.na(data$FireplaceQu)]<- 'None'
data$FireplaceQu<-as.factor(data$FireplaceQu)
any(is.na(data$FireplaceQu))
#to scale the column content into ordinal order according to different quality level:
data$FireplaceQu <- recode(data$FireplaceQu, "Ex"=5,"Gd"=4,"TA"=3,"Fa"=2,"Po"=1,"None"=0)
summary(data$FireplaceQu)  


#double check if now all NA in applicable columns have all been taken care of (except for SalePrice Column:
md.pattern(data) #the md.pattern graph did indicate the only column with NA now is SalePrice



#some feature engineering:
##combine the basement squre feet TotalBsmtSF with the ground level squre feet info GrLivArea, and make a new column called"total_area"
data<-mutate(data,'total_area'=GrLivArea+TotalBsmtSF)
ggplot(data=data[!is.na(data$SalePrice),], aes(x=total_area, y=SalePrice))+geom_point()+geom_text(aes(label=ifelse(total_area>7500,rownames(data),'')),hjust=0,vjust=0)

##we observe from the plot that there are 2 outliers we will be removing next 
data <- data[-c(524, 1299),]   #to remove outliers



##MSSubClass is convered from Numeric to factor as it is actually a categorical variable by content 
data$MSSubClass<-as.factor(data$MSSubClass)
 
##convert MoSold & YrSold into factor instead interger, as the one month/Yr sold in one year is not better than the other:
data$MoSold<-as.factor(data$MoSold)

data$YrSold<-as.factor(data$YrSold)


write.csv(data, file = "afterclean_dataV4.csv") 


#check the correlation value between each independent variable & Saleprices:
Num_Varibs <- which(sapply(data, is.numeric)) #to find all variables in the data dataframe which are numeric 
Num_Varibs  #to display all numeric variables
data_numVar <- data[, Num_Varibs]
cor_numVar <- cor(data_numVar , use="pairwise.complete.obs") 
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
cor_sorted  #to display all the variables which have correlation with SalePrices at least abs(0.5)



                                        #PArt 2: Models Building: 


#cross-fold validation (to see how our models fit well to predict y when we dont have information on prediction SalePrices)

#apply(data[1:1458,], 2, function(x) sum(is.na(x)))   #to check if na exist

train_final <- data[1:1166,] #choose 80% of original train dataframe into a train set
test_final  <- data[1167:1458,] #choose 20% of original train dataframe into a test set
prediction_final<-data[1459:nrow(data),] #entries starting from Id 1461 (or row# 1459) are all part of prediction dataframe



#XlstFlrSF & GarageArea & GarageFinished & GarageYrBlt & YearRemodAdd are not included for modelbuilding due to high correlation with other variables under the same category
#Model 1:
linear_model<- lm(SalePrice~OverallQual+total_area+KitchenQual+GarageCars+BsmtQual+FullBath+TotRmsAbvGrd+YearBuilt+FireplaceQu,data=train_final) 
summary(linear_model) # summary of the "linear_model" regression model


predicted.SalePrices<-predict(linear_model, prediction_final) #use the "linear_model" model to predict saleprices for the prediction data
write.csv(predicted.SalePrices, file = "predicted.SalePrices.lmmodelv4.csv") 

hist(predicted.SalePrices) #histogram of predicted.SalePrices from model #1
summary(predicted.SalePrices) #can see this model's prediction has saleprices being negative--> sign of not an reliable/accurate model. need to explore other options

par(mfrow=c(1,4)) # this command sets the plot window to show 1 row of 4 plots
plot(linear_model) #diagnostic plots for the "linear_model" model.

predicted.SalePrices.testing<-predict(linear_model, test_final)
predicted.SalePrices.testing
##next 2 lines to calcualte the value of MAPE in order to evalute the quality of our model prediction power using model #1
percent.errors <- abs((test_final$SalePrice-predicted.SalePrices.testing)/test_final$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors) #display Mean Absolute Percentage Error (MAPE) is 14.10901 meaning this frist model with linear regression has a weak prediction power




#model 2: log-saleprice model
log_model<- lm(log(SalePrice)~OverallQual+total_area+KitchenQual+GarageCars+BsmtQual+FullBath+TotRmsAbvGrd+YearBuilt+FireplaceQu,data=train_final)
summary(log_model)

predicted.SalePrices.log<-exp(predict(log_model, prediction_final))
write.csv(predicted.SalePrices.log, file = "predicted.SalePrices.logmodelv4.csv")

summary(predicted.SalePrices.log)  #we can see now negative prediction saleprices not longer appear with this second model. Sign of model prediction power improvement

par(mfrow=c(1,4))
plot(log_model) #diagnostic plots for the "log_model" model 

predicted.SalePrices.testing.log<-exp(predict(log_model, test_final))
percent.errors.log <- abs((test_final$SalePrice-predicted.SalePrices.testing.log)/test_final$SalePrice)*100
mean(percent.errors.log) #display MAPE is 11.3599 which is slightly better than model#1




###Model 3: using Interactions in building log-Saleprice model:
### Interactions: applying nonlinear feature engeering eg. multiple multiple x variables together instead of addition only
log_model.i<- lm(log(SalePrice)~total_area+OverallQual+KitchenQual*BsmtQual*FireplaceQu+GarageCars+TotRmsAbvGrd+FullBath+YearBuilt,data=train_final)
summary(log_model.i)

predicted.SalePrices.log.i<-exp(predict(log_model.i, prediction_final))
write.csv(predicted.SalePrices.log.i, file = "Predicted.SalePrices.LOG.INTERACTIONv4.csv")

summary(predicted.SalePrices.log.i)
##to calculate MAPE value for our model #3:
predicted.SalePrices.testing.log.i<-exp(predict(log_model.i, test_final))
percent.errors.log.i <- abs((test_final$SalePrice-predicted.SalePrices.testing.log.i)/test_final$SalePrice)*100
mean(percent.errors.log.i) #display MAPE is 11.30171 after involving interactions between variables



### Variable Selection: 
###
### Regularizations (LASSO and ridge)
###

#install.packages("glmnet")
library(glmnet)

#create the y variable and matrix (capital X) of x variables
y<-log(train_final$SalePrice)

data_without_sp<- data[, ! colnames(data) %in% c("SalePrice","GrLivArea","TotalBsmtSF")]

X<-model.matrix(~.,data_without_sp)



# split X into testing, trainig/holdout and prediction as before
X.training<-X[1:1166,]
X.testing<-X[1167:1458,]
X.prediction<-X[1459:nrow(data_without_sp),]

par(mfrow=c(1,1))

#
###LASSO (alpha=1)
#
lasso.fit<-glmnet(x = X.training , y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")


#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)

penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph-> -5.662986
plot(crossval,xlim=c(-6,-4),ylim=c(0.012,0.027)) # lets zoom-in

#using LASSO reg model with optimal lamba for penalty power:
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing-test_final$SalePrice)/lasso.testing*100) #calculate and display MAPE=8.060578



#
###Ridge (alpha=0)
#
ridge.fit<-glmnet(x = X.training , y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)

penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) #see where it was on the graph-> -2.662648
plot(crossval,xlim=c(-3,0),ylim=c(0.012,0.027)) # lets zoom-in

ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-test_final$SalePrice)/ridge.testing*100)  #calculate and display MAPE=8.663106


#
###Elastic Net Regression (alpha=0.5)
#
elastic.fit<-glmnet(x = X.training , y = y, alpha = 0.5)
plot(elastic.fit, xvar = "lambda")

crossval <-  cv.glmnet(x =X.training, y = y, alpha = 0.5)
plot(crossval)

penalty.elastic <- crossval$lambda.min 
log(penalty.elastic) #see where it was on the graph-> -4.969839

elastic.opt.fit <-glmnet(x = X.training, y = y, alpha = 0.5, lambda = penalty.elastic) #estimate the model with that
coef(elastic.opt.fit)

elastic.testing <- exp(predict(elastic.opt.fit, s = penalty.elastic, newx =X.testing))
mean(abs(elastic.testing-test_final$SalePrice)/elastic.testing*100)  #calculate and display MAPE= 8.037803




# comparing the performance on the testing set according to MAPE value, elastic net is the best, so use it for prediction
predicted.prices.elastic <- exp(predict(elastic.opt.fit, s = penalty.elastic, newx =X.prediction))

final_submission_df<- data.frame('ID' = c(1461:2919),
                      'SalePrice' = predicted.prices.elastic[,1])

write.csv(final_submission_df, file = "Predicted SalePrices elastic V5.csv", row.names = FALSE) # export the predicted prices into a CSV file












