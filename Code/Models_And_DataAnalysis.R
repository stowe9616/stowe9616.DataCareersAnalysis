
library(tidyr)
library(tidyverse)
library(stringr)
library(dplyr)
library(stringr)
library(readr)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)

urlfile<-'https://github.gatech.edu/raw/MGT-6203-Summer-2022-Canvas/Team-33/main/Your%20Project%20Name/Data/FinalDatasetDataNormalized.csv?token=GHSAT0AAAAAAAACJ67G5URVQPCMAM54GKL4YWZ4Q5A'

##############################Load in data and split #####################################
FullDataset<-read.csv(urlfile)
FullDataset<-FullDataset[-c(3030,3031,3813,3814,3815,3816,3817), ]

# Split Full set into test(20%), training(60%), and validation set(20%)
 set.seed(123)
 split1<- sample(c(rep(0, 0.6 * nrow(FullDataset)), rep(1, 0.4 * nrow(FullDataset))))
 FullDataTrain<-FullDataset[split1 ==0,]
 FullDataTestValid<-FullDataset[split1 ==1,]
 split2<-sample(c(rep(0,0.5* nrow(FullDataTestValid)),rep(1,0.5*nrow(FullDataTestValid))))
 FullDataValid<-FullDataTestValid[split2 ==0,]
 FullDataTest<-FullDataTestValid[split2 == 1,]
 


#####Filter out datasets into 3 different titles to run more in depth regressions#####################

DataAnalyst<-filter(FullDataset,Job.Title=='Data Analyst')
DataEngineer<-filter(FullDataset,Job.Title=='Data Engineer')
DataScientist<-filter(FullDataset,Job.Title=='Data Scientist')



##################################Initial Data Analysis ##########################################################
##################################Look at correlation between certain skills #####################################
JustSkills<-FullDataset[,c("python","hadoop","mongo","spark","sql","tableau","kafka","excel","azure","pandas","visualization","machinelearning","aws","saas","etl","warehouse")]
M=cor(JustSkills)
corrplot(M,method='number')
### very light correlations - Strongest correlations between
#                                  1) Spark and hadoop
#                                  2) Kafka and spark
#                                  3) Tableau and Visualization


######################### Check for Outliers ##############################################
par(mfrow=c(1,2))
boxplotSummary<-boxplot(FullDataset$AverageSalary,main="Average Salary")
                        
boxplot(FullDataset$AvgSalaryNorm,main="Normalized Average Salary")
        
outliers<-boxplot(FullDataset$AverageSalary,plot=FALSE)$out
outliers
#### 44 outliers in non-normalized data but normalized data contains 136 outliers 


########################### Check for normality ############################################
par(mfrow=c(1,2))
plot(density(FullDataset$AverageSalary),main="Average Salary")
plot(density(FullDataset$AvgSalaryNorm),main="Average Salary Normalized")


# Not Normal


###########################################################################################
########################### Data Analyst Analysis #########################################
###########################################################################################
##################################Look at correlation between certain skills #####################################
JustSkillsAnalyst<-DataAnalyst[,c("python","hadoop","mongo","spark","sql","tableau","kafka","excel","azure","pandas","visualization","machinelearning","aws","saas","etl","warehouse")]
M=cor(JustSkillsAnalyst)
corrplot(M,method='number')
### very light correlations - Strongest correlations between
#                                  1) Spark and hadoop
#                                  2) Kafka and spark
#                                  3) Tableau and Visualization
##################################################################################################################
# Split Full set into test(20%), training(60%), and validation set(20%)
set.seed(123)
split1<- sample(c(rep(0, 0.6 * nrow(DataAnalyst)), rep(1, 0.4 * nrow(DataAnalyst))))
AnalystDataTrain<-DataAnalyst[split1 ==0,]
AnalystDataTestValid<-DataAnalyst[split1 ==1,]
split2<-sample(c(rep(0,0.5* nrow(AnalystDataTestValid)),rep(1,0.5*nrow(AnalystDataTestValid))))
AnalystDataValid<-AnalystDataTestValid[split2 ==0,]
AnalystDataTest<-AnalystDataTestValid[split2 == 1,]

######################### Check for Outliers ##############################################
par(mfrow=c(1,2))
boxplotSummary<-boxplot(DataAnalyst$AverageSalary,main="Analyst Average Salary")

boxplot(DataAnalyst$AvgSalaryNorm,main="Analyst Normalized Average Salary")

outliersAnalyst<-boxplot(DataAnalyst$AvgSalaryNorm,plot=FALSE)$out
outliersAnalyst
### Removing 53 outliers from analyst data since we have the most of data analysts
DataAnalystNoOutliers<-DataAnalyst
DataAnalystNoOutliers<- DataAnalystNoOutliers[-which(DataAnalystNoOutliers$AvgSalaryNorm %in% outliersAnalyst),]

########################### Check for normality ############################################
par(mfrow=c(1,2))
plot(density(DataAnalystNoOutliers$AverageSalary),main="Analyst Average Salary")
plot(density(DataAnalystNoOutliers$AvgSalaryNorm),main="Analyst Average Salary Normalized")





###########################################################################################
########################### Data Scientist #########################################
###########################################################################################
##################################Look at correlation between certain skills #####################################
JustSkillsScientist<-DataScientist[,c("python","hadoop","mongo","spark","sql","tableau","kafka","excel","azure","pandas","visualization","machinelearning","aws","saas","etl","warehouse")]
M=cor(JustSkillsScientist)
corrplot(M,method='number')
### very light correlations - Strongest correlations between
#                                  1) Spark and hadoop
#                                  2) Kafka and spark
#                                  3) Tableau and Visualization
###################################################################################################################

######################### Check for Outliers ##############################################
par(mfrow=c(1,2))
boxplotSummary<-boxplot(DataScientist$AverageSalary,main="Scientist Average Salary")

boxplot(DataScientist$AvgSalaryNorm,main="Scientist Normalized Average Salary")

outliersScientist<-boxplot(DataScientist$AvgSalaryNorm,plot=FALSE)$out
outliersScientist
### Almost no outliers in the data scientist data
DataScientistNoOutliers<-DataScientist
DataScientistNoOutliers<- DataScientistNoOutliers[-which(DataScientistNoOutliers$AvgSalaryNorm %in% outliersScientist),]

########################### Check for normality ############################################
par(mfrow=c(1,2))
plot(density(DataScientist$AverageSalary),main="Scientist Average Salary")
plot(density(DataScientist$AvgSalaryNorm),main="Scientist Average Salary Normalized")





###########################################################################################
########################### Data Engineer #########################################
###########################################################################################
##################################Look at correlation between certain skills #####################################
JustSkillsEngineer<-DataEngineer[,c("python","hadoop","mongo","spark","sql","tableau","kafka","excel","azure","pandas","visualization","machinelearning","aws","saas","etl","warehouse")]
M=cor(JustSkillsEngineer)
corrplot(M,method='number')
### very light correlations - Strongest correlations between
#                                  1) Spark and hadoop
#                                  2) Kafka and spark
#                                  3) Tableau and Visualization

################################################################################################################
######################### Check for Outliers ##############################################
par(mfrow=c(1,2))
boxplotSummary<-boxplot(DataEngineer$AverageSalary,main="Engineer Average Salary")

boxplot(DataEngineer$AvgSalaryNorm,main="Engineer Normalized Average Salary")

outliersEngineer<-boxplot(DataEngineer$AvgSalaryNorm,plot=FALSE)$out
outliersEngineer
### 37 outliers in Engineer data that is skewing the data heavily - Removing for this case
DataEngineerNoOutliers<-DataEngineer
DataEngineerNoOutliers<- DataEngineerNoOutliers[-which(DataEngineerNoOutliers$AvgSalaryNorm %in% outliersEngineer),]

########################### Check for normality ############################################
par(mfrow=c(1,2))
plot(density(DataEngineerNoOutliers$AverageSalary),main="Engineer Average Salary")
plot(density(DataEngineerNoOutliers$AvgSalaryNorm),main="Engineer Average Salary Normalized")



################################### Models ########################################################


##############################################################################################
############################# Tree Model #####################################################
##############################################################################################
tree<-rpart(AvgSalaryNorm ~ Senior + CompanySize + Job.Title+ TotalSkills+State,data=FullDataTrain,control=rpart.control(cp=.0001))
printcp(tree)

#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)

prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output


predictionTree<-predict(pruned_tree,FullDataValid)

data.frame(R2 = R2(predictionTree, FullDataValid$AvgSalaryNorm), 
           RMSE = RMSE(predictionTree,  FullDataValid$AvgSalaryNorm), 
           MAE = MAE(predictionTree, FullDataValid$AvgSalaryNorm))




##############################################################################################
############################# Linear Regression Models #####################################################
##############################################################################################

#One model without states as predictors - Job titles and company sizes are significant

LinearRegNoStates<-lm(AvgSalaryNorm ~ Senior + CompanySize + Job.Title+ TotalSkills,data=FullDataTrain)
summary(LinearRegNoStates)
prediction1<-predict(LinearRegNoStates,FullDataTest)

data.frame(R2 = R2(prediction1, FullDataTest$AvgSalaryNorm), 
           RMSE = RMSE(prediction1,  FullDataValid$AvgSalaryNorm), 
           MAE = MAE(prediction1, FullDataValid$AvgSalaryNorm))



#One model with states as predictors

LinearRegStates<-lm(AvgSalaryNorm ~ Senior + CompanySize + Job.Title+ TotalSkills+ State,data=FullDataTrain)
summary(LinearRegStates)
prediction2<-predict(LinearRegStates,FullDataTest)

data.frame(R2 = R2(prediction2, FullDataTest$AvgSalaryNorm), 
           RMSE = RMSE(prediction2,  FullDataValid$AvgSalaryNorm), 
           MAE = MAE(prediction2, FullDataValid$AvgSalaryNorm))



################################### Specific Models based on Job Title##############################################
################################### Using Leave one out Cross Validation due to smaller data sets #########################
##### Data Analyst ################
train_control<-trainControl(method="LOOCV")
Analystmodel<-train(AvgSalaryNorm ~CompanySize+ TotalSkills+ State + sql + excel + python,data=DataAnalystNoOutliers,method="lm",trControl=train_control)
print(Analystmodel)
summary(Analystmodel)

##### Data Scientist ##############
train_control<-trainControl(method="LOOCV")
Scientistmodel<-train(AvgSalaryNorm ~CompanySize+ TotalSkills+ State + sql + machinelearning + python,data=DataScientist,method="lm",trControl=train_control)
print(Scientistmodel)
summary(Scientistmodel)

##### Data Engineer##############
train_control<-trainControl(method="LOOCV")
Engineermodel<-train(AvgSalaryNorm ~CompanySize+ TotalSkills+ State + sql + spark + python,data=DataEngineerNoOutliers,method="lm",trControl=train_control)
print(Engineermodel)
summary(Engineermodel)