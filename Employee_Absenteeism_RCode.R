rm(list=ls())


x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
     "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees', 'readxl')


lapply(x, require, character.only = TRUE)
rm(x)

#Reading the Data
Empabs_data = read_xls("C:/Users/npava/Desktop/Employee_Absentism_Project/Absenteeism_at_work_Project.xls")

Empabs_data = as.data.frame(Empabs_data)

#Missing Value Analysis
missing_values = data.frame(apply(Empabs_data, 2, function(x) {sum(is.na(x))}))
sum(missing_values)
new_DF <- Empabs_data[rowSums(is.na(Empabs_data)) > 0,]
Columns_DF <- Empabs_data[colSums(is.na(Empabs_data)) > 0,]
sum(is.na(Empabs_data))

cnames <- c('Distance_from_Residence_to_Work', 'Service_time', 'Age', 'Work_load_Average_day', 'Transportation_expense',
            'Hit_target', 'Son', 'Pet', 'Weight', 'Height','Absenteeism_time_in_hours','Body_mass_index')                          

cat_names <- c('Social_smoker','Month_of_absence','Social_drinker','Reason_for_absence','Disciplinary_failure','ID','Education','Seasons','Day_of_the_week')


#Checking the summary
summary(Empabs_data[,cnames])
lapply(Empabs_data[,cnames], function(feat) length(unique(feat)))
str(Empabs_data)


for(i in cnames){
  Empabs_data[,i] = as.factor(Empabs_data[,i])
}

str(Empabs_data)

library(mice)

library('missForest')
Imputed_values = mice(Empabs_data, method = 'rf', seed = 1 )
Imputed_values_output = complete(Imputed_values)
anyNA(Imputed_values_output)
sum(is.na(Imputed_values_output))

write.csv(Imputed_values_output, "C:/Users/npava/Desktop/Employee_Absentism_Project/Miceoutput.csv", row.names = F )
miceOutput=read.csv ("C:/Users/npava/Desktop/Employee_Absentism_Project/Miceoutput.csv")

data = miceOutput


#Data exploration


library(ggthemes)
library(grid)
library(gridExtra)

p <- ggplot(data, aes(x = Pet, fill = Pet)) + geom_bar() 
s <- ggplot(data, aes(x = Son, fill = Son)) + geom_bar()

SS <- ggplot(data, aes(x =  Social_smoker, fill =  Social_drinker)) + geom_bar() 

S <- ggplot(data, aes(x =   Seasons,fill = Seasons)) + geom_bar()

grid.arrange(p,s, nrow = 1)
grid.arrange(SS,S, nrow = 1)

library(dplyr)

absent <- as.data.frame( data %>% select(everything()) %>% filter(Absenteeism_time_in_hours > 0))
Reason <-  as.data.frame(absent %>% group_by(Reason_for_absence) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))
ggplot(Reason,aes(x = reorder(Reason_for_absence,percent), y= percent, pos=3, xpd=NA, fill= Reason_for_absence)) + geom_bar(stat = 'identity') + coord_flip() + theme(legend.position='none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + xlab('Reason for absence')


a <- ggplot(data, aes(x = Age, fill = Son)) + geom_bar()            
grid.arrange(a, nrow = 1)

##Outlier Analysis



for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = data)+
           stat_boxplot(geom = "boxplot", width = 0.1)+
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=2, notch=FALSE) +
           theme(axis.text.x =element_blank(),
                 axis.ticks.x=element_blank(),axis.title.y =element_blank())+
           ggtitle(paste("Box plot of",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn10,gn11,gn12,gn3,gn2,gn4,gn5,gn6,gn7,gn8,gn9,nrow= 4, ncol=4)



for (i in cnames)
{
  print(i)
  value=data[,i][data[,i] %in% boxplot.stats(data[,i],coef=1.5)$out]
  print(value)
  data[,i][data[,i] %in% value] = NA
}

#Imputing outliers using MICE method   
micemethod_2 <- mice(data, method="rf", seed=1)  # perform mice imputation, based on random forests.
miceOutput_2 <- complete(micemethod_2)  # generate the completed data.
anyNA(miceOutput_2)
data = miceOutput_2

write.csv(miceOutput_2, "C:/Users/npava/Desktop/Employee_Absentism_Project/Miceoutput_2.csv", row.names = F )
write.csv(miceOutput, "C:/Users/npava/Desktop/Employee_Absentism_Project/Miceoutput_1.csv", row.names = F )


corrgram(data[,cnames], order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot for Numerical Variables")

#ANOVA Analysis
anova_multi_way <- aov(Absenteeism_time_in_hours~(Social_smoker)+
                         (Month_of_absence)+
                         (Social_drinker)+
                         (Reason_for_absence)+
                         (Disciplinary_failure)+
                         (ID)+
                         (Education)+
                         (Seasons)+
                         (Day_of_the_week), data = data)
summary(anova_multi_way)

## Dimension Reduction
data = subset(data,select = -c(Body_mass_index,ID))


#Feature Scaling
cnames <- c('Distance_from_Residence_to_Work', 'Service_time', 'Age', 'Work_load_Average_day', 'Transportation_expense',
            'Hit_target', 'Son', 'Pet', 'Weight', 'Height')                          

cat_names <- c('Social_smoker',
               'Month_of_absence',
               'Social_drinker',
               'Reason_for_absence',
               'Disciplinary_failure',
               'Education',
               'Seasons',
               'Day_of_the_week')
for (i in cat_names){
  
  data[,i] = as.factor(data[,i]) 
}

#Scaling all numeric variables leaving our target variable untouched
for(i in cnames){
  print(i)
  data[,i] = as.numeric(data[,i]) 
  data[,i] = (data[,i] - min(data[,i]))/
    (max(data[,i] - min(data[,i])))
}
str(data)
anyNA(data)


#Removing the rows containing absurd information
data= data[!data$Month_of_absence==0 & !data$Reason_for_absence==0, ]

#Creating dummies for categorical variables
DFdummies <- as.data.frame(model.matrix(~. -1, data))
dim(DFdummies)

library(DataCombine)
rmExcept(c("marketing_train","DFdummies","miceoutput_2")) 

wdf=data
data=DFdummies

#Creating train and test data
library(caret)
set.seed(1)
train.index = createDataPartition(data$Absenteeism_time_in_hours, p = .80, list = FALSE)
X_train = data[ train.index,]
y_train  = data[-train.index,]
y_test=y_train$Absenteeism_time_in_hours
X_test=X_train$Absenteeism_time_in_hours

library(usdm)


#Creating a function to run all types of regression and comparing the values
Show.RMSE <- function(method, train_data, test_data){
  regressor_fit <- caret::train(Absenteeism_time_in_hours~., data = X_train, method = method)
  
  y_pred <- predict(regressor_fit, y_train)
  print("RMSE value of test data")
  print(caret::RMSE(y_test, y_pred)) 
}
require(gbm)
library (ridge)
library(enet)
library(elasticnet)
library(h20)
regressors=c('lm','knn','svmLinear3', 'rpart2','rf','xgbTree','ridge')

#Running all the regressions and checking the performance on test data
for(i in regressors){
  print(i)
  Show.RMSE(i, X_train, y_test) 
  print(strrep('-',50))
}



