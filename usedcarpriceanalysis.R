
#Loading all the libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(car)
library(stringr)
library(readr)
library(psych)
library(zoo)

library(grid)
library(gridExtra)
library(mice)
library(randomForest)
library(MASS)

#Load dataset
usedcarprice <- read_csv("data/data.csv")
#View the data set
View(usedcarprice)
#Summary of data set
summary(usedcarprice)
#Remove "Unknown" string entity from Transmission Type column
usedcarprice <- usedcarprice[-grep("UNKNOWN",usedcarprice$`Transmission Type`),]
summary(usedcarprice$`Transmission Type`)

#Data set published in 2017. So we can find age of car from year
usedcarprice$age<-2017- usedcarprice$Year
summary(usedcarprice$age)

#Conver character data into numeric
usedcarprice=usedcarprice %>% mutate_if(is.character, as.factor)
summary(usedcarprice)

#find NA Value and total of NA Value
map(usedcar, ~which(is.na(.)))
sum(is.na(usedcarprice))

#Remove NA Values
na.aggregate(usedcarprice)
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
replace(usedcarprice, TRUE, lapply(usedcarprice, NA2mean))
usedcarprice[] <- lapply(usedcarprice, NA2mean)

#Recheck NA value
map(usedcarprice, ~which(is.na(.)))
sum(is.na(usedcarprice))

#Remove NA Values different way
usedcarprice <- na.omit(usedcarprice)

#Recheck NA value
map(usedcarprice, ~which(is.na(.)))
sum(is.na(usedcarprice))

#Finding duplicate value
duplicated(usedcarprice)
sum(duplicated(usedcarprice))

#Removing duplicated value
usedcarprice %>% filter(n() > 1)
usedcarprice <- usedcarprice %>% dplyr::distinct()
dim(usedcarprice)
#Finding duplicate value
sum(duplicated(usedcarprice))

# View data set again
view(usedcarprice)


# Select a subset of numeric variables for regression modelling
# We dont prefer to delete the column
price_sub <- subset(usedcarprice, select = c(Make, age, `Engine HP`,`Engine Cylinders`,
                                             `Transmission Type`, `Driven_Wheels`, 
                                             `Number of Doors`, `Vehicle Style`, 
                                             `highway MPG`, `city mpg`,Popularity,MSRP) )


# Visualization corerelation according the new subset of data
pairs.panels(price_sub, col="red")

#
# Develop model
# Spliting data into training and validation samples
# 80% where (train.size)% for training and 20% where (100-train.size)% for validation
# Convert to numeric for for model
price_sub=price_sub %>% mutate_if(is.factor, as.numeric)
view(price_sub)
summary(price_sub)

set.seed(2019)
train.size <- 0.8
train.index <- sample.int(length(price_sub$MSRP), round(length(price_sub$MSRP) * train.size))
train.sample <- price_sub[train.index,]
valid.sample <- price_sub[-train.index,]

# We do a stepwise selection of variables by backwards elimination
# We consider all candidate variables and eliminate one at the time
# Fit the model and summary - visualise by ploting
fit <- lm(MSRP ~ Make+ age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)
crPlots(fit)

# Eliminate extreme values means outlier seen from the plot
# Cook's D plot, cutoff as 4/(n-k-1)
# identify D values > cutoff
# Row names discovered in 2 rounds
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("5022", "1404", "4118")),]
#Re-Fit the Model 2
fit <- lm(MSRP ~ Make+ age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("17091", "2981", "8507")),] 
#Re-Fit model 3
fit <- lm(MSRP ~ Make+age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                        
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("1709", "7714", "8854")),] 
#Re-Fit model 4
fit <- lm(MSRP ~ Make+age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)
# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                        
plot(fit, which=5, cook.levels=cutoff)                        
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("3859", "6723", "7884")),] 
#Re-Fit model 5
fit <- lm(MSRP ~ Make+ age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                        
plot(fit, which=5, cook.levels=cutoff)                        
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("851", "1818", "5318")),] 
#Re-Fit model 6
fit <- lm(MSRP ~ Make+age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                        
plot(fit, which=5, cook.levels=cutoff)                        
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("1774", "6047", "7156")),] 
#Re-Fit model 7
fit <- lm(MSRP ~ Make+age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)
# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                        
plot(fit, which=5, cook.levels=cutoff)                        
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("3973", "7938", "8467")),] 
#Re-Fit model 8
fit <- lm(MSRP ~ Make+age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                        
plot(fit, which=5, cook.levels=cutoff)                        
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("3565", "3983", "5198")),] 
#Re-Fit model 9
fit <- lm(MSRP ~ Make+age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                        
plot(fit, which=5, cook.levels=cutoff)                        
train.sample <- train.sample[-which(rownames(train.sample)    
                                    %in% c("2951", "7603", "8174")),] 
#Re-Fit model 10
fit <- lm(MSRP ~ Make+ age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)                       
plot(fit, which=5, cook.levels=cutoff)                       
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("1855", "4126", "7007")),] 
#Re-Fit model 11
fit <- lm(MSRP ~ Make+age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            `city mpg`+Popularity, data=train.sample)
summary(fit)

# Re-eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff) 
# Now we have seen there is no outliers
# We will drop Make and city mpg from the subset. We will make the model without those
fit <- lm(MSRP ~ age+`Engine HP`+`Engine Cylinders`+`Transmission Type`+
            `Driven_Wheels`+`Number of Doors`+`highway MPG`+
            Popularity, data=train.sample)
summary(fit)

#Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
vif(fit)


##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.Price <- predict(fit, 
                                   newdata = subset(train.sample, select=c(MSRP, age,`Engine HP`,`Engine Cylinders`,`Transmission Type`,
                                                                           `Driven_Wheels`,`Number of Doors`,`highway MPG`,
                                                                           `city mpg`,Popularity)))
valid.sample$Pred.Price <- predict(fit, 
                                   newdata = subset(valid.sample, select=c(MSRP, age,`Engine HP`,`Engine Cylinders`,`Transmission Type`,
                                                                           `Driven_Wheels`,`Number of Doors`,`highway MPG`,
                                                                           `city mpg`,Popularity)))

# The theoretical model performance is defined here as R-Squared
summary(fit)
# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Price, train.sample$MSRP), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.Price - train.sample$MSRP)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.Price - train.sample$MSRP)))
c(train.corr^2, train.RMSE, train.MAE)

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$MSRP), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.Price - valid.sample$MSRP)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.Price - valid.sample$MSRP)))
c(valid.corr^2, valid.RMSE, valid.MAE)


# Random Forest


price_sub$EngineHP<-price_sub$`Engine HP`
price_sub$EngineCylinders<-price_sub$`Engine Cylinders`
price_sub$Transmission<-price_sub$`Transmission Type`
price_sub$DrivenWheel<-price_sub$Driven_Wheels
price_sub$NumberDoor<-price_sub$`Number of Doors`
price_sub$vehicleStyle<-price_sub$`Vehicle Style`
price_sub$highwayMPG<-price_sub$`highway MPG`


set.seed(2019)
train.size <- 0.8
train.index <- sample.int(length(price_sub$MSRP), round(length(price_sub$MSRP) * train.size))
train.sample <- price_sub[train.index,]
valid.sample <- price_sub[-train.index,]

# create model
rf.model <- randomForest(MSRP ~ age+EngineHP+EngineCylinders+Transmission+
                           DrivenWheel+NumberDoor+
                           Popularity, data=train.sample,
                         importance = T,
                         ntree = 1000,
                         mtry = 3)

summary(rf.model)

# make predictions using test data
rf.predict <- predict(rf.model, newdata = valid.sample, type = 'response')
rf.predict
library(formattable)
# test df
formattable(head(data.frame(valid.sample, rf.predict)), 
            list(rf.predict = formatter("span",
                                        style = x ~ style(color = ifelse( x == 1, "green", "gray")))))
# extract importance to data frame  
importance <- as.data.frame(rf.model$importance)
importance <- rownames_to_column(importance, var = 'Feature')

# MeanDecreaseAccuracy
mda <- ggplot(importance, 
              aes(reorder(Feature,MeanDecreaseAccuracy), MeanDecreaseAccuracy)) +
  geom_point(stat = 'identity') +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=Feature, 
                   xend=Feature, 
                   y=min(MeanDecreaseAccuracy), 
                   yend=max(MeanDecreaseAccuracy)), 
               linetype="dashed", 
               size=0.1) +
  labs(x='Feature',
       y='Mean Decrease in Accuracy') +
  coord_flip() +
  theme_classic(base_family = 'Roboto Condensed', base_size = 12)

# calculate misclassification error
misClasificError <- mean(rf.predict != valid.sample$MSRP)
print(paste('Accuracy:',percent(1-misClasificError, 2)))

#confusion matrix
cm <- table(valid.sample$MSRP, rf.predict == 1)
formattable(as.data.frame.matrix(cm))
