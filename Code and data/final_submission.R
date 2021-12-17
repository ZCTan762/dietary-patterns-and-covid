#MGSC 661: Final Project 

# Read in data files 
# % of food intake (kg) in countries around the world
food_supply_o <- read.csv("Food_Supply_Quantity_kg_Data.csv")

# % of energy intake (kcal) from different types of food in countries around the world. 
kcal_intake_o <- read.csv("Food_Supply_kcal_Data.csv")

# % of fat intake from different types of food in countries around the world
fat_intake_o <- read.csv("Fat_Supply_Quantity_Data.csv")

# % of protein intake from different types of food in countries around the world
protein_intake_o <- read.csv("Protein_Supply_Quantity_Data.csv")

# Governance data
governance_o <- read.csv("REIGN_2021.csv")

# Income data
income_o <- read.csv("income_group_mod.csv")

# Select only some columns
food_supply <- food_supply_o[, c(2:31)]
kcal_intake <- kcal_intake_o[, c(2:24)]
fat_intake <- fat_intake_o[, c(2:24)]
protein_intake <- protein_intake_o[, c(2:24)]
governance <- governance_o[, c(2,3,5)]
income <- income_o[, c(2:2)]

# Function to output new column names 
rename_columns <- function(prefix, dataframe){
  new_names <- c()
  for (i in colnames(dataframe)){
    i = paste(prefix, i, sep = "")
    new_names <- c(new_names, i)
  }
  return(new_names)
}


# Rename column names 
colnames(kcal_intake) <- rename_columns("kcal_",kcal_intake)
colnames(fat_intake) <- rename_columns("fat_",fat_intake)
colnames(protein_intake) <- rename_columns("protein_",protein_intake)


# cbind function to combine dfs
combined <- cbind(food_supply, kcal_intake, fat_intake, protein_intake, income, governance)
attach(combined)


##### Preprocessing #####

# Keep only Deaths as a target predictor
drop_list <- c('Active', 'Recovered', 'Confirmed')
#drop_list <- c('Active', 'Deaths', 'Confirmed')
#drop_list <- c('Active', 'Deaths', 'Recovered')
combined <- combined[ , !names(combined) %in% drop_list]

# check for nulls 
nulls <- colSums(is.na(combined))
na_count <- data.frame( nulls)
na_count

# Drop rows with NAs in target var
library(tidyr)

combined <- combined %>% drop_na('Deaths')
#combined <- combined %>% drop_na('Recovered')
#combined <- combined %>% drop_na('Confirmed')

# Exclude Undernourished column for numerical analyses
combined_numeric <- combined[,c(-25,-97)]

# Move NA
#

# Create correlation matrix
# cor_matrix <- cor(combined_numeric, method = c("pearson"))
# write.csv(round(cor_matrix, 3), file = "cormatrix.csv")

### Drop correlated variables
cor_drop_list <- c('Milk...Excluding.Butter', 'kcal_Alcoholic.Beverages', 
                   'protein_Alcoholic.Beverages', 'kcal_Animal.fats', 'fat_Animal.fats',
                   'protein_Animal.fats', 'protein_Milk...Excluding.Butter', 
                   'protein_Animal.Products', 'kcal_Milk...Excluding.Butter',
                   'kcal_Animal.Products', 'kcal_Aquatic.Products..Other',
                   'fat_Aquatic.Products..Other', 'protein_Aquatic.Products..Other',
                   'protein_Cereals...Excluding.Beer','kcal_Cereals...Excluding.Beer',
                   'kcal_Eggs', 'kcal_Fish..Seafood', 'kcal_Fruits...Excluding.Wine',
                   'kcal_Meat', 'kcal_Miscellaneous', 'kcal_Offals', 'kcal_Oilcrops', 
                   'kcal_Pulses', 'kcal_Spices', 'kcal_Starchy.Roots', 'kcal_Sugar.Crops', 
                   'kcal_Treenuts', 'kcal_Vegetal.Products', 'kcal_Vegetables', 'fat_Eggs',
                   'fat_Fish..Seafood', 'fat_Miscellaneous', 'fat_Offals', 'fat_Oilcrops',
                   'fat_Pulses', 'fat_Starchy.Roots', 'fat_Sugar.Crops', 'fat_Treenuts',
                   'fat_Vegetable.Oils','fat_Vegetables', 'protein_Eggs', 'protein_Fish..Seafood',
                   'protein_Fruits...Excluding.Wine', 'protein_Meat', 'protein_Offals',
                   'protein_Pulses', 'protein_Spices', 'protein_Starchy.Roots', 
                   'protein_Stimulants', 'protein_Sugar.Crops', 'protein_Treenuts',
                   'protein_Vegetables','protein_Miscellaneous','kcal_Vegetable.Oils',
                   'fat_Spices','kcal_Stimulants','fat_Animal.Products',
                   'fat_Alcoholic.Beverages','protein_Vegetal.Products')
combined <- combined[ , !names(combined) %in% cor_drop_list]
combined_numeric <- combined_numeric[ , !names(combined_numeric) %in% cor_drop_list]

# Check correlation once more 
cor_matrix <- cor(combined_numeric, method = c("pearson"))
# write.csv(round(cor_matrix, 3), file = "cormatrix2.csv")

# Plot correlation heatmap
library(ggplot2)
library(reshape2)
melted_cormat <- melt(cor_matrix)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5), axis.text.y = element_text( size = 5))

# Treat Undernourished - Replace all '<2.5' with 1.5 and divide into 3 bins by value
combined$Undernourished=ifelse(combined$Undernourished=="<2.5",1.5, combined$Undernourished)
#combined$Undernourished = as.numeric(combined$Undernourished)
combined$Undernourished <- cut(combined$Undernourished, breaks = 3, labels = c("Low", "Mid", "High"))

# combined$income=ifelse(combined$income=="","No Data", combined$income)
# combined$income[combined$income==""] <- NA
# combined$income <- factor(ifelse(is.na(combined$income), 
#                                          "No Data", paste(combined$income)), 
#                                          levels = c(levels(combined$income), "No Data"))


##### Regression Random Forest #####

library(randomForest)

### Identify variable importance
x <- list()
y <- list()
for (j in 1:10)
{
  for (i in 1:20)
  {
    impforest_r=randomForest(combined$Deaths~., data=combined, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    # impforest_r=randomForest(combined$Recovered~., data=combined, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    #impforest_r=randomForest(combined$Confirmed~., data=combined, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    
    pred_importance <- importance(impforest_r)
    pred_importance <- pred_importance[order(pred_importance[,1],decreasing=TRUE),]
    x[[i]] <- pred_importance[,1]
  }
  y[[j]] <- rowMeans(cbind(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], x[[8]], x[[9]], 
                           x[[10]], x[[11]], x[[12]], x[[13]], x[[14]], x[[15]], x[[16]],
                           x[[17]], x[[18]], x[[19]], x[[20]]))
}

importance_reg <- data.frame(sort(rowMeans(cbind(y[[1]], y[[2]], y[[3]], y[[4]], y[[5]], 
                                                 y[[6]], y[[7]], y[[8]], y[[9]], 
                                                 y[[10]])),decreasing = T))

#write.csv(importance_reg, file = "importance_reg_recovered.csv")

### Drop insignificant predictors
# rf_drop_list <- c('Undernourished', 'Stimulants', 'fat_Meat', 'Aquatic.Products..Other',
#                       'fat_Sugar...Sweeteners','fat_Stimulants', 'Offals', 'Population',
#                       'Fruits...Excluding.Wine','Aquatic.Products..Other', 'Vegetables',
#                       'Starchy.Roots')
# 
# combined_filtered <- combined[ , !names(combined) %in% rf_drop_list]

### APPROACH 2: Choose significant predictors
combined_selected <- combined[,c("Deaths","Obesity","Eggs","Alcoholic.Beverages",
"Oilcrops","fat_Vegetal.Products","Animal.Products",
"Vegetal.Products")]

# combined_selected <- combined[,c("Recovered","fat_Stimulants", "Obesity","Eggs",
#                                  "Fish..Seafood")]

# combined_selected <- combined[,c("Confirmed", "Obesity","Eggs","Vegetal.Products",
#                                  "Animal.Products")]

### Find importance again
x <- list()
y <- list()
for (j in 1:10)
{
  for (i in 1:20)
  {
    impforest_r=randomForest(combined_selected$Deaths~., data=combined_selected, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    #impforest_r=randomForest(combined_selected$Recovered~., data=combined_selected, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    #impforest_r=randomForest(combined_selected$Confirmed~., data=combined_selected, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    pred_importance <- importance(impforest_r)
    pred_importance <- pred_importance[order(pred_importance[,1],decreasing=TRUE),]
    x[[i]] <- pred_importance[,1]
  }
  y[[j]] <- rowMeans(cbind(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], x[[8]], x[[9]], 
                           x[[10]], x[[11]], x[[12]], x[[13]], x[[14]], x[[15]], x[[16]],
                           x[[17]], x[[18]], x[[19]], x[[20]]))
}

sort(rowMeans(cbind(y[[1]], y[[2]], y[[3]], y[[4]], y[[5]], y[[6]], y[[7]], y[[8]], y[[9]], 
                    y[[10]])),decreasing = T)


##### Testing - Regression Random Forest #####
set.seed(13)
require(caTools)
accuracy_list = rep(NA, 50)
r2_list = rep(NA, 50)
for (i in 1:50)
{
  # Train test split
  sample = sample.split(combined_selected$Deaths, SplitRatio = .7)
  #sample = sample.split(combined_selected$Recovered, SplitRatio = .7)
  #sample = sample.split(combined_selected$Confirmed, SplitRatio = .7)
  train = subset(combined_selected, sample == TRUE)
  test  = subset(combined_selected, sample == FALSE)
  
  # Build model with training data
  testforest=randomForest(train$Deaths~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)
  #testforest=randomForest(train$Recovered~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)
  # testforest=randomForest(train$Confirmed~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)
  
  # Run predictions
  predicted_score = predict(testforest, newdata=test)
  r2_list[i] <- mean(testforest$rsq)
  accuracy_list[i] <- mean((predicted_score - test$Deaths)^2)
  #accuracy_list[i] <- mean((predicted_score - test$Recovered)^2)
  # accuracy_list[i] <- mean((predicted_score - test$Confirmed)^2)
}
mean(accuracy_list)
mean(r2_list)


##### Categorical Random Forest #####

library(randomForest)
combinedcat <- data.frame(combined)
#tracemem(combinedcat)==tracemem(combined)

# Split target into 3 bins
library(Hmisc)
combinedcat$Deaths_cat <- cut2(combinedcat$Deaths, g=3, labels = c("Low Risk", "Medium Risk", "High Risk"))
combinedcat$Deaths_cat <- as.character(combinedcat$Deaths_cat)
combinedcat$Deaths_cat[combinedcat$Deaths_cat == "[0.00000,0.00438)"] <- "Low Risk"
combinedcat$Deaths_cat[combinedcat$Deaths_cat == "[0.00438,0.05095)"] <- "Medium Risk"
combinedcat$Deaths_cat[combinedcat$Deaths_cat == "[0.05095,0.18543]"] <- "High Risk"
combinedcat$Deaths_cat <- as.factor(combinedcat$Deaths_cat)

# combinedcat$Recovered_cat <- cut2(combinedcat$Recovered, g=3, labels = c("Low", "Medium", "High"))
# combinedcat$Recovered_cat <- as.character(combinedcat$Recovered_cat)
# combinedcat$Recovered_cat[combinedcat$Recovered_cat == "[0.000,0.153)"] <- "Low"
# combinedcat$Recovered_cat[combinedcat$Recovered_cat == "[0.153,1.508)"] <- "Medium"
# combinedcat$Recovered_cat[combinedcat$Recovered_cat == "[1.508,9.040]"] <- "High"
# combinedcat$Recovered_cat <- as.factor(combinedcat$Recovered_cat)
# 
# combinedcat$Confirmed_cat <- cut2(combinedcat$Confirmed, g=3, labels = c("Low Risk", "Medium Risk", "High Risk"))
# combinedcat$Confirmed_cat <- as.character(combinedcat$Confirmed_cat)
# combinedcat$Confirmed_cat[combinedcat$Confirmed_cat == "[0.000312, 0.251)"] <- "Low Risk"
# combinedcat$Confirmed_cat[combinedcat$Confirmed_cat == "[0.250961, 2.663)"] <- "Medium Risk"
# combinedcat$Confirmed_cat[combinedcat$Confirmed_cat == "[2.663104,10.408]"] <- "High Risk"
# combinedcat$Confirmed_cat <- as.factor(combinedcat$Confirmed_cat)



# Drop deaths column
drop_list <- c('Deaths')
#drop_list <- c('Confirmed')
# drop_list <- c('Recovered')
combinedcat <- combinedcat[ , !names(combinedcat) %in% drop_list]

### Identify variable importance
x <- list()
y <- list()
for (j in 1:10)
{
  for (i in 1:20)
  {
    impforest=randomForest(combinedcat$Deaths_cat~., data=combinedcat, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    #impforest=randomForest(combinedcat$Recovered_cat~., data=combinedcat, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    #impforest=randomForest(combinedcat$Confirmed_cat~., data=combinedcat, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    
    pred_importance <- importance(impforest)
    pred_importance <- pred_importance[order(pred_importance[,1],decreasing=TRUE),]
    x[[i]] <- pred_importance[,4]
  }
  y[[j]] <- rowMeans(cbind(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], x[[8]], x[[9]], 
                           x[[10]], x[[11]], x[[12]], x[[13]], x[[14]], x[[15]], x[[16]],
                           x[[17]], x[[18]], x[[19]], x[[20]]))
}

importance_cat <- data.frame(sort(rowMeans(cbind(y[[1]], y[[2]], y[[3]], y[[4]], y[[5]], 
                                                 y[[6]], y[[7]], y[[8]], y[[9]], 
                                                 y[[10]])),decreasing = T))

#write.csv(importance_cat, file = "importance_cat_deaths.csv")

### Drop insignificant predictors
# rf_drop_list_cat <- c('Undernourished', 'Stimulants', 'fat_Meat', 'Aquatic.Products..Other',
#                   'fat_Sugar...Sweeteners','fat_Stimulants', 'Offals', 'Population',
#                   'Fruits...Excluding.Wine','Aquatic.Products..Other', 'Vegetables',
#                   'Starchy.Roots')
# 
# combinedcat_filtered <- combinedcat[ , !names(combinedcat) %in% rf_drop_list_cat]

### APPROACH 2: Choose significant predictors
combinedcat_selected <- combinedcat[,c("Deaths_cat","Obesity", "Eggs", "Vegetal.Products",
                                       "Animal.fats","Animal.Products","Oilcrops")]

# combinedcat_selected <- combinedcat[,c("Recovered_cat","Eggs","protein_Vegetable.Oils",
#                                        "kcal_Sugar...Sweeteners")]

# combinedcat_selected <- combinedcat[,c("Confirmed_cat","Eggs","Obesity","Animal.Products",
#                                        "Vegetal.Products","fat_Milk...Excluding.Butter", 
#                                        "fat_Vegetal.Products")]
# "Oilcrops","kcal_Sugar...Sweeteners",
# "Treenuts", "protein_Oilcrops")]


### Find importance again
x <- list()
y <- list()
for (j in 1:10)
{
  for (i in 1:20)
  {
    impforest=randomForest(combinedcat_selected$Deaths_cat~., data=combinedcat_selected, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    
    pred_importance <- importance(impforest)
    pred_importance <- pred_importance[order(pred_importance[,1],decreasing=TRUE),]
    x[[i]] <- pred_importance[,4]
  }
  y[[j]] <- rowMeans(cbind(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], x[[8]], x[[9]], 
                           x[[10]], x[[11]], x[[12]], x[[13]], x[[14]], x[[15]], x[[16]],
                           x[[17]], x[[18]], x[[19]], x[[20]]))
}

sort(rowMeans(cbind(y[[1]], y[[2]], y[[3]], y[[4]], y[[5]], y[[6]], y[[7]], y[[8]], y[[9]], 
                    y[[10]])),decreasing = T)


##### Testing - Categorical Random Forest #####
set.seed(13)
require(caTools)
require(caret)
accuracy_list = rep(NA, 50)
for (i in 1:50)
{
  # Train test split
  sample = sample.split(combinedcat_selected$Deaths_cat, SplitRatio = .7)
  # sample = sample.split(combinedcat_selected$Recovered_cat, SplitRatio = .7)
  # sample = sample.split(combinedcat_selected$Confirmed_cat, SplitRatio = .7)
  train = subset(combinedcat_selected, sample == TRUE)
  test  = subset(combinedcat_selected, sample == FALSE)
  
  # Build model with training data
  testforestcat=randomForest(train$Deaths_cat~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)
  #testforestcat=randomForest(train$Recovered_cat~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)
  
  # Run predictions
  predicted_score = predict(testforestcat, newdata=test)
  
  #confusionMatrix(predicted_score, test$Deaths_cat)    # For classification forest
  #cm$overall['Accuracy']
  accuracy_list[i] <- confusionMatrix(predicted_score, test$Deaths_cat)$overall['Accuracy']
  #accuracy_list[i] <- confusionMatrix(predicted_score, test$Recovered_cat)$overall['Accuracy']
}
mean(accuracy_list)


##### Making predictions #####
predict(testforestcat,data.frame(Obesity=20.4,Animal.Products=20.0,Eggs=2.4,
                                 Animal.fats = 0.67, Vegetal.Products=0.5, Fish..Seafood=9.0,
                                 fat_Milk...Excluding.Butter = 15.0,kcal_Sugar...Sweeteners = 7,
                                 Treenuts = 0.54, fat_Cereals...Excluding.Beer = 7.0,
                                 Alcoholic.Beverages = 3.0))

##### Random Forest: Importance Score Visualization #####

#packages required visualization 
#install.packages('devtools')
#devtools::install_github('bbc/bbplot', force = TRUE)

#load necessary packages
#if(!require(pacman))install.packages("pacman")

# pacman::p_load('dplyr', 'tidyr', 'gapminder',
#                'ggplot2',  'ggalt',
#                'forcats', 'R.utils', 'png',
#                'grid', 'ggpubr', 'scales',
#                'bbplot')

importance_plot <- function(importance, var){
  nums <- seq(1,nrow(importance))
  importance$nums <- nums
  colnames(importance) <- c('Variable', 'Mean_Decrease_Accuracy', 'Index')
  #colnames(importance) <- c('Variable', 'Mean_Decrease_Accuracy')
  #importance_f <- subset(importance, select = c(-Variable))
  
  #plotting only top 15
  importance_f <- importance[1:15,]
  head(importance_f)
  #mean decrease
  
  library(bbplot)
  library(ggplot2)
  fin_plot <- ggplot(data = importance_f, aes(x = Index, y = Mean_Decrease_Accuracy, label = Variable))+ 
    geom_line()+
    geom_point()+
    xlab("Top 15 Predictors") +
    ylab("Mean Decrease in Accuracy") +
    labs(title = "Finding the Most Significant Predictors", subtitle = paste0("Random Forest: Importance Score, Target Variable: ",var))+ 
    theme( plot.title=element_text(size=15), plot.subtitle=element_text(size=10), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))+
    geom_vline(aes(xintercept=10), color="blue", linetype="dashed", size=0.5)+
    geom_hline(aes(yintercept=9), color="orange", linetype="dashed", size=0.5)+
    geom_text(nudge_y = 0.3, size = 2)
  
  print(fin_plot)
}

### Read saved importance values and print to console

# Importance values for target Confirmed - Categorical Forest
importance <- read.csv("importance_reg_confirmed.csv")
importance_plot(importance, "Confirmed (Regression)")

# Importance values for target Recovered - Categorical Forest
importance <- read.csv("importance_cat_recovered.csv")
importance_plot(importance, "Recovered (Categorical)")

# Importance values for target Deaths - Categorical Forest
importance <- read.csv("importance_cat_deaths.csv")
importance_plot(importance, "Deaths (Categorical)")

# Importance values for target Deaths - Regression Forest
importance <- read.csv("importance_reg_deaths.csv")
importance_plot(importance, "Deaths (Regression)")

# Importance values for target Recovered - Regression Forest
importance <- read.csv("importance_reg_recovered.csv")
importance_plot(importance, "Recovered (Regression)")

# Importance values for target Confirmed - Regression Forest
importance <- read.csv("importance_cat_confirmed.csv")
importance_plot(importance, "Confirmed (Categorical)")

##### Gradient Boosting #####

library(gbm)

sample = sample.split(combined_selected$Deaths, SplitRatio = .7)
train = subset(combined_selected, sample == TRUE)
test  = subset(combined_selected, sample == FALSE)

boosted=gbm(train$Deaths~., data = train,
            distribution="gaussian",n.trees=10000, interaction.depth=4)
summary(boosted)

predicted_score=predict(boosted, newdata=test, n.trees=10000)
mean((predicted_score - test$Deaths)^2)

##### K-MEANS CLUSTERING #####

#as only features from the dietary and covid impact dataset were used 
#data would have to be re-loaded and re-processed for k-means 

#loading data
# % of food intake (kg) in countries around the world
food_supply_o <- read.csv("Food_Supply_Quantity_kg_Data.csv")
# % of energy intake (kcal) from different types of food in countries around the world. 
kcal_intake_o <- read.csv("Food_Supply_kcal_Data.csv")
# % of fat intake from different types of food in countries around the world
fat_intake_o <- read.csv("Fat_Supply_Quantity_Data.csv")
# % of protein intake from different types of food in countries around the world
protein_intake_o <- read.csv("Protein_Supply_Quantity_Data.csv")

# Select only required columns
#country <- food_supply_o[, 1]
food_supply <- food_supply_o[, c(1:31)] #not dropping countries here 
kcal_intake <- kcal_intake_o[, c(2:24)]
fat_intake <- fat_intake_o[, c(2:24)]
protein_intake <- protein_intake_o[, c(2:24)]

#renaming columns
#defining the function to output new columns names 
rename_columns <- function(prefix, dataframe){
  new_names <- c()
  for (i in colnames(dataframe)){
    i = paste(prefix, i, sep = "")
    new_names <- c(new_names, i)
  }
  return(new_names)
}

#renaming column names of the dataframes
colnames(kcal_intake) <- rename_columns("kcal_",kcal_intake)
colnames(fat_intake) <- rename_columns("fat_",fat_intake)
colnames(protein_intake) <- rename_columns("protein_",protein_intake)

#creating a merged dataframe 
# cbind function to combine dfs
combined <- cbind(food_supply, kcal_intake, fat_intake, protein_intake)
names(combined)
ncol(combined)

#creating index
rownames(combined) <- food_supply_o[, 1]
head(combined)

#transforming undernourished variable 
combined$Undernourished=ifelse(combined$Undernourished=="<2.5",1.5, combined$Undernourished)
#convert char to numeric
combined$Undernourished = as.numeric(combined$Undernourished)
# Treat Undernourished - Replace all '<2.5' with 1.5. And divide into 3 bins by value
combined$Undernourished <- cut(combined$Undernourished, breaks = 3, labels = c("Low", "Mid", "High"))

#processing null values 
#dropping active, confirmed, recovered
drop_list <- c('Active', 'Recovered', 'Confirmed') 
combined <- combined[ , !names(combined) %in% drop_list]

#dropping null values from target variable 
library(tidyr)
combined <- combined %>% drop_na('Deaths')

#creating a dataframe for count of nulls in each column in the combined dataframe 
nulls <- colSums(is.na(combined))
na_count <- data.frame( nulls)

library(dplyr) 
na_count %>% filter(nulls > 0)

#data with labels
full_data <- combined
rownames(full_data) <- c()

#data without labels for modelling
combined <- subset(combined,select = -c(Country))

#target variable transformation
#adding Deaths as a categorical variable 
library(Hmisc)
combined$Deaths_cat <- cut2(combined$Deaths, g=3, labels = c("Low Risk", "Medium Risk", "High Risk"))
combined$Deaths_cat <- as.character(combined$Deaths_cat)

table(combined$Deaths_cat)

combined$Deaths_cat[combined$Deaths_cat == "[0.00000,0.00438)"] <- "Low Risk"
combined$Deaths_cat[combined$Deaths_cat == "[0.00438,0.05095)"] <- "Medium Risk"
combined$Deaths_cat[combined$Deaths_cat == "[0.05095,0.18543]"] <- "High Risk"

combined$Deaths_cat <- as.factor(combined$Deaths_cat)

#Creating a function for Data Pre-processing for K-Means
#defining a function that creates dummies and scales data
kmeans_processing <- function(combined)
{#dummifying undernourished & dropping the categorical variable
  library(fastDummies)
  if ('Undernourished' %in% colnames(combined)){
    combined <- dummy_cols(combined, select_columns = 'Undernourished', remove_first_dummy = TRUE, remove_selected_columns = TRUE)}
  
  if ('Deaths_cat' %in% colnames(combined)){
    #dummifying deaths_cat & dropping the categorical variable
    combined <- dummy_cols(combined, select_columns = 'Deaths_cat', remove_first_dummy = TRUE, remove_selected_columns = TRUE)}
  
  #standardizing values 
  scaled.data <- scale(combined)
  
  return(scaled.data)}


#selecting predictors as obtained from the results of Random Forest 
#processing the data for K-Means Clustering Model 
combined_selected <- combined[,c("Deaths","Obesity","Oilcrops","Alcoholic.Beverages",
                                 "Eggs","fat_Vegetal.Products","Animal.Products",
                                 "Vegetal.Products","Animal.fats","protein_Vegetable.Oils")]

combined_selected_fin <- full_data[,c("Country","Deaths","Obesity","Oilcrops","Alcoholic.Beverages",
                                      "Eggs","fat_Vegetal.Products","Animal.Products",
                                      "Vegetal.Products","Animal.fats","protein_Vegetable.Oils")]
combined_kmeans1 <- data.frame(kmeans_processing(combined_selected))


#finding optimal clusters using elbow method 
set.seed(123)
# function to compute total within-cluster sum of square 
within_val <- function(k) {
  kmeans(combined_kmeans1, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k_vals <- seq(1, 9)

# extract wss for 2-15 clusters
wss_values <- c()

for (i in k_vals){
  new_val <- within_val(i)
  wss_values <- c(wss_values, new_val)
}

wss_df <- data.frame(k_vals, wss_values)

op <- subset(wss_df, k_vals == 2)

#ggplot for elbow method
library(bbplot)
ggplot(data = wss_df, aes(x = k_vals, y = wss_values))+ 
  geom_line()+
  geom_point()+
  xlab("Number of clusters K") +
  ylab("Total within-clusters sum of squares") +
  scale_x_continuous(limits=c(0,12),
                     breaks = seq(0, 8, by = 4),
                     labels = c("0","4", "8 clusters"))+
  labs(title = "Finding Optimal Number of Clusters", subtitle ="Elbow Method")+ 
  theme( plot.title=element_text(size=15), plot.subtitle=element_text(size=10), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))+ 
  geom_point(data = op, colour="#1380A1")+ 
  geom_text(data=op, label= paste0("Optimal K =", 2), vjust=1.5, hjust = 1)

#Building a model for 2 clusters 
k <- kmeans(combined_kmeans1, centers = 2, nstart = 25)
paste0('Within cluster variation of data: ',round(k$tot.withinss,2))
combined_kmeans1$cluster=as.factor(k$cluster)
kmeans12_centers <- data.frame(k$centers)
kmeans12_centers

#comparing cluster centers
#write.csv(combined_kmeans1,"/Users/ananyanair/Desktop/kmeans_result.csv", row.names = TRUE)

#visualization for cluster centers and its values
head(kmeans12_centers)

#cluster order: 2,1 
kmeans12_centers = subset(kmeans12_centers, select = -c(cluster))
pivot_centers <- kmeans12_centers %>% 
  pivot_longer(cols = colnames(kmeans12_centers))
head(pivot_centers)

clusters1 <- rep(1,10)
clusters2 <- rep(2,10)
all <- c(clusters2, clusters1)
pivot_centers$labels <-all
head(pivot_centers)


ggplot(aes(x = factor(name), y = value, fill=factor(labels)), data = pivot_centers) +
  stat_summary(fun=mean, geom="bar", position = "dodge2") + 
  theme(axis.text = element_text(size = 8, angle = 90, vjust = 1, hjust=1))+
  scale_fill_discrete(name = "Cluster Label") 

#visualization of the clusters on a two-dimensional plane
library(ggthemes)
library(gridExtra)
#col <- c('Obesity')

colnames(combined_kmeans1) <- c("Deaths", "Obesity", "Oilcrops","Alcoholic.Beverages", "Eggs","fat_Vegetable.Products","Animal.Products","Vegetable.Products","Animal.fats","protein_Vegetable.Oils","cluster")     


plot_cols <- c('Obesity', 'Alcoholic.Beverages', "Vegetable.Products", "Animal.Products")


plot_list <-c()
for (i in plot_cols){
  plot_list[[i]] <- ggplot(combined_kmeans1,aes_string(x=i, y = 'Deaths'))+
    geom_point(alpha = 0.6) + 
    geom_point(aes(colour=cluster))
  
  print(plot_list[[i]])
}

require(gridExtra)
grid.arrange(grobs=plot_list,ncol=2)



#extra: appendix visualization of the clusters for remaining features 
library(ggthemes)
library(gridExtra)
#col <- c('Obesity')

colnames(combined_kmeans1) <- c("Deaths", "Obesity", "Oilcrops","Alcoholic.Beverages", "Eggs","fat_Vegetable.Products","Animal.Products","Vegetable.Products","Animal.fats","protein_Vegetable.Oils","cluster")     
plot_cols <- c('Obesity', 'Alcoholic.Beverages', "Eggs", "Animal.fats")
plot_cols2 <-c('Oilcrops','Eggs', "fat_Vegetable.Products", "Animal.fats", "protein_Vegetable.Oils" )

plot_list <-c()
for (i in plot_cols2){
  plot_list[[i]] <- ggplot(combined_kmeans1,aes_string(x=i, y = 'Deaths'))+
    geom_point(alpha = 0.6) + 
    geom_point(aes(colour=cluster))
  
  print(plot_list[[i]])
}
grid.arrange(grobs=plot_list,ncol=2)

#Analysing Cluster Observations with respect to their income group 
#importing updated combined_kmeans1, for each country its corresponding income group was found (using VLOOKUP and country: income group dataset)
cluster_income <- read.csv('Clustering_Analysis/kmeans_result_income.csv')

library(dplyr)
cluster_income %>%
  count(Income_Group,cluster)

dataset_income <- read.csv('Clustering_Analysis/dataset_income.csv')
head(dataset_income)

dataset_income %>%
  select(Income_Group, Alcoholic.Beverages, Obesity, Sugar...Sweeteners,, Vegetables) %>%
  group_by(Income_Group) %>%
  summarise(mean_alcohol = round(mean(Alcoholic.Beverages),2), mean_obesity = round(mean(Obesity),2), mean_Sugar = round(mean(Sugar...Sweeteners),2), mean_Vegetables = round(mean(Vegetables),2))



#additional visualizations
#re-loading data 
# % of food intake (kg) in countries around the world
food_supply_o <- read.csv("Food_Supply_Quantity_kg_Data.csv")
# % of energy intake (kcal) from different types of food in countries around the world. 
kcal_intake_o <- read.csv("Food_Supply_kcal_Data.csv")
# % of fat intake from different types of food in countries around the world
fat_intake_o <- read.csv("Fat_Supply_Quantity_Data.csv")
# % of protein intake from different types of food in countries around the world
protein_intake_o <- read.csv("Protein_Supply_Quantity_Data.csv")

# Select only required columns
#country <- food_supply_o[, 1]
food_supply <- food_supply_o[, c(1:31)]
kcal_intake <- kcal_intake_o[, c(2:24)]
fat_intake <- fat_intake_o[, c(2:24)]
protein_intake <- protein_intake_o[, c(2:24)]

#defining the function to output new columns names 
rename_columns <- function(prefix, dataframe){
  new_names <- c()
  for (i in colnames(dataframe)){
    i = paste(prefix, i, sep = "")
    new_names <- c(new_names, i)
  }
  return(new_names)
}

#renaming column names of the dataframes
colnames(kcal_intake) <- rename_columns("kcal_",kcal_intake)
colnames(fat_intake) <- rename_columns("fat_",fat_intake)
colnames(protein_intake) <- rename_columns("protein_",protein_intake)

# cbind function to combine dfs
combined <- cbind(food_supply, kcal_intake, fat_intake, protein_intake)
names(combined)
ncol(combined)

#creating index
#rownames(combined) <- food_supply_o[, 1]
head(combined)

#transforming undernourished column to a categorical variable
combined$Undernourished=ifelse(combined$Undernourished=="<2.5",1.5, combined$Undernourished)
#convert char to numeric
combined$Undernourished = as.numeric(combined$Undernourished)
# Treat Undernourished - Replace all '<2.5' with 1.5. And divide into 3 bins by value
combined$Undernourished <- cut(combined$Undernourished, breaks = 3, labels = c("Low", "Mid", "High"))

#processing null values
#dropping null values from target variable 
library(tidyr)
combined <- combined %>% drop_na('Deaths', 'Active', 'Confirmed', 'Recovered')

#creating a dataframe for count of nulls in each column in the combined dataframe 
nulls <- colSums(is.na(combined))
na_count <- data.frame( nulls)

library(dplyr) 
na_count %>% filter(nulls > 0)

#adding Deaths as a categorical variable 
library(Hmisc)
combined$Deaths_cat <- cut2(combined$Deaths, g=3, labels = c("Low Risk", "Medium Risk", "High Risk"))
combined$Deaths_cat <- as.character(combined$Deaths_cat)

table(combined$Deaths_cat)

combined$Deaths_cat[combined$Deaths_cat == "[0.00000,0.00438)"] <- "Low Risk"
combined$Deaths_cat[combined$Deaths_cat == "[0.00438,0.05095)"] <- "Medium Risk"
combined$Deaths_cat[combined$Deaths_cat == "[0.05095,0.18543]"] <- "High Risk"

combined$Deaths_cat <- as.factor(combined$Deaths_cat)

#data with labels
data <- combined
head(data)


#Visualization for Death Rate 
library(ggplot2)
library(bbplot)
ggplot(data = combined, aes(x=Deaths)) + 
  geom_histogram( colour = "white", fill = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(aes(xintercept=mean(Deaths)), color="red", linetype="dashed", size=0.5) +
  bbc_style() +
  labs(title = "How COVID Mortality Varies Around the world", subtitle ="Distribution of COVID-19 Death Rates, 02/06/2021") +
  xlab("Death Rate as % of Population") +
  theme(plot.title=element_text(size=15), plot.subtitle=element_text(size=10), axis.title.x = element_text(size=14)) +
  geom_text(aes(x=mean(Deaths), label=paste0("Mean Death Rate \n",round(mean(Deaths),2)), y=49))

#Visualisation for Recovered Rate
library(ggplot2)
library(bbplot)
ggplot(data = combined, aes(x=Recovered)) + 
  geom_histogram( colour = "white", fill = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(aes(xintercept=mean(Recovered)), color="red", linetype="dashed", size=0.5) +
  bbc_style() +
  labs(title = "How COVID-19 Recovery Varies Around the world", subtitle ="Distribution of COVID-19 Recovery Rates, 02/06/2021") +
  xlab("Recovery Rate as % of Population") +
  theme(plot.title=element_text(size=15), plot.subtitle=element_text(size=10), axis.title.x = element_text(size=14)) +
  geom_text(aes(x=mean(Recovered), label=paste0("Mean Recovery Rate \n",round(mean(Recovered),2)), y=40))

#Visualisation for Confirmed Rate
library(ggplot2)
library(bbplot)
ggplot(data = combined, aes(x=Confirmed)) + 
  geom_histogram( colour = "white", fill = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(aes(xintercept=mean(Confirmed)), color="red", linetype="dashed", size=0.5) +
  bbc_style() +
  labs(title = "How COVID-19 Confirmed Cases Vary Around the world", subtitle ="Distribution of COVID-19 Confirmed Cases Rates, 02/06/2021") +
  xlab("Confirmed Rate as % of Population") +
  theme(plot.title=element_text(size=15), plot.subtitle=element_text(size=10), axis.title.x = element_text(size=14)) +
  geom_text(aes(x=mean(Confirmed), label=paste0("Mean Confirmed Rate \n",round(mean(Confirmed),2)), y=49))


#Visualisation for Active Cases Rate
library(ggplot2)
library(bbplot)
ggplot(data = combined, aes(x=Active)) + 
  geom_histogram( colour = "white", fill = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(aes(xintercept=mean(Active)), color="red", linetype="dashed", size=0.5) +
  bbc_style() +
  labs(title = "How COVID Active Cases Vary Around the world", subtitle ="Distribution of COVID-19 Active Cases Rates, 02/06/2021") +
  xlab("Active Cases Rate as % of Population") +
  theme(plot.title=element_text(size=15), plot.subtitle=element_text(size=10), axis.title.x = element_text(size=14)) +
  geom_text(aes(x=mean(Active)+0.5, label=paste0("Mean Active Cases Rate \n",round(mean(Active),2)), y=49))