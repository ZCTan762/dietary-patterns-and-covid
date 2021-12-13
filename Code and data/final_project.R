
# Read in data files 

# % of food intake (kg) in countries around the world
food_supply_o <- read.csv("Food_Supply_Quantity_kg_Data.csv")

# % of energy intake (kcal) from different types of food in countries around the world. 
kcal_intake_o <- read.csv("Food_Supply_kcal_Data.csv")

# % of fat intake from different types of food in countries around the world
fat_intake_o <- read.csv("Fat_Supply_Quantity_Data.csv")

# % of protein intake from different types of food in countries around the world
protein_intake_o <- read.csv("Protein_Supply_Quantity_Data.csv")


# Select only some columns
food_supply <- food_supply_o[, c(2:31)]
kcal_intake <- kcal_intake_o[, c(2:24)]
fat_intake <- fat_intake_o[, c(2:24)]
protein_intake <- protein_intake_o[, c(2:24)]


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
combined <- cbind(food_supply, kcal_intake, fat_intake, protein_intake)
attach(combined)

##### Preprocessing #####

# Keep only Deaths as a target predictor
drop_list <- c('Active', 'Recovered', 'Confirmed')
combined <- combined[ , !names(combined) %in% drop_list]

# check for nulls 
# nulls <- colSums(is.na(combined))
# na_count <- data.frame( nulls)
# na_count

# Drop rows with NAs in target var
library(tidyr)

combined <- combined %>% drop_na('Deaths')

# Exclude Undernourished column for numerical analyses
combined_numeric <- combined[,-25]

# Treat Undernourished - Replace all '<2.5' with 1.5 and divide into 3 bins by value
combined$Undernourished=ifelse(combined$Undernourished=="<2.5",1.5, combined$Undernourished)
combined$Undernourished <- cut(combined$Undernourished, breaks = 3, labels = c("Low", "Mid", "High"))
# Move NA  
combined$Undernourished <- factor(ifelse(is.na(combined$Undernourished), 
                                         "No Data", paste(combined$Undernourished)), 
                                  levels = c(levels(combined$Undernourished), "No Data"))

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
# cor_matrix <- cor(combined_numeric, method = c("pearson"))
# write.csv(round(cor_matrix, 3), file = "cormatrix2.csv")

# Plot correlation heatmap
library(ggplot2)
library(reshape2)
melted_cormat <- melt(cor_matrix)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed()


##### Regression Random Forest #####

library(randomForest)
myforest=randomForest(Deaths~., data=combined, ntree=1000,importance=TRUE, na.action = na.omit)

### Identify variable importance
x <- list()
y <- list()
for (j in 1:10)
{
  for (i in 1:20)
  {
    impforest_r=randomForest(combined$Deaths~., data=combined, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    
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

### Drop insignificant predictors
rf_drop_list <- c('Undernourished', 'Stimulants', 'fat_Meat', 'Aquatic.Products..Other',
                      'fat_Sugar...Sweeteners','fat_Stimulants', 'Offals', 'Population',
                      'Fruits...Excluding.Wine','Aquatic.Products..Other', 'Vegetables',
                      'Starchy.Roots')

combined_filtered <- combined[ , !names(combined) %in% rf_drop_list]

### APPROACH 2: Choose significant predictors
combined_selected <- combined[,c("Deaths","Obesity","Oilcrops","Alcoholic.Beverages",
                                 "Eggs","fat_Vegetal.Products","Animal.Products",
                                 "Vegetal.Products","Animal.fats","protein_Vegetable.Oils")]


### Find importance again
x <- list()
y <- list()
for (j in 1:10)
{
  for (i in 1:20)
  {
    impforest_r=randomForest(combined_selected$Deaths~., data=combined_selected, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    
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

require(caTools)
require(caret)
accuracy_list = rep(NA, 25)
for (i in 1:25)
{
  # Train test split
  sample = sample.split(combined_selected$Deaths, SplitRatio = .7)
  train = subset(combined_selected, sample == TRUE)
  test  = subset(combined_selected, sample == FALSE)
  
  # Build model with training data
  testforest=randomForest(train$Deaths~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)
  
  # Run predictions
  predicted_score = predict(testforest, newdata=test)
  accuracy_list[i] <- mean((predicted_score - test$Deaths)^2)
  
}
mean(accuracy_list)



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

# Drop deaths column
drop_list <- c('Deaths')
combinedcat <- combinedcat[ , !names(combinedcat) %in% drop_list]

### Identify variable importance
x <- list()
y <- list()
for (j in 1:10)
{
  for (i in 1:20)
  {
    impforest=randomForest(combinedcat$Deaths_cat~., data=combinedcat, ntree=(500+(j*50)),importance=TRUE, na.action = na.omit)
    
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

### Drop insignificant predictors
rf_drop_list_cat <- c('Undernourished', 'Stimulants', 'fat_Meat', 'Aquatic.Products..Other',
                  'fat_Sugar...Sweeteners','fat_Stimulants', 'Offals', 'Population',
                  'Fruits...Excluding.Wine','Aquatic.Products..Other', 'Vegetables',
                  'Starchy.Roots')

combinedcat_filtered <- combinedcat[ , !names(combinedcat) %in% rf_drop_list_cat]

### APPROACH 2: Choose significant predictors
combinedcat_selected <- combinedcat[,c("Deaths_cat","Obesity", "Animal.Products", "Animal.fats",
                                       "Vegetal.Products","Eggs","Oilcrops","Fish..Seafood",
                                       "fat_Milk...Excluding.Butter","kcal_Sugar...Sweeteners",
                                       "Alcoholic.Beverages","Treenuts","fat_Vegetal.Products",
                                       "fat_Cereals...Excluding.Beer")]

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

require(caTools)
require(caret)
accuracy_list = rep(NA, 25)
for (i in 1:25)
{
  # Train test split
  sample = sample.split(combinedcat_selected$Deaths_cat, SplitRatio = .7)
  train = subset(combinedcat_selected, sample == TRUE)
  test  = subset(combinedcat_selected, sample == FALSE)
  
  # Build model with training data
  testforestcat=randomForest(train$Deaths_cat~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)
  
  # Run predictions
  predicted_score = predict(testforestcat, newdata=test)
  
  #confusionMatrix(predicted_score, test$Deaths_cat)    # For classification forest
  #cm$overall['Accuracy']
  accuracy_list[i] <- confusionMatrix(predicted_score, test$Deaths_cat)$overall['Accuracy']
}
mean(accuracy_list)


##### Gradient Boosting #####

library(gbm)

# distribution="gaussian"  is used for regression problems
# interaction.depth=4  is the number of nodes in each tree
boosted=gbm(train$Deaths_cat~., data = train,
            distribution="bernoulli",n.trees=10000, interaction.depth=4)
summary(boosted)

predicted_score=predict(boosted, newdata=test, n.trees=10000)
confusionMatrix(predicted_score, test$Deaths_cat)$overall['Accuracy']

# LDA setup
# prior_prob<-data.frame(table(combined$Deaths_cat)/nrow(combined))
# colnames(prior_prob) <- c("category", "probability")



# Perform PCA
pca=prcomp(combined_numeric, scale=TRUE)
summary(pca)

# Scree plot for PCA - Good for report
plot(pca, type = "l", main = "Scree plot for PCA")

### The first 2 components explain only 32% of variance. Use correlation matrix 
### to shave down first instead.

# Find variable importance through PCA (only numeric)

# PC1
loading_Scores_PC1 <- pca$rotation[,1]       # describe how much each attribute contributes to the principal component
influence_PC1 <- abs(loading_Scores_PC1)     # rank in order of influence, ignore negative/positive influence
influence_PC1ranked <- names(sort(influence_PC1,decreasing = T))    # Output components of PC in order of in

# PC2
loading_Scores_PC2 <- pca$rotation[,2]       # describe how much each attribute contributes to the principal component
influence_PC2 <- abs(loading_Scores_PC2)     # rank in order of influence, ignore negative/positive influence
influence_PC2ranked <- names(sort(influence_PC2,decreasing = T))    # Output components of PC in order of in

# % variance explained
pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))        # Cumulative sum
par(mfrow=c(1,1))

autoplot(pca, data = pca_data, loadings = TRUE, col="grey", loadings.label = TRUE )