
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


# Defining the function to output new columns names 
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
attach(combined)
names(combined)
ncol(combined)

# Keep only Deaths as a target predictor
drop_list <- c('Active', 'Recovered', 'Confirmed')
combined <- combined[ , !names(combined) %in% drop_list]

# check for nulls 
nulls <- colSums(is.na(combined))
na_count <- data.frame( nulls)
na_count

# Drop rows with NAs in target var
library(tidyr)
# TODO: Drop 1 NA observation in Obesity TEMP
combined <- combined %>% drop_na('Deaths') %>% drop_na('Obesity')


# Treat Undernourished - Replace all '<2.5' with 1.5 and divide into 3 bins by value
combined$Undernourished=ifelse(combined$Undernourished=="<2.5",1.5, combined$Undernourished)
combined$Undernourished <- cut(combined$Undernourished, breaks = 3, labels = c("Low", "Mid", "High"))
# Move NA  
combined$Undernourished <- factor(ifelse(is.na(combined$Undernourished), 
                                         "No Data", paste(combined$Undernourished)), 
                                  levels = c(levels(combined$Undernourished), "No Data"))

### Exclude Undernourished column for numerical analyses
combined_numeric <- combined[,-25]

# Create correlation matrix
cor_matrix <- cor(combined_numeric, method = c("pearson"))
write.csv(round(cor_matrix, 3), file = "cormatrix.csv")

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
write.csv(round(cor_matrix, 3), file = "cormatrix2.csv")

##################### WORKS TILL HERE #####################

### Random Forest - find importance
library(randomForest)
myforest=randomForest(Deaths~., data=combined, ntree=1000,importance=TRUE, na.action = na.omit)

pred_importance <- importance(myforest)
pred_importance <- pred_importance[order(pred_importance[,1],decreasing=TRUE),]

# test <- pred_importance[,1]
x <- list()

for (i in 1:10)
{
  myforest=randomForest(Deaths~., data=combined, ntree=1000,importance=TRUE, na.action = na.omit)
  
  pred_importance <- importance(myforest)
  pred_importance <- pred_importance[order(pred_importance[,1],decreasing=TRUE),]
  x[[i]] <- pred_importance[,1]
}

# Find average
rowMeans(cbind(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], x[[8]], x[[9]], x[[10]]))

## Drop insignificant predictors
rf_drop_list <- c('fat_Fruits...Excluding.Wine', 'Starchy.Roots', 
                  'Aquatic.Products..Other','fat_Sugar...Sweeteners','Offals',
                  'Fruits...Excluding.Wine')
                   
combined <- combined[ , !names(combined) %in% rf_drop_list]
combined_numeric <- combined_numeric[ , !names(combined_numeric) %in% rf_drop_list]

## Find importance again
x <- list()

for (i in 1:10)
{
  myforest=randomForest(Deaths~., data=combined, ntree=1000,importance=TRUE, na.action = na.omit)
  
  pred_importance <- importance(myforest)
  pred_importance <- pred_importance[order(pred_importance[,1],decreasing=TRUE),]
  x[[i]] <- pred_importance[,1]
}

# Find average
rowMeans(cbind(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], x[[8]], x[[9]], x[[10]]))

# Train test
require(caTools)
sample = sample.split(combined$Deaths, SplitRatio = .5)
train = subset(combined, sample == TRUE)
test  = subset(combined, sample == FALSE)

# Build model with training data
testforest=randomForest(Deaths~., data=train, ntree=1000,importance=TRUE, na.action = na.omit)

# Run predictions
predicted_score = predict(testforest, newdata=test)
mean((predicted_score - test$Deaths)^2)

# library(caret)
# confusionMatrix(predicted_score, test$Deaths)

# Separate out the target
target = combined[,c(21)]
#predictors = combined[,-21]

# Run predictions
predicted_score = predict(myforest, newdata=combined)
mean((predicted_score - target)^2)









# Perform PCA
predictors_numeric = predictors[,-25]
pca=prcomp(predictors_numeric, scale=TRUE)
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