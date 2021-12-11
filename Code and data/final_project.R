
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


##################### WORKS TILL HERE #####################


# Separate out the target
target = combined[,c(26)]
predictors = combined[,-26]

# Perform PCA -  
pca=prcomp(pca_data, scale=TRUE)
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