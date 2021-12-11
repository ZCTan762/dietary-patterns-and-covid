
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

# Drop rows with NA entries in target - Ananya


# Select only numeric predictors
# Removes 'undernourished' column - contains greater than and less than signs
# combined_numeric <- combined %>%
#   select_if(is.numeric)

# Treat Undernourished - Replace all '<2.5' with 1.5. And divide into 3 bins by value
combined$Undernourished=ifelse(combined$Undernourished=="<2.5",1.5, combined$Undernourished)
combined$Undernourished <- cut(combined$Undernourished, breaks = 3, labels = c("Low", "Mid", "High"))
# Move NA  
combined$Undernourished <- factor(ifelse(is.na(combined$Undernourished), 
                                         "No Data", paste(combined$Undernourished)), 
                                  levels = c(levels(combined$Undernourished), "No Data"))


# Create correlation matrix
cor_matrix <- cor(combined_numeric, method = c("pearson"))
round(cor_matrix, 3)
write.csv(cor_matrix, file = "cormatrix.csv")



# dataf[, c(1, 2, 3)]
# dataf[, c('A', 'B', 'Cost')]

