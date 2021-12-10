
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

# cbind function to combine dfs
combined <- cbind(food_supply, kcal_intake, fat_intake, protein_intake)
names(combined)
ncol(combined)

# dataf[, c(1, 2, 3)]
# dataf[, c('A', 'B', 'Cost')]

