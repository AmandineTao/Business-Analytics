#############################################################
#        Which ski resort is the most suitable to invest in?      #
#############################################################

# -------------- Step 1 : import and clean data --------------#

# Load the dataset
data  <-  read.csv2("skidata.csv", row.names = 1) # The "row.names = 1" argument uses the first column of the csv file for rownames

# Take a look at the structure and the variable types in our dataset
head(data)
str(data)

# Correct importing errors : change data types from integer (quantitative) to factor (qualitative)
data$dept <- factor(data$dept)
data$snowboard <- factor(data$snowboard, levels = c(0,1), labels = c("no", "yes"))
data$cross <- factor(data$cross, levels = c(0,1), labels = c("no", "yes"))
data$pool <- factor(data$pool, levels = c(0,1), labels = c("no", "yes"))
data$cinema <- factor(data$cinema, levels = c(0,1), labels = c("no", "yes"))
data$discot <- factor(data$discot, levels = c(0,1), labels = c("no", "yes"))
data$gliding <- factor(data$gliding, levels = c(0,1), labels = c("no", "yes"))
data$casino <- factor(data$casino, levels = c(0,1), labels = c("no", "yes"))
data$museum <- factor(data$museum, levels = c(0,1), labels = c("no", "yes"))
data$horse <- factor(data$horse, levels = c(0,1), labels = c("no", "yes"))
data$snowshoe <- factor(data$snowshoe, levels = c(0,1), labels = c("no", "yes"))
data$iceskate <- factor(data$iceskate, levels = c(0,1), labels = c("no", "yes"))
data$fitness <- factor(data$fitness, levels = c(0,1), labels = c("no", "yes"))
data$smobile <- factor(data$smobile, levels = c(0,1), labels = c("no", "yes"))

# Convert the number of runs per color (black, red, blue, green) into a percentage of total runs
data$pcBlackruns <- data$nbBlackruns/data$runs
data$pcRedruns <- data$nbRedruns/data$runs
data$pcBlueruns <- data$nbBlueruns/data$runs
data$pcGreenruns <- data$nbGreenruns/data$runs

# Exclude unnecessary variables from the data set (number of runs by color)
data.pca <- data[-c(10:13)]
View(data.pca)

# -------------- Step 2 : process the data using PCA --------------#

# Run a principal components analysis on our reduced data set
#install.packages('FactoMineR')
library(FactoMineR) # Load FactoMineR
res <- PCA(data.pca, quali.sup = c(1, 11:24))

# Select an appropriate number of dimensions
res$eig
barplot(res$eig[,1], main="Eigenvalues", names.arg=1:nrow(res$eig)) # Create a bar chart of eigenvalues

# Interpret what each dimension means using variable correlations
res$var
plot(res, choix="var", select="contrib 5") # Modify the correlation circle to only show the 5 variables that contribute the most to the dimensions

# -------------- Step 3 : use results to answer business question --------------#

# Create a competitive map of ski resorts
plot(res, cex=0.8, invisible="quali", title="Competitive map of ski resorts")

plot(res, cex=0.8, invisible="quali", label="none", title="Competitive map of ski resorts") # Hide labels (ski resort names)
plot(res, cex=0.8, invisible="quali", habillage="dept", title="Competitive map of ski resorts")
plot(res, cex=0.8, choix="ind", invisible="quali", habillage="dept", select="cos2 0.7")
plot(res, cex=0.8, choix="ind", invisible="quali", habillage="snowboard")
