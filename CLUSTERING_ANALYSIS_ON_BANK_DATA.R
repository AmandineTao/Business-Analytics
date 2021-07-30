########################################################################################################
#   Segment customers according to their behavioral profil? who is the target for the new product?         #
########################################################################################################

# -------------- Step 1 : import and clean data --------------#

# Load the dataset
data  <-  read.csv2('bankdata.csv')

# Take a look at the structure and the variable types in our dataset
str(data)

# -------------- Step 2 : prepare the data --------------#

# We will only use quantitative variables measuring banking behaviour for our clustering. Extract these into a new dataframe
grpdata <- data[c(2:9)] # extract variables 2 to 9

# We need to first standardize our variables
grpdata.scaled  <-  scale(grpdata) # the scale function automatically performs data standardization on all your variables

# -------------- Step 3 : process the data using HCA --------------#

d  <-  dist(grpdata.scaled, method = "euclidean") # the dist() function computes the distances of all the observations in our dataset
hcward  <-  hclust(d, method="ward.D") # hclust() function performs hiearchical clustering, we pass it the distances, and we set the method argument to "ward.D"

plot(hcward) #visualise the dendogram of the cluster solution

# -------------- Step 4 : determine the number of clusters --------------#

# Visualize the number of individuals in each cluster
rect.hclust(tree = hcward, k = 3, border = "blue")
rect.hclust(tree = hcward, k = 4, border = "green")

# Retain a four cluster solution
data$groups4 <- cutree(hcward,k=4) # assign individuals to our 4 clusters
table(data$groups4) # count the number of individuals in each cluster
prop.table(table(data$groups4))

# -------------- Step 5 : describe the clusters --------------#

# Use ANOVA and chi-squared tests to check for relationships between groups and variables
anova_results <- aov(data$Balance ~ data$groups4) # Run an ANOVA on the Balance variable by group
summary(anova_results) #Display results of ANOVA

summary(aov(data$Nb.accounts ~ data$groups4)) # Run and show results of ANOVA
summary(aov(data$Total.withdrawls ~ data$groups4))
summary(aov(data$Total.loans ~ data$groups4))
summary(aov(data$Total.deposits ~ data$groups4))
summary(aov(data$Total.overdraft ~ data$groups4))
summary(aov(data$Nb.overdraft ~ data$groups4))
summary(aov(data$Var.savings ~ data$groups4))
summary(aov(data$Age ~ data$groups4))
summary(aov(data$Children ~ data$groups4))

chisq.test(table(data$PEL, data$groups4))
chisq.test(table(data$CEL, data$groups4))
chisq.test(table(data$ASV, data$groups4))
chisq.test(table(data$CMT, data$groups4))
chisq.test(table(data$PEA, data$groups4))
chisq.test(table(data$Profession, data$groups4))

# Generate cluster means for each quantitative variable
apply(grpdata, 2, mean)
tapply(data$Balance, data$groups4, mean)
tapply(data$Total.deposits, data$groups4, mean)
tapply(data$Total.overdraft, data$groups4, mean)
tapply(data$Nb.overdraft, data$groups4, mean)
tapply(data$Var.savings, data$groups4, mean)

# Run a Tukey test to see which clusters differ pairwise across quantitative variables
data$groups4 <- factor(data$groups4) # The data$groups4 variable needs to be of type factor to work with this test

TukeyHSD(aov(data$Balance ~ data$groups4))
TukeyHSD(aov(data$Total.deposits ~ data$groups4))
TukeyHSD(aov(data$Total.overdraf ~ data$groups4))
TukeyHSD(aov(data$Nb.overdraft ~ data$groups4))
TukeyHSD(aov(data$Var.savings ~ data$groups4))

# Generate cluster counts for each quanlitative variable
prop.table(table(data$PEL, data$groups4), 2)
prop.table(table(data$CEL, data$groups4), 2)
prop.table(table(data$ASV, data$groups4), 2)
prop.table(table(data$CMT, data$groups4), 2)
prop.table(table(data$PEA, data$groups4), 2)
prop.table(table(data$Profession, data$groups4), 2)

# You can now write descriptions of the four clusters
