##################################################
#        DATA EXPLORATION ON TITANIC DATA      #
##################################################

# Create a new project called "Titanic" and copy the dataset titanic.csv into the main folder

# Load the dataset
TitanicData  <-  read.csv2('titanic.csv', na.strings="", stringsAsFactors = FALSE) # The na.strings argument replaces blanks with NA
# By default, all text strings are imported as factors. You can avoid this with the "stringsAsFactors" argument.

# Take a look at our data in familiar tabular form
View(TitanicData) # Note the use of camel case

# verify the structure and the variable types in our dataset
str(TitanicData)

# Transform variables Class, Survived, Station into categorical variables (type = factor)
TitanicData$Class <- factor(TitanicData$Class, levels = c(1,2,3,4), labels = c("1st","2nd", "3rd", "Crew"))
summary(TitanicData$Class) # Check the transformation of the Class variable
TitanicData$Survived <- factor(TitanicData$Survived, levels = c(0,1), labels = c("No","Yes"))
summary(TitanicData$Survived)

# Were women and children rescued first?
# Create a table of passenger survival
table(TitanicData$Survived)
prop.table(table(TitanicData$Survived)) # convert to relative frequencies
mosaicplot(table(TitanicData$Survived), color = TRUE)

# Create a two way comparison of the number of male and female survivors
table(TitanicData$Sex, TitanicData$Survived)
prop.table(table(TitanicData$Sex, TitanicData$Survived),1) # The 1 indicates the dimension to be used to calculate proportions (1=rows, 2=columns)
mosaicplot(table(TitanicData$Sex, TitanicData$Survived), color = TRUE)

# Draw a mosaic plot to represent the contingency table graphically.
mosaicplot(table(TitanicData$Sex, TitanicData$Survived), color = TRUE,
           main = "Survival on the Titanic",
           xlab = "Gender",
           ylab = "Did the passenger survive?")

# Women and children first : Is there a relationship between survival, age and gender?
TitanicData$Child <- 0 # Create a new variable called "Child" and assign it a default value of 0
TitanicData$Child[TitanicData$Age < 18] <- 1 # Square brackets create a subset of the dataset and assign values of 1 to rows that meet the criteria
TitanicData$Child <- factor(TitanicData$Child, levels = c(0,1), labels = c("Adult","Child"))

# Create a two way comparison of the number of child and adult survivors
table(TitanicData$Child, TitanicData$Survived)
prop.table(table(TitanicData$Child, TitanicData$Survived),1)

# Draw a mosaic plot to represent the 2x2 contingency table graphically.
mosaicplot(table(TitanicData$Child, TitanicData$Survived), color = TRUE,
           main = "Survival on the Titanic",
           xlab = "Age",
           ylab = "Did the passenger survive?")

# Draw a mosaic plot to see the influence of sex, age, and class on survival.
mosaicplot(table(TitanicData$Sex, TitanicData$Child, TitanicData$Class, TitanicData$Survived),
           main = "Survival on the Titanic",
           xlab = "Gender",
           ylab = "Age",
           col = hcl(c(240, 120)),
           off = c(5, 5, 5, 5))