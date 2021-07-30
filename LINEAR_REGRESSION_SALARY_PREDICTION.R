##################################################
#        Predict a salary for the new CEO.        #
##################################################

# -------------- Step 1 : import and clean data --------------#

# Load the dataset
ceodata <- read.csv2('ceosalary.csv')

# Take a look at the structure and the variable types in our dataset and summary statistics
str(ceodata) # Qualitative variables should be of type factor
summary(ceodata)

# -------------- Step 2 : verify that conditions are met --------------#

# Produce a histogram of the quantitative variables to verify normality
par(mfrow = c(1, 4)) 
boxplot(ceodata$Age, main = "Age of CEO", ylab = "Years")
boxplot(ceodata$Headcount, main = "Number of employees", ylab = "People")
boxplot(ceodata$CEOsalary, main = "Salary of CEO", ylab = "'000 euros")
boxplot(ceodata$Turnover, main = "Turnover", ylab = "'000 euros")
par(mfrow = c(1, 1))

# Compute the correlation matrix between quantitative variables to check collinearity
cor(ceodata[3:6])
pairs(ceodata[3:6])

# -------------- Step 3 : calculate the regression model --------------#

# Estimate a linear regression model of CEO salary as a function of everything else.
linreg <- lm(CEOsalary~Age+Headcount+Turnover+factor(Gender)+factor(Sector), data=ceodata)
summary(linreg) # Reports the results of the regression

# -------------- Step 4 : validate the model --------------#

# Remove non significant variables and re-run regression
linreg <- lm(CEOsalary~Headcount+Turnover+factor(Gender), data=ceodata)
summary(linreg)

# Check regression assumptions by examining residuals
residuals <- linreg$residuals

# Verify that residuals are normally distributed
qqnorm(residuals)
qqline (residuals)

# Verify the heteroskadasticity and non autocorrelation of residuals
plot(residuals)
plot(residuals, ceodata$Headcount)
plot(residuals, ceodata$Turnover)

# Verify the predictive power of our model
cor(linreg$fitted.values, ceodata$CEOsalary) # Computes the correlation between the fitted values and the actual ones
plot(ceodata$CEOsalary, linreg$fitted.values) # Plot the fitted values vs. the actual ones

# -------------- Step 5 : use the model for prediction --------------#

# Predict a salary for the new CEO
predictors <- data.frame(Headcount=800, Turnover=60000, Gender="Female")
predict(linreg, predictors)
predict(linreg, predictors, interval = "prediction", level = 0.95)