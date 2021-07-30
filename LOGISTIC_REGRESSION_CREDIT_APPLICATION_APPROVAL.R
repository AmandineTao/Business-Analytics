#########################################################
#        PREDICTIVE MODEL OF CREDIT APPLICATION APPROVAL #
########################################################

# Load the dataset
credit.data <- read.csv2('CreditScoring.csv', na.strings = "")
str(credit.data)

summary(credit.data)

# Re-order the factors to place the reference category first
credit.data$Home <- relevel(credit.data$Home, "rent")
credit.data$Marital <- relevel(credit.data$Marital, "single")
credit.data$Job<- relevel(credit.data$Job, "fulltime")

# Create two subsets : one to build a predictive model, one for testing
credit.train <- credit.data[1:2980, ]
credit.test <- credit.data[-c(1:2980), ]

# Run the logistic regression model using the glm( ) function
credit.model <- glm(Decision ~., family=binomial(link='logit'), data=credit.train) # The ~ means "is a function of" and the . means "all other variables"
summary(credit.model)

# Remove non-significant variables one by one and re-run the regression
credit.model <- glm(Decision ~ Seniority + Home + Marital + Age + Job 
                    + Expenses + Income + Assets + Debt + Amount + Price, 
                    family=binomial(link='logit'), data=credit.train)
summary(credit.model)

# Predict approval using the model
credit.predict <- predict(credit.model, newdata=credit.test, type='response') # The argument type='response' argument returns predicted probabilities
head(credit.predict)
credit.predict <- ifelse(credit.predict > 0.5, 1, 0)
head(credit.predict)

# Calculate the prediction accuracy
misclassificationError <- mean(credit.predict != credit.test$Decision)
misclassificationError <- round(misclassificationError, digits=2)
print(paste('Prediction accuracy =',1-misclassificationError))

# Create tables to better understand the predictive quality of the model
credit.predict <- factor(credit.predict, levels = c(0,1), labels = c("predicted reject", "predicted accept"))
credit.test$Decision <- factor(credit.test$Decision, levels = c(0,1), labels = c("rejected", "accepted"))

table(credit.predict,credit.test$Decision)
prop.table(table(credit.predict,credit.test$Decision),2)
mosaicplot(table(credit.predict,credit.test$Decision), color = TRUE)

# Predict a decision for an application
predictors <- data.frame(Seniority=5, Home="owner", Marital="single", Age=50, Job="freelance", 
                         Expenses=50, Income=300, Assets=0, Debt=0, Amount=500, Price=400)
predict(credit.model, predictors, type="response")
