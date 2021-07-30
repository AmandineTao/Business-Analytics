##################################################
#        lactalis contamination scandal               #
#Has the opinion of the press and the public improved?  #
##################################################

# -------------- Step 1 : import text corpus --------------#

# Load the dataset
data <- read.csv2("lactalis.csv")

# Take a look at the structure and the variable types in our dataset
str(data)

# Correct importing errors : change data types
data$Date <- as.Date(data$Date, "%d/%m/%Y")
data$Title <- as.character(data$Title)
data$Text <- as.character(data$Text)

# -------------- Step 2 : clean the text corpus --------------#

# Clean text using Base R functionalities
data$TextClean <- gsub(pattern="\\W", replace=" ", data$Text) # find punctuation and spaces (using the short-hand code \\W) and replace with a space
data$TextClean <- gsub(pattern="\\d", replace=" ", data$TextClean) # find numbers (d = digits) and replace with a blank
data$TextClean <- tolower(data$TextClean)

# Clean text using text mining package
# Install and load the tm package
install.packages("tm")
library(tm)

# Remove uninteresting words (that, and, or, etc) called stopwords
# Type the script sort(stopwords()) to see what the stopwords are
data$TextClean <- removeWords(data$TextClean, stopwords("english")) # remove non useful word (specify the language)
data$TextClean <- gsub(pattern="\\b[A-z]\\b{1}", replace="", data$TextClean) # Get rid of orphan letters. \\b[A-z] means 'a word beginnig with any letter from A-z'
data$TextClean <- stripWhitespace(data$TextClean) # Clean up extra whitespace caused by the cleaning process

# -------------- Step 3 : break the corpus down into keywords --------------#

# Install and load package to help deal with strings
install.packages("stringr")
library(stringr)
data$TextClean <- str_split(data$TextClean, pattern = "\\s+") # Split a string into its individual words. The regex indicates 'one or more spaces'

# -------------- Step 4 : generate word clouds --------------#

# Install and load package to draw word clouds
install.packages("wordcloud")
library(wordcloud)

textbag <- unlist(data$TextClean)
wordcloud(textbag)
wordcloud(textbag, random.order = FALSE) # Show most frequent words in middle and less frequent towards outside
wordcloud(textbag, min.freq=4, random.order=FALSE, scale=c(5,1), color=rainbow(7))
textbag <- removeWords(textbag, "said")
wordcloud(textbag, min.freq=4, random.order=FALSE, scale=c(5,1), color=rainbow(7))

# Draw wordcloud for September
data$MonthYear <- format(as.Date(data$Date), "%Y-%m") #create new variable to group together articles by month
data$MonthYear <- as.factor(data$MonthYear)
textbag <- data[data$MonthYear=="2018-09", 5]
textbag <- unlist(textbag)
textbag <- removeWords(textbag, "said")
wordcloud(textbag, min.freq=4, random.order=FALSE, scale=c(3,1), color=rainbow(7))

# -------------- Step 5 : compare the keywords with a sentiment dictionary --------------#

# Load lexicons of positive and negtive words
poswords <- scan("pos.txt", what="character")
negwords <- scan("neg.txt", what="character")

# Match words from our text with the positive words
data$TextPosMatches <- lapply(data$TextClean, function(x) match(x, poswords))
data$TextPosMatchesCount <- lapply(data$TextPosMatches, function(x) sum(!is.na(x)))

# Match words from our text with the negative words
data$TextNegMatches <- lapply(data$TextClean, function(x) match(x, negwords))
data$TextNegMatchesCount <- lapply(data$TextNegMatches, function(x) sum(!is.na(x)))

# -------------- Step 6 : plot sentiment over time --------------#

# Calculate a sentiment score 
data$SentimentScore <- unlist(data$TextPosMatchesCount)-unlist(data$TextNegMatchesCount)

# Plot the sentiment score over time
plot(SentimentScore~Date, data, type = "l")

# Create box plots of the sentiment score over time
boxplot(SentimentScore~Date, data)

plot(SentimentScore~MonthYear, data)