# Now that we have a clean and tidy dataset, let's start the fun part - analyzing! 
# First let's source our cleaned survey from the `LynCenterSurveyLargeDF` file and
# remove the unnecessary objects.
setwd("C:/MyStuff/DataScience/Projects/LynxCenter")
source("LynxCenterSurveyLargeDF.R")
rm(books, i, j, datepos, path, surveys)
library(data.table)
library(ggplot2)
library(tm)

# Now let's begin our analysis. First, let's count the totoal number of times 
# each response was recorded. To do this, we're going to grab the unique responses
# from the `Topic` column in our `clean.survey` data frame. 
topics <- unique(clean.survey$Topic) # Find the unique response `Topic`s

topic.list <- vector("list", length = length(topics)) # dummy variable to store 

# Loop through the `topics` vector and find the number of times each topic was 
# recorded
for (i in 1:length(topics)) {
  topic.list[[i]] <- grep(topics[i], clean.survey$Topic)
  topic.list[[i]] <- length(topic.list[[i]])
}  

# Associate the appropriate response with the number of times it occurs
names(topic.list) <- topics

topic.list <- unlist(topic.list) # Turn topic.list into a vector
topic.list

# When we print our result, we immediately see a lot of weird entires. It seems
# that some of the entries are related to two of the original columns in our
# survey spreadsheets. To mitigate this, let's go about this another way. Instead
# of searching through the unique values in our `Topic` columns, let's just use the
# original column names and the method we used above. This should allow us to 
# account for the observations in our `clean.survey` data frame that have multiple
# topics. Since we don't have the original survey in our working environment, let's
# load in a survey and get them. 
survey <- read_excel("November_2015.xlsx", sheet = 2, col_names = TRUE, na = "", skip = 3)
topics <- colnames(survey) # Save it to topics since we don't need our old vector

# If we print it out, we see there are some blank as well as NA values, let's remove 
# those. 
topics[is.na(topics)] <- "" # Change NAs to blank
topics <- unique(topics) # Reduce total number of blanks
topics <- topics[-1] # Remove the blank value

# Now we have our desired vector. Let's use the same method as we did above to 
# find the total number of times each topic appeared

topic.count <- vector("list", length = length(topics)) # dummy variable to store 

# Loop through the `topics` vector and find the number of times each topic was 
# recorded
for (i in 1:length(topics)) {
  topic.count[[i]] <- grep(topics[i], clean.survey$Topic)
  topic.count[[i]] <- length(topic.count[[i]])
}  

# If we print this out it inititally looks a lot better. Let's give each element
# it's proprer name so we know what topic is associated to each count
topic.count <- unlist(topic.count)

# Let's create a data table with the results so we can visualize them. 
topic.df <- data.table(Topic = topics, Value = topic.count)
topic.df <- topic.df[order(-Value)] # Reorder the topics largest to smallest

# Now let's create an initial visualization of the top 15 results. For this, 
# we'll use a simple bar chart. 
ggplot(topic.df[1:15, ], aes(x = reorder(Topic, -Value), y = Value)) + 
  geom_bar(stat = "identity", fill = "#e2cc7c", col = "Black") +
  labs(title = "Number of Responses Topics", x = "Topics")


# From this initial visualization, we can see that the "General Info" topic 
# is double the next value. In fact, "General Info" makes up around 20% of our 
# overall responses. Because of this, we'll do analysis on this category alone. 
# Let's redo our above visualization without "Genearl Info" observation. 
topic.df <- topic.df[-1, ]

ggplot(topic.df[1:15], aes(x = reorder(Topic, -Value), y = Value)) + 
  geom_bar(stat = "identity", fill = "#e2cc7c", col = "Black") +
  labs(title = "Number of Responses per Topic", x = "Topics")


# Now let's look at the "General Info" topic in depth. To do this, let's find 
# all the rows of our `clean.survey` data.frame. Let's make it a data.table as well.
clean.survey <- data.table(clean.survey)
gen.info <- clean.survey[Topic == "General Info"]

# To try to learn more about what is contained in "General Info", let's look at 
# the comments section. 
gen.info[, Comments]

# Seems like we get a lot of comments that have to do with the location of something
# or looking for something. Let's see the total number of comments that fall under 
# this category. 
comments <- gen.info[, Comments]
locs.comments <- comments[grep("location", comments, ignore.case = TRUE)]
locs.comments <- c(locs.comments, comments[grep("looking", comments, ignore.case = TRUE)])

# Now that we have the total, let's find the proportion of comments regarding the 
# location of something on campus with the total number of comments
length(locs.comments) / length(comments)

# We can see that almost 20% of the comments have to do with the location of something
# on campus. Let's run some numbers on the other comments that didn't have to deal 
# with a location. To do this, we'll create a new variable. 
other.comms <- comments[-grep("location", comments, ignore.case = TRUE)]
other.comms <- other.comms[-grep("looking", other.comms, ignore.case = TRUE)]


# If we look at the responses for these comments, we can see that they are more 
# verbose than the `locs.comments` are, so we should go a differet route to 
# analyze these. To do this, we're going to do some basic bag of words text mining
# using the tm package.

# Let's begin the structuring to do some analysis
other.source <- VectorSource(other.comms) # create a source

# We have a source, now let's create a corpus
other.corpus <- VCorpus(other.source)

# This function will be used to clean the text in our corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "student"))
  return(corpus)
}

other.corpus <- clean_corpus(other.corpus) # Clean our corpus 

# Now we need to make a TermDocumentMatrix to evaluate our counts
other.TDM <- TermDocumentMatrix(other.corpus)
other.TDM <- as.matrix(other.TDM)

# See the dimensions. Where 'Docs' is the number of responses, and 'Terms' are
# the words given in each response
dim(other.TDM) 

# Now we need to come up with the counts. To do this we use rowSums, since our 
# 'Terms' are in the rows and then sort them in decreasing order
term.freqs <- rowSums(other.TDM)
term.freqs <- sort(term.freqs, decreasing = TRUE)
term.freqs <- term.freqs[1:15]

# Now we visualize the term count. First we need a data frame
term.freqs <- data.frame(term = names(term.freqs), Count = term.freqs)
ggplot(term.freqs, aes(x = reorder(term, -Count), y = Count)) + 
  geom_bar(stat = "identity", , fill = "#e2cc7c", col = "Black") +
  labs(title = "Lynx Center Survey Most Frequent Words in Comments", x = "Term")



# Now that we've looked at the "General Info" responses for the survey, let's look
# at the comments of all the other responses and see what we get. 
all.other <- clean.survey[Topic != "General Info"]$Comments 
all.other

# When we print this out and do some searching we can see that there are a lot 
# of responses that deal with the location of something. We'll want to get past these
# responses, since the location of something on campus is an easy problem to help
# a student with. We want to dig deeper and see what other problems students are facing
non.locs <- all.other[-grep("location", all.other, ignore.case = TRUE)]
non.locs <- non.locs[-grep("looking", non.locs, ignore.case = TRUE)]


# Now we have all of the different responses that don't deal with a location or
# a student looking for something. Let's quickly remove the "" responses
non.locs <- non.locs[!(non.locs == "")]

# Since these responses are verbose, let's follow the same methodology as we did
# for the `other.comms` object. 

# Begin the structuring to do some analysis
non.locs.source <- VectorSource(non.locs) # create a source

# We have a source, now let's create a corpus
non.locs.corpus <- VCorpus(non.locs.source)

non.locs.corpus <- clean_corpus(non.locs.corpus) # Clean our corpus 

# Now we need to make a TermDocumentMatrix to evaluate our counts
non.locs.TDM <- TermDocumentMatrix(non.locs.corpus)
non.locs.TDM <- as.matrix(other.TDM)

# See the dimensions. Where 'Docs' is the number of responses, and 'Terms' are
# the words given in each response
dim(other.TDM) 

# Now we need to come up with the counts. To do this we use rowSums, since our 
# 'Terms' are in the rows and then sort them in decreasing order
term.freqs <- rowSums(other.TDM)
term.freqs <- sort(term.freqs, decreasing = TRUE)
term.freqs <- term.freqs[1:20]

# Now we visualize the term count. First we need a data frame
term.freqs <- data.frame(term = names(term.freqs), Count = term.freqs)
ggplot(term.freqs, aes(x = reorder(term, -Count), y = Count)) + 
  geom_bar(stat = "identity", , fill = "#e2cc7c", col = "Black") +
  labs(title = "Lynx Center Survey Most Frequent Words in Comments", x = "Term")


