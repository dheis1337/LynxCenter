library(readxl) # readxl to read i
library(data.table)
library(dplyr)
library(ggplot2)
library(tm)
library(lubridate)

path <- c("C:/MyStuff/DataScience/Projects/LynxCenter")
setwd(path)

# Import data from Excel doc
survey <- read_excel("September_2015.xlsx", sheet = 1, col_names = TRUE, na = "", skip = 3)


# Initial exploring
str(survey) # find structure of data
dim(survey) # find dimensions
  
head(survey)
names(survey)


# After initial exploring, it seems we have appropriate column classes and names
# for necessary columns. However, we have a few unnecessary columns, since columns
# 51:100 don't have any names. If we look back to our spreadsheet, there is in 
# fact no data-filled columns past the Comments column, which is our 50th column.
# There are also the total rows, which we don't want. There are rows 99, 179, 260, 
# 314, 345, 346 for sheet 1. However, we skipped 3 lines, so we have to subtract 4
# from each of these. We'll run a test to see if this is accurate. 
# Let's go ahead and remove all of these unnecessary columns and rows and make the object
# a data.table. 

survey <- survey[, -c(53:99)] # remove columns
survey <- survey[, -c(2, 7, 13, 19, 24, 30, 35, 37, 43, 51)] # remove columns

# Confirm to check we're removing correct rows
survey[95, 1:6]  
survey[175, 1:6]
survey[256, 1:6]
survey[310, 1:6]
survey[341:342, 1:6]

survey <- survey[-c(95, 175, 256, 310, 341, 342), ] # remove rows


survey <- data.table(survey) # Make into a data.table

# For now, I'll focus on the Comments variable. I'll clean this up for further 
# analysis later. Let's get the comments in its own data.table

comments <- survey$Comments

# It'll be helpful to know what kind of object we have. 
mode(comments)

# Luckily in our case, we're dealing with the appropriate object mode. For the 
# bag of words analysis, we aren't concerned with NA values, which are associated
# with no answers in this survey. Let's remove those. 
comments <- comments[!is.na(comments)]

# Now that we have the responses, let's look at them a little bit and see what we 
# get. 
comments

# It seems like there are a TON of location related responses. Since there is so 
# many responses pertaining to the location of some office, let's put these into 
# their own category. 
locs <- comments[grep("location", comments)]
locs <- c(locs, comments[grep("Location", comments)])

# Now let's make some offices one word so their bunched together.
locs <- gsub("financial aid", "financialaid", locs, ignore.case = TRUE)
locs <- gsub("merc lab", "merclab", locs, ignore.case = TRUE)
locs <- gsub("campus tour", "campustour", locs, ignore.case = TRUE)

# This is relatively clean. Let's begin the structuring to do some analysis
locs.source <- VectorSource(locs) # create a source

# We have a source, now let's create a corpus
locs.corpus <- VCorpus(locs.source)

# This function will be used to clean the text in our corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "locations", "location", "office"))
  return(corpus)
}

locs.corpus <- clean_corpus(locs.corpus) # Clean our corpus 

# We're close, but we need to break down some words to their stems. Namely, let's 
# work with 'financial', 'registrars', 'advising/advisors'. 
financial <- c("financial", "finance", "financal", "financials")
bursar <- c("bursars", "bursar")
advisor <- c("advisors", "advisor", "advising")
registrar <- c("registrars", "registrar", "registars")



# Now we need to make a TermDocumentMatrix to evaluate our counts
locs.TDM <- TermDocumentMatrix(locs.corpus)
locs.TDM <- as.matrix(locs.TDM)

# See the dimensions. Where 'Docs' is the number of responses, and 'Terms' are
# the words given in each response
dim(locs.TDM) 

# Now we need to come up with the counts. To do this we use rowSums, since our 
# 'Terms' are in the rows and then sort them in decreasing order
term.freqs <- rowSums(locs.TDM)
term.freqs <- sort(term.freqs, decreasing = TRUE)
term.freqs <- term.freqs[1:15]

# Now we visualize the term count. First we need a data frame
term.freqs <- data.frame(term = names(term.freqs), Count = term.freqs)
ggplot(term.freqs, aes(x = reorder(term, -Count), y = Count)) + geom_bar(stat = "identity", , fill = "SteelBlue")



