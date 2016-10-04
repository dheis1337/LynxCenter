library(readxl) # readxl to read i
library(tidyr)

path <- c("C:/MyStuff/DataScience/Projects/LynxCenter")
setwd(path)

# Import data from Excel doc
survey <- read_excel("September_2015.xlsx", sheet = 1, col_names = TRUE, na = "", skip = 3)

# Initial exploring
str(survey) # find structure of data
dim(survey) # find dimensions


# Function to get rid of the unnecessary columns and all of the rows where there 
# aren't any observations. This is done by removing all of the rows where there are 
# aren't any observations in the `Time` column
RowColClean <- function(x) {
   a <- colnames(x) == "" # finds columns with a blank name
   x <- x[, a == FALSE] # removes columns with blank names
   x <- x[, !is.na(names(x))] # removes all partition columns
   x <- x[!is.na(x$Time), ] # Removes all NA rows 
}

survey <- RowColClean(survey) 

# Running the RowColClean function assigned a random date to the `Time` column's
# observation. The TimeClean function removes this date component and leaves 
# just the time component
TimeClean <- function(x) {
  x$Time <- gsub(".* ", "", x$Time)
}


survey$Time <- TimeClean(survey)

# The spreadsheet only has each date observation recorded once in the `Date` 
# column. In other words, date x is recorded once, and then y number of observations
# are recorded in every other column without the date x repeated to match it. 
# The DateClean function takes Date column and repeates each date observation 
# the necessary number of times to match the associated number of observations 
# on that given date
datepos <- vector("numeric", length = 0)
DateClean <- function(x) {
  uniqdates <- unique(x$Date)
  uniqdates <- uniqdates[!is.na(uniqdates)]
  
  for (i in 1:length(uniqdates)) {
    datepos[i] <- grep(uniqdates[i], x$Date)
  }
  datepos <- c(datepos, length(x$Date))
  
  fin.dates <- c(x$Date[1])
  for (i in 1:length(uniqdates)) {
    fin.dates <- c(fin.dates, rep(uniqdates[i], times = (datepos[(i + 1)] - datepos[i])))
  }
  
  fin.dates <- as.POSIXct(fin.dates, origin = "1970-01-01 00:00.00 UTC")
  fin.dates <- gsub(" .*", "", fin.dates)
  
}

survey$Date <- DateClean(survey)

# Each observation is recorded as a 1 and then associated with a specific column. 
# This function takes each 1-valued observation and replaces it with the name of 
# the column its in, i.e. with its associated value. The function sequences through
# column 3 through 40 because those are the columns with a 1-valued observation.
ObservationClean <- function(x) {
  x[is.na(x)] <- ""
  names <- colnames(x)
  for (i in 3:(ncol(x) - 1)) {
    val <- x[, i]
    val <- unlist(val)
    x[, i] <- gsub(1, replacement = names[i], val)
  }
  return(x)
}

survey <- ObservationClean(survey)

# Now that each observation is changed from a 1-valued observation to its appropriate
# topic, we know unite all of these columns into a `Topic` column. We also bring
# the `In Person` and `Phone` column into one column - `Contact Type`. Finally, 
# `Date` and `Time` into one column - `Date.Time` 
ColUnite <- function(x) {
  x <- unite(x, col = "Topic", `Freshman Orientation`, `Transfer Orientation`, `Online Orientation`,
                      `Lynx  Center Staff`, `TAGS`, `Admissions`, `Int'l Admissions`, `ASAC`,
                      `Qdoba`, `TAC`, `Financial Aid`, Bursar, Registrar, `Scholarship Resource Office`,
                      EOP, TriO, `Dissability Resources`, LRC, `Computer Lab`, COMM, HIST, 
                      MHMSS, `POLI SCI`, MATH, `AB1 Classrooms`, `CUD Buildings`, `Auraria Campus`,
                      `Health Center`, `CUD Depts. / Offices`, `General Info`, ITS, `UCDAccess/Portal`,
                      `ID Voucher`, `Lynx Center Supplies (Computer Etc.)`, `Lost and Found`, Facilities,
                      sep = "")
  x <- unite(x, col = "Date.Time", Date, Time, sep = " ")
  x <- unite(x, col = "Contact Type", Phone, `In Person`, sep = "")
}

survey <- ColUnite(survey)
survey$Date.Time <- as.POSIXct(survey$Date.Time) # Convert `Date.Time` to as.POSIXCT

survey$Topic
survey$Comments


