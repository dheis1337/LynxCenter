# The purpose of this script is to utilize the `LynxCenterSurveyClean` script
# to cycle through all of the workbooks of the survey results and compile them
# into one data.table. Let's begin by sourcing the `LynxCenterSurveyClean` script.
source("LynxCenterSurveyClean.R")

# Let's remove the unnecessary objects from the `LynxCenterSurveyClean.R` script
rm(survey)
rm(path)

# The sheets we want from our working directory 
books <- c("September_2015.xlsx", "October_2015.xlsx", "November_2015.xlsx", "December_2015.xlsx") 


path <- vector("character", length = 0) # Create dummy variable for paths
surveys <- vector("list", length = 0) # Create dummy variable for each survey workbook
# Loop through the sheets object, paste the directory to the sheet name and read 
# each sheet into a data frame. The first level of elements of the `surveys` list are, 
# themselves lists, which contains each of workbooks. The second level of elements
# is a data frame, each containing the survey sheets within their respective workbooks.
for (i in 1:length(books)) {
  path[i] <- paste("C:/MyStuff/DataScience/Projects/LynxCenter/", books[i], sep = "") 
  surveys[[i]] <- lapply(excel_sheets(path[i]), read_excel, path = path[i], col_names = TRUE, 
                         na = "", skip = 3)
}

# Loops through and applies RowColClean to each data frame 
for(i in 1:length(surveys)) {
  surveys[[i]] <- lapply(surveys[[i]], RowColClean)
}

# Loops through and applies TimeClean to each data frame 

for (i in 1:length(surveys))
  for (j in 1:length(surveys[[i]])) {
    surveys[[i]][[j]]$Time <- TimeClean(surveys[[i]][[j]])
}

# One quick fix we need to make at this point is that the `Date` column of the
# first two data frames in the 4th subelement of `surveys` and the last two data
# frames of the 3rd element has been read in funny. We need to convert them to the correct date. 

surveys[[3]][[2]]$Date <- as.Date(surveys[[3]][[2]]$Date, origin = "1900-01-01")
surveys[[3]][[3]]$Date <- as.Date(surveys[[3]][[3]]$Date, origin = "1900-01-01")
surveys[[4]][[1]]$Date <- as.Date(surveys[[4]][[1]]$Date, origin = "1900-01-01")
surveys[[4]][[2]]$Date <- as.Date(surveys[[4]][[2]]$Date, origin = "1900-01-01")

# Loops through and applies DateClean to each data frame 
datepos <- vector("numeric", length = 0) #dummy variable for DateClean function
for (i in 1:length(surveys)) {
  for (j in 1:length(surveys[[i]])) {
    surveys[[i]][[j]]$Date <- DateClean(surveys[[i]][[j]])
  }
}

# Loops through and applies ObservationClean to each data frame 
for (i in 1:length(surveys)) {
  for (j in 1:length(surveys[[i]])) {
    surveys[[i]][[j]] <- ObservationClean(surveys[[i]][[j]])
  }
}

# Loops through and applies ColUnite to each data frame 
for (i in 1:length(surveys)) {
  for (j in 1:length(surveys[[i]])) {
    surveys[[i]][[j]] <- ColUnite(surveys[[i]][[j]])
  }
}


clean.survey <- data.frame() # Create dummy data frame
# Loops through the first element of surveys, and makes each of the second-level
# elements - the data frames - into one data frame, then stores this value in 
# `clean.survey`. The result is one large data frame of all results to the survey 
# workbooks. 
for (i in 1:length(surveys)) {
  clean.survey <- rbind(clean.survey, bind_rows(surveys[[i]]))
}



