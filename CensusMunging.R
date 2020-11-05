#This code munged US census data and I provided my thought process using comments
#Munging US Census data from 2010
URLToRead <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
censusData <- read.csv(URLToRead)
#Examine structure of data to look at variables
str(censusData)
#Examine fist and last 8 rows of data to see any obvious deletions
head(censusData, 8)
tail(censusData, 8)
#I have no desire to see the regions census data so I delete the first 8 rows 
#and store it back in the variable censusData
censusData <- censusData[-1:-8,]
#Looking at the censusData in another tab, it is clear only the first 5 columns
#contain data, so I only keep the first 5 columns and again store them in censusData
censusData <- censusData[,1:5]
#Rows 52-58 contain data about Puerto Rico and no data in some, so I delete those
#rows and restore the dataframe
censusData <- censusData[-52:-58,]
#Here I assign stateName to the first row and then delete the previous, 
#unnecessary first row
censusData$stateName <- censusData[,1]
censusData <- censusData[,-1]
#All of the state names in the data have "." before the state name
#To fix this, I used the gsub function to replace all of the "." with "", the "\\" 
#is necessary so R reads "." as an actual period and not part of a command
censusData$stateName <- gsub("\\.", "", censusData$stateName)
#Next I used the gsub function to replace all commas with blanks
censusData$april10census <- gsub(",", "", censusData$X)
censusData$april10base <- gsub(",", "", censusData$X.1)
censusData$july10pop <- gsub(",", "", censusData$X.2)
censusData$july11pop <- gsub(",", "", censusData$X.3)
#In a similar way, we don't want any spaces in our data except for state names 
#so I used gsub to replace all spaces with nothing
censusData$april10census <- as.numeric(gsub(" ", "", censusData$april10census))
censusData$april10base <- as.numeric(gsub(" ", "", censusData$april10base))
censusData$july10pop <- as.numeric(gsub(" ", "", censusData$july10pop))
censusData$july11pop <- as.numeric(gsub(" ", "", censusData$july11pop))
#Once again, I look at the first 8 rows of the data to see how its looking
head(censusData, 8)
#The first 4 columns now contain unnecessary duplicate messy data so I deleted those
censusData <- censusData[,-1:-4]
#Next, I don't want the rows to have any names so I set rownames to NULL(nothing)
rownames(censusData) <- NULL
#Next, I wanted to make the column names easier to understand
#First, I created a vector called cnames and assigned the desired names to it
cnames <- c("StateName", "Census", "Estimates", "Population2010", "Population2011")
#Then, I assigned the vector cnames to the column names of censusData using the colnames
#function
colnames(censusData) <- cnames
#Then, I wanted to sort the data from highest population to lowest so I used the order
#function and the "-" to sort them
sortedStates <- censusData[order(-censusData$Population2011), ]
#Now we can take a look at our clean and sorted data
sortedStates


