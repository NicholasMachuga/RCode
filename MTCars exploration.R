#Store mtcars in a new variable
myCars <- mtcars
#Print the first 5 rows of myCars to get a sense for the dataframe
head(myCars, 5)
#Find the max hp of the cars
max(myCars$hp)
#Print out the details of the car with the highest hp
myCars[which.max(myCars$hp),]
#print out the highest MPG from myCars
max(myCars$mpg)
#Print out the car with the highest MPG
myCars[which.max(myCars$mpg),]
#Make a function to sort the data from highest to lowest by any parameter
sortby <- function(x)
{
  myCars_sorted <- myCars[ order(-x),]
  return(myCars_sorted)
}
#Sort the cars highest to lowest by MPG
sortby(myCars$mpg)

#Make a new column in myCars for efficiency, or MPG/HP
myCars$efficiency <- myCars$mpg/myCars$hp
#Print out myCars
myCars
