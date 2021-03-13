#Set working directory and import libraries
libraries <- c("forecast", "tidyverse", "ggplot2", "rlist")
lapply(libraries, library, character.only = 1)
#Import files
Florida <- data.frame(read.csv("Florida Covid History.csv"))
Connecticut <- data.frame(read.csv("Connecticut Covid History.csv"))
#Clean and rename Florida dataframe
Florida <- Florida[,c(-2,-4,-6,-8,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-23,-25,-26,-27,-29,-30,-31,-35,-36,-37,-38,-39,-40,-41)]
Florida <- Florida[c(-365:-399),]
colnames(Florida) <- c("Date", "totalDeaths", "deathsIncrease", "totalHospitalized", "currentlyHospitalized", "currentHospitalizeIncrease", "positiveCases", "positiveCasesIncrease", "positiveAntibodyTests", "positiveTests", "totalTests", "totalTestsIncrease", "totalAntibodyTests")
lapply(Florida, as.numeric)
Florida$Date <- gsub("/", "", Florida$Date)
#Clean and rename Connecticut dataframe
Connecticut <- Connecticut[,c(-2,-4,-6,-8,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-23,-25,-26,-27,-29,-30,-31,-35,-36,-37,-38,-39,-40,-41)]
Connecticut <- Connecticut[c(-1,-2,-3, -365:-368),]
colnames(Connecticut) <- c("Date", "totalDeaths", "deathsIncrease", "totalHospitalized", "currentlyHospitalized", "currentHospitalizeIncrease", "positiveCases", "positiveCasesIncrease", "positiveAntibodyTests", "positiveTests", "totalTests", "totalTestsIncrease", "totalAntibodyTests")
lapply(Connecticut, as.numeric)
Connecticut$Date <- gsub("/", "", Connecticut$Date)
#Declare other needed variables such as population and 7 day averages using while loops
FLPopulation <- 21840000
CTPopulation <- 3565000
FL7DayCaseTotals <- c()
i <- 364
#Use while loops to create 7 day averages using "i" as a placeholder
while(i > 0)
{
  total <- Florida$positiveCasesIncrease[(i-6): i]
  total <- sum(total)
  FL7DayCaseTotals <- c(FL7DayCaseTotals, total)
  i <- i - 7
}
CT7DayCaseTotals <- c()
i <- 364
while(i > 0)
{
  total <- Connecticut$positiveCasesIncrease[(i-6): i]
  total <- sum(total)
  CT7DayCaseTotals <- c(CT7DayCaseTotals, total)
  i <- i - 7
}
#Use sequence function to create dates by week for graphs
Weeks<- seq.Date(as.Date("2020-3-4"), as.Date("2021-3-2"), by = "weeks")
#Calculate average cases per day and average cases per day per 100,000
FLCasesPerDay <- mean(Florida$positiveCasesIncrease)
FLCasesPerDay
CTCasesPerDay <- mean(Connecticut$positiveCasesIncrease)
CTCasesPerDay
FLCPDPerHundredThousand <- (FLCasesPerDay/FLPopulation) * 100000
CTCPDPerHundredThousand <- (CTCasesPerDay/CTPopulation) * 100000
FLCPDPerHundredThousand
CTCPDPerHundredThousand
FL7DayCaseTotalsPerHundredThousand <- (FL7DayCaseTotals/FLPopulation) * 100000
CT7DayCaseTotalsPerHundredThousand <- (CT7DayCaseTotals/CTPopulation) * 100000
#Create dataframes for 7DayAvgesPerHundredThousand and week
FLTotalCaseAndWeek <- data.frame(Weeks, FL7DayCaseTotalsPerHundredThousand)
CTTotalCaseAndWeek <- data.frame(Weeks, CT7DayCaseTotalsPerHundredThousand)
FLTotalCaseAndWeek$Weeks <- as.Date(FLTotalCaseAndWeek$Weeks)
TotalCaseAndWeek <- data.frame(Weeks, FL7DayCaseTotalsPerHundredThousand, CT7DayCaseTotalsPerHundredThousand)
#Graph
ggplot(FLTotalCaseAndWeek,aes(x = Weeks, y= FL7DayCaseTotalsPerHundredThousand, group=1)) +
 geom_line(color = "firebrick", size = 2) + geom_point(size = 2) + labs(title = "Florida Cases Per 100,000 by Week", x = "Week", y = "Cases Per 100,000") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[4]), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[14]), linetype = "dashed") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10))

ggplot(CTTotalCaseAndWeek,aes(x = Weeks, y= CT7DayCaseTotalsPerHundredThousand, group=1)) +
  geom_line(color = "blue", size = 2) + geom_point(size = 2) + labs(title = "Connecticut Cases Per 100,000 by Week", x = "Week", y = "Cases Per 100,000") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[3]), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[36]), linetype = "dashed") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10))
#Graph both on same plot
ggplot(TotalCaseAndWeek,aes(x = Weeks, y= CT7DayCaseTotalsPerHundredThousand, group=1)) +
  geom_line(aes(colour = "Connecticut"), color = "red", size = 2) + geom_point(size = 2) + 
  geom_line(aes(colour = "Florida"), y = FL7DayCaseTotalsPerHundredThousand, color = "blue", size = 2) + geom_point(y = FL7DayCaseTotalsPerHundredThousand, size = 2)+
  labs(title = "Cases Per 100,000 by Week", x = "Week", y = "Cases Per 100,000") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10)) 
  
#Repeat previous process for deaths
#Create variables
FL7DayDeathTotals <- c()
i <- 364
while(i > 0)
{
  total <- Florida$deathsIncrease[(i-6): i]
  total <- sum(total)
  FL7DayDeathTotals <- c(FL7DayDeathTotals, total)
  i <- i - 7
}
CT7DayDeathTotals <- c()
i <- 364
while(i > 0)
{
  total <- Connecticut$deathsIncrease[(i-6): i]
  total <- sum(total)
  CT7DayDeathTotals <- c(CT7DayDeathTotals, total)
  i <- i - 7
}

#Calculate average deaths per day and average deaths per day per 100,000
FLDeathsPerDay <- mean(Florida$deathsIncrease)
FLDeathsPerDay
CTDeathsPerDay <- mean(Connecticut$deathsIncrease)
CTDeathsPerDay
FLDeathPerDayPerHundredThousand <- (FLCasesPerDay/FLPopulation) * 100000
CTDeathsPerDayPerHundredThousand <- (CTCasesPerDay/CTPopulation) * 100000
FLDeathPerDayPerHundredThousand
CTDeathsPerDayPerHundredThousand
FL7DayDeathTotalsPerHundredThousand <- (FL7DayDeathTotals/FLPopulation) * 100000
CT7DayDeathTotalsPerHundredThousand <- (CT7DayDeathTotals/CTPopulation) * 100000
#Create dataframes for 7DayDeathsPerHundredThousand and week
FLTotalDeathsAndWeek <- data.frame(Weeks, FL7DayDeathTotalsPerHundredThousand)
CTTotalDeathsAndWeek <- data.frame(Weeks, CT7DayDeathTotalsPerHundredThousand)
TotalDeathsAndWeek <- data.frame(Weeks, FL7DayDeathTotalsPerHundredThousand, CT7DayDeathTotalsPerHundredThousand)
#Create graphs for 7dayDeathsPerHundredThousand by week
ggplot(FLTotalDeathsAndWeek,aes(x = Weeks, y= FL7DayDeathTotalsPerHundredThousand, group=1)) +
  geom_line(color = "red", size = 2) + geom_point(size = 2) + labs(title = "Florida Deaths Per 100,000 by Week", x = "Week", y = "Deaths Per 100,000") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[4]), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[14]), linetype = "dashed") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10))

ggplot(CTTotalDeathsAndWeek,aes(x = Weeks, y= CT7DayDeathTotalsPerHundredThousand, group=1)) +
  geom_line(color = "blue", size = 2) + geom_point(size = 2) + labs(title = "Connecticut Deaths Per 100,000 by Week", x = "Week", y = "Deaths Per 100,000") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[3]), linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[36]), linetype = "dashed") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10))
#Both on same plot
ggplot(TotalDeathsAndWeek,aes(x = Weeks, y= CT7DayDeathTotalsPerHundredThousand, group=1)) +
  geom_line(color = "red", size = 2) + geom_point(size = 2) + 
  geom_line(y = FL7DayDeathTotalsPerHundredThousand, color = "blue", size = 2) + geom_point(y = FL7DayDeathTotalsPerHundredThousand, size = 2)+
  labs(title = "Deaths Per 100,000 by Week", x = "Week", y = "Deaths Per 100,000") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10))
#Repeat for hospitilizations
#Connecticut did not record their hospitalization increase and Florida's is off so we have to do it
i <- 1
FLHospitalizationIncrease <- c()
while(i < 365)
{
  increase <- Florida$currentlyHospitalized[i] - Florida$currentlyHospitalized[(i+1)]
  FLHospitalizationIncrease <- c(FLHospitalizationIncrease, increase)
  i <- i +1
}
Florida$currentHospitalizeIncrease <- FLHospitalizationIncrease

i <- 1
CTHospitalizationIncrease <- c()
while(i < 365)
{
  increase <- Connecticut$currentlyHospitalized[i] - Connecticut$currentlyHospitalized[(i+1)]
  CTHospitalizationIncrease <- c(CTHospitalizationIncrease, increase)
  i <- i +1
}
Connecticut$currentHospitalizeIncrease <- CTHospitalizationIncrease

#Calculate Net Hospitalizations
FL7DayHospitalizationTotals <- c()
i <- 364
while(i > 0)
{
  total <- Florida$currentHospitalizeIncrease[(i-6): i]
  total <- sum(total)
  FL7DayHospitalizationTotals <- c(FL7DayHospitalizationTotals, total)
  i <- i - 7
}

CT7DayHospitalizationTotals <- c()
i <- 364
while(i > 0)
{
  total <- Connecticut$currentHospitalizeIncrease[(i-6): i]
  total <- sum(total)
  CT7DayHospitalizationTotals <- c(CT7DayHospitalizationTotals, total)
  i <- i - 7
}

FLNetHospitalizationsAndWeeks <- data.frame(Weeks, FL7DayHospitalizationTotals)
CTNetHospitalizationsAndWeeks <- data.frame(Weeks, CT7DayHospitalizationTotals)
NetHospitalizationsAndWeeks <- data.frame(Weeks, FL7DayHospitalizationTotals, CT7DayHospitalizationTotals)
#Graph
ggplot(FLNetHospitalizationsAndWeeks,aes(x = Weeks, y= FL7DayHospitalizationTotals, group=1)) +
  geom_line(color = "red", size = 2) + geom_point(size = 2) + labs(title = "Florida Net Hospitalizations by Week", x = "Week", y = "Net Hospitalization") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[4]), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[14]), linetype = "dashed") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10))
  
ggplot(CTNetHospitalizationsAndWeeks,aes(x = Weeks, y= CT7DayHospitalizationTotals, group=1)) +
  geom_line(color = "blue", size = 2) + geom_point(size = 2) + labs(title = "Connecticut Net Hospitalizations by Week", x = "Week", y = "Net Hospitalization") +
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[3]), linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(CTTotalCaseAndWeek$Weeks[36]), linetype = "dashed") +
  scale_x_date(labels = scales::date_format("%m-%d-%Y"), breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust =1, size = 10))

