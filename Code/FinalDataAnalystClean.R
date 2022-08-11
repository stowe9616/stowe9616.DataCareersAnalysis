# Libraries
library(tidyr)
library(stringr)
library(dplyr)
library(stringr)
library(writexl)
###################################################################
#Load Data 
DataAnalystData<-read.csv("C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/DataAnalyst.csv")
# Data is here https://www.kaggle.com/datasets/andrewmvd/data-analyst-jobs

###########Convert Salary format from one column of "$20K - 80K (Glassdoor Estimate)" to 2 columns of numbers#########-----
###############################################################################################################################################

#Split off the salary column
DataAnalystSalaries<-as.data.frame(str_split_fixed(DataAnalystData$Salary.Estimate,"-",2))
DataAnalystSalaries$V2<-substr(DataAnalystSalaries$V2,0,5)

#Remove the excess characters from the data
DataAnalystSalaries<-lapply(DataAnalystSalaries,gsub,pattern='[[:punct:]]',replacement='')
DataAnalystSalaries<-lapply(DataAnalystSalaries,gsub,pattern='K',replacement='')

#Convert to Data frame and transpose the data to be in the correct configuration
DataAnalystSalaries<-data.frame(Reduce(rbind,DataAnalystSalaries))
DataAnalystSalaries<-as.data.frame(t(DataAnalystSalaries))

#Rename
DataAnalystSalaries<- DataAnalystSalaries %>%
  rename(
    MinSalary = init,
    MaxSalary = V2
  )

#Multiply by 1000 to get the actual salaries
DataAnalystSalaries$MinSalary<-as.numeric(DataAnalystSalaries$MinSalary) * 1000
DataAnalystSalaries$MaxSalary<-as.numeric(DataAnalystSalaries$MaxSalary) * 1000

DataAnalystSalaries<-as.vector(DataAnalystSalaries)
DataAnalystData$MinSalary<-DataAnalystSalaries$MinSalary
DataAnalystData$MaxSalary<-DataAnalystSalaries$MaxSalary

##############Prepare Job Description column for extraction to Skills columns##############

#Convert to lowercase
DataAnalystData$Job.Description<-tolower(DataAnalystData$Job.Description)

#Get rid of spaces
#DataScientistData$Job.Description<-str_replace_all(DataScientistData$Job.Description,fixed(" "),"")

#Create new binary columns for all of the skills listed below
DataAnalystData <- DataAnalystData %>%
  dplyr::mutate(
    python = as.integer(grepl('python',Job.Description)),
    hadoop = as.integer(grepl('hadoop',Job.Description)),
    mongo =  as.integer(grepl('mongo',Job.Description)),
    spark =  as.integer(grepl('spark',Job.Description)),
    sql =  as.integer(grepl('sql',Job.Description)),
    tableau =  as.integer(grepl('tableau',Job.Description)),
    kafka =  as.integer(grepl('kafka',Job.Description)),
    excel =  as.integer(grepl('\\bexcel\\b',Job.Description)),
    azure =  as.integer(grepl('azure',Job.Description)),
    pandas=  as.integer(grepl('pandas',Job.Description)),
    visualization =  as.integer(grepl('visualization',Job.Description)),
    machinelearning =  as.integer(grepl('machine learning',Job.Description)),
    aws =  as.integer(grepl('\\baws\\b',Job.Description)),
    saas =  as.integer(grepl('saas',Job.Description)),
    etl =  as.integer(grepl('etl',Job.Description)),
    warehouse =  as.integer(grepl('warehous',Job.Description)),
    total =  as.integer(grepl('',Job.Description))
  )
#Count the sums of each column
Sums<-setNames(nm=c('colname','Sum'),stack(colSums(DataAnalystData[,17:32]))[2:1])
Sums$Percentages<-Sums$Sum/nrow(DataAnalystData)
Sums
#Export to excel to use for visualization
write_xlsx(Sums,"C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/DataAnalystExport.xlsx")

#####################Converting Locations to City/State and removing non-US#################################
###############################################################################################################################################

#Use string split to split the locations into city and state vectors
DataAnalystData<-DataAnalystData %>% separate(Location,c('City','State'),sep=",")

DataAnalystData[!(nchar(as.character(DataAnalystData$State)) > 2),]


DataAnalystData$Job.Title = toupper(DataAnalystData$Job.Title)
counts_DataAnalystJobTitles = as.data.frame(sort(table(DataAnalystData$Job.Title),decreasing = TRUE))
head(counts_DataAnalystJobTitles,15)

#########Job Title Cleaning ############################

# 1) Remove any jobs that do not include 'data' and 'science|scientist|engineer|engineering|analyst'
########################################################################################################################
tList = list()

for(i in 1:nrow(DataAnalystData)){
  jobTitle = DataAnalystData[i,2]
  
  # remove any titles that do not include 'data' 
  if(isFALSE(grepl('data', jobTitle, ignore.case = TRUE))){
    jobTitle = "IGNORE"
  }
  
  tList[i] = jobTitle
}

# jobs to remove have the title 'IGNORE' --> I remove them here
DataAnalystData$Job.Title = as.character(tList)
DataAnalystData = DataAnalystData[DataAnalystData$Job.Title != 'IGNORE',]
counts_DataAnalystJobTitles = as.data.frame(sort(table(DataAnalystData$Job.Title),decreasing = TRUE))
head(counts_DataAnalystJobTitles, 15)
########################################################################################################################

# consolidate
########################################################################################################################

tList2 = list()
DataAnalystData$Job.Title = toupper(DataAnalystData$Job.Title)

for(i in 1:nrow(DataAnalystData)){
  jobTitle = DataAnalystData[i,2]
  
  if(grepl('Sr|Sr.|Senior|Lead', jobTitle, ignore.case = TRUE) & grepl('Data Scientist|Science', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Senior Data Scientist")}
  if(grepl('Sr|Sr.|Senior|Lead', jobTitle, ignore.case = TRUE) & grepl('Data Engineer', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Senior Data Engineer")}
  if(grepl('Sr|Sr.|Senior|Lead', jobTitle, ignore.case = TRUE) & grepl('Data Analyst', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Senior Data Analyst")}
  
  if(grepl('Jr|Jr.|Junior|Business', jobTitle, ignore.case = TRUE) & grepl('Data Scientist', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Data Scientist")}
  if(grepl('Jr|Jr.|Junior|Business', jobTitle, ignore.case = TRUE) & grepl('Data Engineer', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Data Engineer")}
  if(grepl('Jr|Jr.|Junior|Business', jobTitle, ignore.case = TRUE) & grepl('Data Analyst', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Data Analyst")}
  
  if(isFALSE(grepl('SENIOR|VP|DIRECTOR|MANAGER|PRESIDENT|INTERN|OFFICER', jobTitle, ignore.case = TRUE)) & grepl('Data Scientist', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Data Scientist")}
  if(isFALSE(grepl('SENIOR|VP|DIRECTOR|MANAGER|PRESIDENT|INTERN|OFFICER', jobTitle, ignore.case = TRUE)) & grepl('Data Engineer', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Data Engineer")}
  if(isFALSE(grepl('SENIOR|VP|DIRECTOR|MANAGER|PRESIDENT|INTERN|OFFICER', jobTitle, ignore.case = TRUE)) & grepl('Data Analyst', jobTitle, ignore.case = TRUE)){
    jobTitle = toupper("Data Analyst")}
  
  tList2[i] = jobTitle
}

DataAnalystData$Job.Title = as.character(tList2)
counts_DataAnalystJobTitles = as.data.frame(sort(table(DataAnalystData$Job.Title),decreasing = TRUE))
head(counts_DataAnalystJobTitles, 15)

########################################################################################################################

# further consolidation
########################################################################################################################
# AT THIS POINT: 6 PRIMARY JOB TITLES ACCOUNT FOR 91+% OF DATA POINTS
# I WILL REMOVE EVERYTHING ELSE TO FILTER OUT:
# - LEADERSHIP POSITIONS (i.e. Data Science Director)
# - CROSS-NAMING (i.e. Data Science Engineer)
# - AMBIGUOUS RANKS (i.e. Data Science Consultant)


tList3 = list()

for(i in 1:nrow(DataAnalystData)){
  jobTitle = DataAnalystData[i,2]
  
  if(isFALSE(grepl('DATA SCIENTIST|SENIOR DATA SCIENTIST|DATA ENGINEER|SENIOR DATA ENGINEER|DATA ANALYST|SENIOR DATA ANALYST', 
                   jobTitle, ignore.case = TRUE))){
    jobTitle = "IGNORE"}
  
  if(grepl('VP|DIRECTOR|MANAGER|PRESIDENT|INTERN|INTERNSHIP|OFFICER', jobTitle, ignore.case = TRUE)){
    jobTitle = "IGNORE"}
  
  tList3[i] = jobTitle
}

DataAnalystData$Job.Title = as.character(tList3)
DataAnalystData = DataAnalystData[DataAnalystData$Job.Title != 'IGNORE',]
counts_DataAnalystJobTitles = as.data.frame(sort(table(DataAnalystData$Job.Title),decreasing = TRUE))
head(counts_DataAnalystJobTitles, 15)


########################################################################################################################

# revert to original capitalization
########################################################################################################################
DataAnalystData$Job.Title[DataAnalystData$Job.Title == "DATA SCIENTIST"] = "Data Scientist"
DataAnalystData$Job.Title[DataAnalystData$Job.Title == "SENIOR DATA SCIENTIST"] = "Senior Data Scientist"
DataAnalystData$Job.Title[DataAnalystData$Job.Title == "DATA ENGINEER"] = "Data Engineer"
DataAnalystData$Job.Title[DataAnalystData$Job.Title == "SENIOR DATA ENGINEER"] = "Senior Data Engineer"
DataAnalystData$Job.Title[DataAnalystData$Job.Title == "DATA ANALYST"] = "Data Analyst"
DataAnalystData$Job.Title[DataAnalystData$Job.Title == "SENIOR DATA ANALYST"] = "Senior Data Analyst"

counts_DataAnalystJobTitles = as.data.frame(sort(table(DataAnalystData$Job.Title),decreasing = TRUE))
head(counts_DataAnalystJobTitles, 15)

########################################################################################################################


# consolidate senior --> regular
########################################################################################################################
tList5 = list()

for(i in 1:nrow(DataAnalystData)){
  jobTitle = DataAnalystData[i,2]
  
  if(grepl('Senior Data Scientist', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Scientist'}
  
  if(grepl('Senior Data Engineer', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Engineer'}
  
  if(grepl('Senior Data Analyst', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Analyst'}
  
  tList5[i] = jobTitle 
}

DataAnalystData$Job.Title = as.character(tList5)
sortedCounts4 = as.data.frame(sort(table(DataAnalystData$Job.Title),decreasing = TRUE))
head(sortedCounts4, 15)


######Get Rid of excess columns to prepare for combining data sets ###################
#####################################################################################

FinalDataAnalystData <- DataAnalystData %>%
  select(Job.Title,Company.Name,City,State,Size,MinSalary,MaxSalary,python,hadoop,mongo,
         spark,sql,tableau,kafka,excel,azure,pandas,visualization,machinelearning,aws,saas,etl,warehouse)

#Export to csv
write_csv(FinalDataAnalystData,"C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/FinalDataAnalystData.csv")