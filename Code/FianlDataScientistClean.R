library(tidyr)
library(stringr)
library(dplyr)
library(stringr)
library(writexl)
library(readr)
DataScientistData<-read.csv("C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/DataScientist.csv")
# Data is here https://www.kaggle.com/datasets/andrewmvd/data-scientist-jobs

###########Convert Salary format from one column of "$20K - 80K (Glassdoor Estimate)" to 2 columns of numbers#########-----
###############################################################################################################################################

#Split off the salary column
DataScientistSalaries<-as.data.frame(str_split_fixed(DataScientistData$Salary.Estimate,"-",2))
DataScientistSalaries$V2<-substr(DataScientistSalaries$V2,0,5)

#Remove the excess characters from the data
DataScientistSalaries<-lapply(DataScientistSalaries,gsub,pattern='[[:punct:]]',replacement='')
DataScientistSalaries<-lapply(DataScientistSalaries,gsub,pattern='K',replacement='')

#Convert to Data frame and transpose the data to be in the correct configuration
DataScientistSalaries<-data.frame(Reduce(rbind,DataScientistSalaries))
DataScientistSalaries<-as.data.frame(t(DataScientistSalaries))

#Rename
DataScientistSalaries<- DataScientistSalaries %>%
  rename(
    MinSalary = init,
    MaxSalary = V2
  )

#Multiply by 1000 to get the actual salaries
DataScientistSalaries$MinSalary<-as.numeric(DataScientistSalaries$MinSalary) * 1000
DataScientistSalaries$MaxSalary<-as.numeric(DataScientistSalaries$MaxSalary) * 1000

DataScientistSalaries<-as.vector(DataScientistSalaries)
DataScientistData$MinSalary<-DataScientistSalaries$MinSalary
DataScientistData$MaxSalary<-DataScientistSalaries$MaxSalary


#####################Prepare Job Description column for extraction to Skills columns#################################
###############################################################################################################################################

#Convert to lowercase
DataScientistData$Job.Description<-tolower(DataScientistData$Job.Description)

#Get rid of spaces
#DataScientistData$Job.Description<-str_replace_all(DataScientistData$Job.Description,fixed(" "),"")

#Create new binary columns for all of the skills listed below - for some instances I needed to use word boundary anchors (\b)
DataScientistData <- DataScientistData %>%
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
Sums<-setNames(nm=c('colname','Sum'),stack(colSums(DataScientistData[,18:34]))[2:1])
Sums
Sums$Percentages<-Sums$Sum/nrow(DataScientistData)

#Export to excel to use for visualization
#write_xlsx(Sums,"C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/DataScientistExport.xlsx")

#####################Converting Locations to City/State and removing non-US#################################
###############################################################################################################################################

#Use string split to split the locations into city and state vectors
DataScientistData<-DataScientistData %>% separate(Location,c('City','State'),sep=",")

DataScientistData[!(nchar(as.character(DataScientistData$State)) > 2),]


#Start of Wills Data



DataScientistData$Job.Title = toupper(DataScientistData$Job.Title)
sortedCounts = as.data.frame(sort(table(DataScientistData$Job.Title),decreasing = TRUE))
head(sortedCounts,20)



# 1) Remove any jobs that do not include 'data' and 'science|scientist|engineer|engineering|analyst'
########################################################################################################################
tList = list()

for(i in 1:nrow(DataScientistData)){
  jobTitle = DataScientistData[i,3]
  
  if(isFALSE(grepl('data', jobTitle, ignore.case = TRUE) & grepl('science|scientist|engineer|engineering|analyst', jobTitle, ignore.case = TRUE))){
    jobTitle = "IGNORE"}
  
  tList[i] = jobTitle
}

# jobs to remove have the title 'IGNORE' --> updated df = data2
DataScientistData$Job.Title = as.character(tList)
DataScientistData = DataScientistData[DataScientistData$Job.Title != 'IGNORE',]
sortedCounts = as.data.frame(sort(table(DataScientistData$Job.Title),decreasing = TRUE))
head(sortedCounts, 15)
########################################################################################################################

# consolidate
########################################################################################################################

tList2 = list()
DataScientistData$Job.Title = toupper(DataScientistData$Job.Title)

for(i in 1:nrow(DataScientistData)){
  jobTitle = DataScientistData[i,3]
  
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

DataScientistData$Job.Title = as.character(tList2)
sortedCounts3 = as.data.frame(sort(table(DataScientistData$Job.Title),decreasing = TRUE))
head(sortedCounts3, 15)

########################################################################################################################

# further consolidation
########################################################################################################################
# AT THIS POINT: 6 PRIMARY JOB TITLES ACCOUNT FOR 91+% OF DATA POINTS
# I WILL REMOVE EVERYTHING ELSE TO FILTER OUT:
# - LEADERSHIP POSITIONS (i.e. Data Science Director)
# - CROSS-NAMING (i.e. Data Science Engineer)
# - AMBIGUOUS RANKS (i.e. Data Science Consultant)


tList3 = list()

for(i in 1:nrow(DataScientistData)){
  jobTitle = DataScientistData[i,3]
  
  if(isFALSE(grepl('DATA SCIENTIST|SENIOR DATA SCIENTIST|DATA ENGINEER|SENIOR DATA ENGINEER|DATA ANALYST|SENIOR DATA ANALYST', 
                   jobTitle, ignore.case = TRUE))){
    jobTitle = "IGNORE"}
  
  if(grepl('VP|DIRECTOR|MANAGER|PRESIDENT|INTERN|INTERNSHIP|OFFICER', jobTitle, ignore.case = TRUE)){
    jobTitle = "IGNORE"}
  
  tList3[i] = jobTitle
}

DataScientistData$Job.Title = as.character(tList3)
DataScientistData = DataScientistData[DataScientistData$Job.Title != 'IGNORE',]
sortedCounts4 = as.data.frame(sort(table(DataScientistData$Job.Title),decreasing = TRUE))
head(sortedCounts4, 15)


########################################################################################################################

# revert to original capitalization
########################################################################################################################
DataScientistData$Job.Title[DataScientistData$Job.Title == "DATA SCIENTIST"] = "Data Scientist"
DataScientistData$Job.Title[DataScientistData$Job.Title == "SENIOR DATA SCIENTIST"] = "Senior Data Scientist"
DataScientistData$Job.Title[DataScientistData$Job.Title == "DATA ENGINEER"] = "Data Engineer"
DataScientistData$Job.Title[DataScientistData$Job.Title == "SENIOR DATA ENGINEER"] = "Senior Data Engineer"
DataScientistData$Job.Title[DataScientistData$Job.Title == "DATA ANALYST"] = "Data Analyst"
DataScientistData$Job.Title[DataScientistData$Job.Title == "SENIOR DATA ANALYST"] = "Senior Data Analyst"

sortedCounts3 = as.data.frame(sort(table(DataScientistData$Job.Title),decreasing = TRUE))
head(sortedCounts3, 15)

########################################################################################################################


# consolidate senior --> regular
########################################################################################################################
tList5 = list()

for(i in 1:nrow(DataScientistData)){
  jobTitle = DataScientistData[i,3]
  
  if(grepl('Senior Data Scientist', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Scientist'}
  
  if(grepl('Senior Data Engineer', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Engineer'}
  
  if(grepl('Senior Data Analyst', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Analyst'}
  
  tList5[i] = jobTitle 
}

DataScientistData$Job.Title = as.character(tList5)
sortedCounts4 = as.data.frame(sort(table(DataScientistData$Job.Title),decreasing = TRUE))
head(sortedCounts4, 15)

# The data frame for the DATA SCIENTIST dataset is held in 'DataScienceJobTitles'
# The data frame for the DATA ENGINEER dataset is held in 'DataEngineerJobTitles'
# The data frame for the DATA ANALYST dataset is held in 'DataAnalystJobTitles'


######Get Rid of excess columns to prepare for combining data sets ###################
#####################################################################################

FinalDataScientistData <- DataScientistData %>%
  select(Job.Title,Company.Name,City,State,Size,MinSalary,MaxSalary,python,hadoop,mongo,
         spark,sql,tableau,kafka,excel,azure,pandas,visualization,machinelearning,aws,saas,etl,warehouse)

#Export to csv
write_csv(FinalDataScientistData,"C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/FinalDataScientistData.csv")