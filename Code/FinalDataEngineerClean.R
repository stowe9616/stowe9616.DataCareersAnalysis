library(tidyr)
library(stringr)
library(dplyr)
library(stringr)
library(writexl)
library(readr)
DataEngineerData<-read.csv("C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/DataEngineer.csv")
# Data is here https://www.kaggle.com/datasets/andrewmvd/data-engineer-jobs

###########Convert Salary format from one column of "$20K - 80K (Glassdoor Estimate)" to 2 columns of numbers#########-----
###############################################################################################################################################

#Split off the salary column
DataEngineerSalaries<-as.data.frame(str_split_fixed(DataEngineerData$Salary.Estimate,"-",2))
DataEngineerSalaries$V2<-substr(DataEngineerSalaries$V2,0,5)

#Remove the excess characters from the data
DataEngineerSalaries<-lapply(DataEngineerSalaries,gsub,pattern='[[:punct:]]',replacement='')
DataEngineerSalaries<-lapply(DataEngineerSalaries,gsub,pattern='K',replacement='')

#Convert to Data frame and transpose the data to be in the correct configuration
DataEngineerSalaries<-data.frame(Reduce(rbind,DataEngineerSalaries))
DataEngineerSalaries<-as.data.frame(t(DataEngineerSalaries))

#Rename
DataEngineerSalaries<- DataEngineerSalaries %>%
  rename(
    MinSalary = init,
    MaxSalary = V2
  )

#Multiply by 1000 to get the actual salaries
DataEngineerSalaries$MinSalary<-as.numeric(DataEngineerSalaries$MinSalary) * 1000
DataEngineerSalaries$MaxSalary<-as.numeric(DataEngineerSalaries$MaxSalary) * 1000

DataEngineerSalaries<-as.vector(DataEngineerSalaries)
DataEngineerData$MinSalary<-DataEngineerSalaries$MinSalary
DataEngineerData$MaxSalary<-DataEngineerSalaries$MaxSalary

###########Prepare Job Description #########-----
###############################################################################################################################################


#Convert to lowercase
DataEngineerData$Job.Description<-tolower(DataEngineerData$Job.Description)

#Get rid of spaces
#DataEngineerData$Job.Description<-str_replace_all(DataEngineerData$Job.Description,fixed(" "),"")

#Create new binary columns for all of the skills listed below
DataEngineerData <- DataEngineerData %>%
  dplyr::mutate(
    python = as.integer(grepl('python',Job.Description)),
    hadoop = as.integer(grepl('hadoop',Job.Description)),
    mongo =  as.integer(grepl('mongo',Job.Description)),
    spark =  as.integer(grepl('spark',Job.Description)),
    sql =  as.integer(grepl('sql',Job.Description)),
    tableau =  as.integer(grepl('tableau',Job.Description)),
    kafka =  as.integer(grepl('kafka',Job.Description)),
    #Using word boundary anchors here to not get words like "Excellent"
    excel =  as.integer(grepl('\\bexcel\\b',Job.Description)),
    azure =  as.integer(grepl('azure',Job.Description)),
    pandas=  as.integer(grepl('pandas',Job.Description)),
    visualization =  as.integer(grepl('visualization',Job.Description)),
    machinelearning =  as.integer(grepl('machine learning',Job.Description)),
   #Word boundary anchor again
     aws =  as.integer(grepl('\\baws\\b',Job.Description)),
    saas =  as.integer(grepl('saas',Job.Description)),
    etl =  as.integer(grepl('etl',Job.Description)),
    warehouse =  as.integer(grepl('warehous',Job.Description)),
    total =  as.integer(grepl('',Job.Description))
  )
#Count the sums of each column
Sums<-setNames(nm=c('colname','Sum'),stack(colSums(DataEngineerData[,16:31]))[2:1])
Sums$Percentages<-Sums$Sum/nrow(DataEngineerData)
Sums
#Export to excel to use for visualization
#write_xlsx(Sums,"C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/DataEngineerExport.xlsx")

#####################Converting Locations to City/State and removing non-US#################################
###############################################################################################################################################

#Use string split to split the locations into city and state vectors
DataEngineerData<-DataEngineerData %>% separate(Location,c('City','State'),sep=",")

DataEngineerData[!(nchar(as.character(DataEngineerData$State)) > 2),]



#####################Prep of Job Titles #################################
###############################################################################################################################################



DataEngineerData$Job.Title = toupper(DataEngineerData$Job.Title)
counts_DataEngineerData = as.data.frame(sort(table(DataEngineerData$Job.Title),decreasing = TRUE))
head(counts_DataEngineerData,15)

# 1) Remove any jobs that do not include 'data' 
########################################################################################################################
tList = list()

for(i in 1:nrow(DataEngineerData)){
  jobTitle = DataEngineerData[i,1]
  
  # remove any titles that do not include 'data' 
  if(isFALSE(grepl('data', jobTitle, ignore.case = TRUE))){
    jobTitle = "IGNORE"
  }
  
  tList[i] = jobTitle
}

# jobs to remove have the title 'IGNORE' --> I remove them here
DataEngineerData$Job.Title = as.character(tList)
DataEngineerData = DataEngineerData[DataEngineerData$Job.Title != 'IGNORE',]
counts_DataEngineerData = as.data.frame(sort(table(DataEngineerData$Job.Title),decreasing = TRUE))
head(counts_DataEngineerData, 15)
########################################################################################################################

# consolidate
########################################################################################################################

tList2 = list()
DataEngineerData$Job.Title = toupper(DataEngineerData$Job.Title)

for(i in 1:nrow(DataEngineerData)){
  jobTitle = DataEngineerData[i,1]
  
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

DataEngineerData$Job.Title = as.character(tList2)
counts_DataEngineerData = as.data.frame(sort(table(DataEngineerData$Job.Title),decreasing = TRUE))
head(counts_DataEngineerData, 15)

########################################################################################################################

# further consolidation
########################################################################################################################
# AT THIS POINT: 6 PRIMARY JOB TITLES ACCOUNT FOR 91+% OF DATA POINTS
# I WILL REMOVE EVERYTHING ELSE TO FILTER OUT:
# - LEADERSHIP POSITIONS (i.e. Data Science Director)
# - CROSS-NAMING (i.e. Data Science Engineer)
# - AMBIGUOUS RANKS (i.e. Data Science Consultant)


tList3 = list()

for(i in 1:nrow(DataEngineerData)){
  jobTitle = DataEngineerData[i,1]
  
  if(isFALSE(grepl('DATA SCIENTIST|SENIOR DATA SCIENTIST|DATA ENGINEER|SENIOR DATA ENGINEER|DATA ANALYST|SENIOR DATA ANALYST', 
                   jobTitle, ignore.case = TRUE))){
    jobTitle = "IGNORE"}
  
  if(grepl('VP|DIRECTOR|MANAGER|PRESIDENT|INTERN|INTERNSHIP|OFFICER', jobTitle, ignore.case = TRUE)){
    jobTitle = "IGNORE"}
  
  tList3[i] = jobTitle
}

DataEngineerData$Job.Title = as.character(tList3)
DataEngineerData = DataEngineerData[DataEngineerData$Job.Title != 'IGNORE',]
counts_DataEngineerData = as.data.frame(sort(table(DataEngineerData$Job.Title),decreasing = TRUE))
head(counts_DataEngineerData, 15)


########################################################################################################################

# revert to original capitalization
########################################################################################################################
DataEngineerData$Job.Title[DataEngineerData$Job.Title == "DATA SCIENTIST"] = "Data Scientist"
DataEngineerData$Job.Title[DataEngineerData$Job.Title == "SENIOR DATA SCIENTIST"] = "Senior Data Scientist"
DataEngineerData$Job.Title[DataEngineerData$Job.Title == "DATA ENGINEER"] = "Data Engineer"
DataEngineerData$Job.Title[DataEngineerData$Job.Title == "SENIOR DATA ENGINEER"] = "Senior Data Engineer"
DataEngineerData$Job.Title[DataEngineerData$Job.Title == "DATA ANALYST"] = "Data Analyst"
DataEngineerData$Job.Title[DataEngineerData$Job.Title == "SENIOR DATA ANALYST"] = "Senior Data Analyst"

counts_DataEngineerData = as.data.frame(sort(table(DataEngineerData$Job.Title),decreasing = TRUE))
head(counts_DataEngineerData, 15)

########################################################################################################################


# consolidate senior --> regular
########################################################################################################################
tList5 = list()

for(i in 1:nrow(DataEngineerData)){
  jobTitle = DataEngineerData[i,1]
  
  if(grepl('Senior Data Scientist', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Scientist'}
  
  if(grepl('Senior Data Engineer', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Engineer'}
  
  if(grepl('Senior Data Analyst', jobTitle, ignore.case = TRUE)){
    jobTitle = 'Data Analyst'}
  
  tList5[i] = jobTitle 
}

DataEngineerData$Job.Title = as.character(tList5)
counts_DataEngineerData = as.data.frame(sort(table(DataEngineerData$Job.Title),decreasing = TRUE))
head(counts_DataEngineerData, 15)

######Get Rid of excess columns to prepare for combining data sets ###################
#####################################################################################

FinalDataEngineerData <- DataEngineerData %>%
  select(Job.Title,Company.Name,City,State,Size,MinSalary,MaxSalary,python,hadoop,mongo,
         spark,sql,tableau,kafka,excel,azure,pandas,visualization,machinelearning,aws,saas,etl,warehouse)

#Export to csv
write_csv(FinalDataEngineerData,"C:/Users/Stew/Desktop/Masters Program/MGT 6203/Project/Data/FinalDataEngineerData.csv")