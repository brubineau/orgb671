# Importing and Cleaning the 6 UNICEF/UNDP Data Files in R
# ORGB 671 - 2020-05-11

# Data Files:
# # FILENAME                   NUMBER OF LINES IN FILE
# # 1. Location.CSV                  6025
# # 2. Summary of Vacancies.txt     20061
# # 3. Performance.txt              44267
# # 4. Employee Demographics.txt    93004
# # 5. Internal Applicants.txt     296001
# # 6. Employee Actions.txt        615282
# # NOTE: Count of lines was done in windows cmd terminal using
# #       find /v /c "" "Filename.ext"
# #       Python can do this quite easily as well.

# Clear Environment
rm(list=ls())
gc()

# Load libraries
library(data.table)

# TRY LOADING DATA AND LOOK FOR PROBLEMS
#########################################
# # 1. Location.CSV                  6025
locs <- read.csv("../Location.CSV") # No problems detected - imports properly without issue
save(locs,file="locationCleaned.RData")

#########################################
# # 2. Summary of Vacancies.txt     20061
vacx <- read.csv("../Summary of Vacancies.txt") # Filename ends with "txt", but is a CSV
# # # Potential issue: imports with 22,118 observations, but should not be more than 20,060 (1st row is header)
# # # Potential issue: "Organization" shows 16 levels, but should have 2.
# # # Potential issue: "Position Grade" shows a value "17201", but should be 1-19.
# # # Potential issue: "Internal...External" shows 3 levels, but should be 2.
# # # Potential issue: "Vacancy Year" shows non-year values.
check <- vacx[which(vacx$Organization!="UNICEF" & vacx$Organization!="UNDP"),] 
# # # has 2060 obs
# # # note 20060 + 2060 = 22120 - 2 away from number of observations.
View(check)
# # # may be an overlapping row. Try looking at the prior row too
check <- vacx[sort(c(which(vacx$Organization!="UNICEF" & vacx$Organization!="UNDP"),
                     which(vacx$Organization!="UNICEF" & vacx$Organization!="UNDP")-1)),] 
View(check)
# # # Issue identified: these Titles included a delimiter character causing the year to create a new observation.
# # # Issue resolution: (1) remove problematic rows, (2) adjust them properly, (3) re-combine them
# # # vacx1 are the observations that have "UNICEF" or "UNDP" as organization variable values
vacx1 <- vacx[-1* sort(c(which(vacx$Organization!="UNICEF" & vacx$Organization!="UNDP"),
                         which(vacx$Organization!="UNICEF" & vacx$Organization!="UNDP")-1)),]
# # #  Step (2) - adjust the problematic observations
# # # check has the observations that have other values for Orgainzation
check1 <- check[2*(1:(dim(check)[1]/2))-1,] # odd rows in check (good until title)
check2 <- check[2*(1:(dim(check)[1]/2)),] # even rows in check (the overflow)
check2$firstBlank <- apply(check2,MAR=1,function(r){min(which(nchar(as.character(r))==0))}) # column number of first blank (character length==0) column entry in the row (immediately prior is year, everything before that is part of title)
check2$titleText <- NA
check2$titleText[which(check2$firstBlank>2)] <- unlist(sapply(which(check2$firstBlank>2),
                                                              function(r){paste(as.character(check2[r,1:(check2$firstBlank[r]-2)]),sep=" ",collapse=" ")}))
check2$year <- sapply(1:dim(check2)[1],
                      function(r){
                        as.numeric(as.character(check2[r,check2$firstBlank[r]-1]))}) # collect the year entries into a single numeric variable
check1$newTitle <- sapply(1:dim(check2)[1],
                          function(r){
                            paste(as.character(check1$Post.title[r]),check2$titleText[r],sep=" ",collapse=" ")}) # combine the check1 title with the stray title text in check2 into a single character variable
check1$vYear <- check2$year # bring the numeric year from check2
vacx1$vYear <- as.numeric(as.character(vacx1$Vacancy.Year)) # create the same numeric year variable in vacx1
vacx1$newTitle <- as.character(vacx1$Post.title) # create the same named character variable as in check1
# # # Step (3) - combine them
vacxNew <- rbind(vacx1,check1) # 20059 obs
# # # clean up intermediate objects
rm(check)
rm(check1)
rm(check2)
rm(vacx1)
# # # New & Remaining issues:
# # # 1 observation with no organization
# # # 1 + 41 = 42 erroneous Position Grades (length 0 or length1)
# # # 7903 vacancies neither internal nor external 
# # # 1 observation has a title that is 90117 characters long - this is likely hiding many observations.

check <- vacxNew[sort(c(which(nchar(as.character(vacxNew$Organization))<2 |
                                nchar(as.character(vacxNew$Position.Grade))<2),
                        which(nchar(as.character(vacxNew$Organization))<2 |
                                nchar(as.character(vacxNew$Position.Grade))<2)-1)),]
# # # no clear pattern explaining the missing position grades. Many Index.No are just "Z"
# # # missing organization observation can be dropped - it is not a vacancy
# # # missing grades can be treated as missing - changed to NA
# # # last issue is disaggregating the observation with a 90117 character title.
# # # Process: (1) remove that observation (and other errors per above)
# # #          (2) Try to reconstruct the observations
# # #          (3) Recombine
# # # Process: (1) remove that observation (and other errors per above)
toFix <- vacxNew[which(nchar(as.character(vacxNew$newTitle))>200),]
vacxNew <- vacxNew[-1*(which(as.character(vacxNew$Organization)=="" |
                               nchar(as.character(vacxNew$newTitle))>200)),]
vacxNew$Position.Grade[which(nchar(as.character(vacxNew$Position.Grade))<2)] <- NA
# # #          (2) Try to reconstruct the observations
vEntries <- as.character(toFix$Post.title)
vEntries1 <- strsplit(vEntries,split="\n",fixed=T)[[1]]
vEntries2 <- sapply(vEntries1,strsplit,split=",",fixed=T)
# # # 1086 entries
# # # the first entry has the values for the observation starting the row
# # # the remaining 1085 are potentially new entries.
# # # the average text length for the other entries in this table is ~105 characters
# # # 105 * 1085 is a bit over the 90117 characters, but it looks like about 1000 observations or so were hidden in this text entry
# # # most of the lists in vEntries2 are of length 9 (955 are) - and the vacancies table has 9 variables
# # # last entry is short (8) - missing year. May be the errant year from before
# # # the position of the 90k-character post.title is observation 898 in the original file
# # # observation 899 is an erroneous observation with just a year, 2018, (in the Post.No variable) and nothing else
# # # I assume this year belongs to the last entry
vEntries3 <- lapply(vEntries2,
                    function(e){
                      le <- length(e)
                      if (le>7) {
                        pt <- paste(e[8:max(le-1,8)],sep=" ",collapse=" ")
                        yr <- if (le==8) {2018} else {e[le]}
                        return(c(Post.No=e[1],
                                 Index.No=e[2],
                                 Organization=e[3],
                                 Requisition.No=e[4],
                                 Position.Grade=e[5],
                                 No.of.Applications.Submitted=e[6],
                                 Internal...External=e[7],
                                 Post.title=pt,
                                 Vacancy.Year=yr))
                        }
                      })
vEntries.df <- data.frame(Post.No=unlist(sapply(vEntries3[2:1086],function(e){e[1]})),
                          Index.No=unlist(sapply(vEntries3[2:1086],function(e){e[2]})),
                          Organization=unlist(sapply(vEntries3[2:1086],function(e){e[3]})),
                          Requisition.No=unlist(sapply(vEntries3[2:1086],function(e){e[4]})),
                          Position.Grade=unlist(sapply(vEntries3[2:1086],function(e){e[5]})),
                          No.of.Applications.Submitted=as.numeric(unlist(sapply(vEntries3[2:1086],function(e){e[6]}))),
                          Internal...External=unlist(sapply(vEntries3[2:1086],function(e){e[7]})),
                          Post.title=unlist(sapply(vEntries3[2:1086],function(e){e[8]})),
                          Vacancy.Year=as.numeric(unlist(sapply(vEntries3[2:1086],function(e){e[9]})))
)
vEntries.df$vYear <- vEntries.df$Vacancy.Year
vEntries.df$newTitle <- as.character(vEntries.df$Post.title)
toFix$Post.title <- vEntries2[[1]][1]
toFix$Vacancy.Year <- as.numeric(vEntries2[[1]][2])
toFix$newTitle <- toFix$Post.title

# # #          (3) Recombine
fixed <- rbind(toFix,vEntries.df)
vacxAll <- rbind(vacxNew,fixed) # 21143 entries
# # # entry 21115 still shifted
shiftedRow <- vacxAll[which(nchar(as.character(vacxAll$Organization))==0),]
shiftedRow$Organization <- vacxAll[which(nchar(as.character(vacxAll$Organization))==0),4]
shiftedRow$Requisition.No <- vacxAll[which(nchar(as.character(vacxAll$Organization))==0),5]
shiftedRow$Position.Grade <- NA
shiftedRow$No.of.Applications.Submitted <- vacxAll[which(nchar(as.character(vacxAll$Organization))==0),8]
shiftedRow$Internal...External <- NA
shiftedRow$Post.title <- NA
shiftedRow$newTitle <- NA
vacxFixed <- rbind(vacxAll[-1*which(nchar(as.character(vacxAll$Organization))==0),],shiftedRow)
vacx <- vacxFixed # 21143 entries
save(vacx,file="vacanciesCleaned.RData")
# # # cleanup
rm(vacxAll)
rm(vacxNew)
rm(vacxFixed)
rm(shiftedRow)
rm(fixed)
rm(toFix)
rm(list=ls(pattern="vEntries"))

# # 3. Performance.txt              44267
perf <- read.delim("Performance.txt")
# # All good - 761 entries with missing ratings
save(perf,file="performanceCleaned.RData")

#########################################
# # 4. Employee Demographics.txt    93004
empdg <- read.delim("Employee Demographics.txt") # The data documentation document indicates that this table contains all employees who worked from 2010 through 2019
check <- empdg[sort(c(which(empdg$Gender!="Male" & empdg$Gender!="Female"), # it is clear that some possible misalignments in this table. 
                      which(empdg$Gender!="Male" & empdg$Gender!="Female")-1,  # non-gender values are in the Gender variable
                      which(substr(empdg$Nationality,1,4)=="2009"),      # There is a date/time value in Nationality
                      which(substr(empdg$Nationality,1,4)=="2009")-1)),] # this looks at those observations, and the lines before
View(check)
# # There doesn't appear to be any overflowing effects
# # Just bad entries (years for hire dates, different formats, numbers in the Gender variable, etc.)
# # Because we are using the Organization and Gender terms from this table
# # Note that the entry with a date in Nationality also has a date in Gender

empdg$Gender[which(empdg$Gender!="Male" & empdg$Gender!="Female")] <- NA # bad gender data set to missing

# # There are multiple date formats for Hire.Date
# # 168 are "NULL"
# # 1 is "00:00.0"
# # so only 169 are not able to be converted to a proper date
empdg$hire <- NA # 92995 NAs
class(empdg$hire) <- "Date" # make sure the class is "Date"
empdg$hire[which(nchar(as.character(empdg$Hire.Date))==23)] <- as.Date(substr(as.character(empdg$Hire.Date)[which(nchar(as.character(empdg$Hire.Date))==23)],1,10)) # 52834 NAs
empdg$hire[grep("/",as.character(empdg$Hire.Date))] <- as.Date(as.character(empdg$Hire.Date)[(grep("/",as.character(empdg$Hire.Date)))],format="%m/%d/%Y") # 173 NAs
empdg$hire[which(empdg$Hire.Date=="04-Mar-2014")] <- as.Date("2014-03-04") # 172 NAs
empdg$hire[which(empdg$Hire.Date=="2/29/1996")] <- as.Date("1996-02-29") # 169 NAs - Leap day problem for some reason

empdg <- empdg[which(year(empdg$hire)<2020),] # remove people hired in (or after) 2020, 92827 obs
save(empdg,file="employeeDemograpicsCleaned.RData")
rm(check) # cleanup


#########################################
# # 5. Internal Applicants.txt     296001
intapps <- read.delim("Internal Applicants.txt")
# # # newer file version loads as 90352 observations. Should be 3 times larger. 
# # # trying older verion
intapps <- read.csv("../Internal Applicants.txt")
# # # older file version loads as 92254 observations. Should be 3 times larger. 
# # # Potential Issue: Number of observations
# # # Potential Issue: Organization shows 36 levels, should have 2 - will start here
# # # Will also look at character lengths for hidden data
check <- intapps[which(intapps$Organization!="UNDP" & intapps$Organization!="UNICEF"),]
# # # all look like overflow observations
check <- intapps[sort(c(which(intapps$Organization!="UNDP" & intapps$Organization!="UNICEF"),
                        which(intapps$Organization!="UNDP" & intapps$Organization!="UNICEF")-1)),]
# # # Confirmed - Post.title overflows - will follow same process as with vacancies.
# # # Issue resolution: (1) remove problematic rows, (2) adjust them properly, (3) re-combine them
# # # iapps1 are the observations that have "UNICEF" or "UNDP" as organization variable values
iapps1 <- intapps[-1*sort(c(which(intapps$Organization!="UNDP" & intapps$Organization!="UNICEF"),
                         which(intapps$Organization!="UNDP" & intapps$Organization!="UNICEF")-1)),]
# # #  Step (2) - adjust the problematic observations
# # # check has the observations that have other values for Orgainzation
check1 <- check[2*(1:(dim(check)[1]/2))-1,] # odd rows in check (good until Post.title)
check2 <- check[2*(1:(dim(check)[1]/2)),] # even rows in check (the overflow)
check2$firstBlank <- apply(check2,MAR=1,function(r){min(which(nchar(as.character(r))==0))}) # column number of first blank (character length==0) column entry in the row (immediately prior is sometimes Hired Index.No, everything before that is part of title)
check2$titleText <- NA
check2$titleText[which(check2$firstBlank>1)] <- unlist(sapply(which(check2$firstBlank>1),
                                                              function(r){paste(as.character(check2[r,1:(check2$firstBlank[r]-1)]),sep=" ",collapse=" ")}))
check2$hiredID <- NA
class(check2$hiredID) <- "numeric"
check2$hiredID <- unlist(sapply(1:(dim(check2)[1]),
                                function(r){
                                  as.numeric(as.character(check2[r,check2$firstBlank[r]-1]))
                                  })) # collect the numeric entries of hiring outcomes where those exist
check1$newTitle <- sapply(1:(dim(check2)[1]),
                          function(r){
                            paste(as.character(check1$Post.title[r]),check2$titleText[r],sep=" ",collapse=" ")}) # combine the check1 title with the stray title text in check2 into a single character variable
check1$hiredID <- check2$hiredID # bring the numeric Hired Index.No from check2
iapps1$hiredID <- as.numeric(as.character(iapps1$Hired.Index.NO)) # create the same numeric Hired IDs variable in iapps1
iapps1$newTitle <- as.character(iapps1$Post.title) # create the same named character variable as in check1
# # # Step (3) - combine them
iappsNew <- rbind(iapps1,check1) # 90352 obs (89479 from UNDP, 873 from UNICEF)
# # # clean up intermediate objects
rm(check)
rm(check1)
rm(check2)
rm(iapps1)
# # # check out character lengths for hidden rows
table(nchar(as.character(iappsNew$Requisition.No))) # all 4, 5, or 6
table(nchar(as.character(iappsNew$System))) # all 2
table(nchar(as.character(iappsNew$Post.No))) # all 4 - 8
table(nchar(as.character(iappsNew$Post.title))) # longest "reasonable" length is 187. 9 larger by thousands
table(nchar(as.character(iappsNew$Hired.Index.NO))) # longest "reasonable" length is 153 - still likely an error - 3 larger by thousands

toFix <- iappsNew[which(nchar(as.character(iappsNew$Post.title))>200 |
                          nchar(as.character(iappsNew$Hired.Index.NO))>200),] # 9 long entries in Post.Title, and 2 long entries in Hired.Index.NO
sapply(toFix$Post.title,function(t){nchar(as.character(t))})
sapply(toFix$Hired.Index.NO,function(t){nchar(as.character(t))})
# # # Use the same approach as with vacancies
iappsOK <- iappsNew[-1*which(nchar(as.character(iappsNew$Post.title))>200 |
                          nchar(as.character(iappsNew$Hired.Index.NO))>200),] # 90343 observations

# # #          (2) Try to reconstruct the observations
iaEntries <- c(as.list(toFix$Post.title),
               as.list(toFix$Hired.Index.NO[which(nchar(as.character(toFix$Hired.Index.NO))>0)]))
iaEntries1 <- lapply(iaEntries,function(e){strsplit(as.character(e),split="\n",fixed=T)[[1]]})
iaEntriesL <- unlist(iaEntries1)
iaEntries2 <- sapply(iaEntriesL,strsplit,split=",",fixed=T)
# > table(unlist(lapply(iaEntries2,function(e){unlist(sapply(e,length))})))
# 1      2      6      7      8      9     10     11 
# 9      2 168509  32567   3947    557     67      1 
# Internal applicants has 7 variables. The longer entries are likley over-flows of the Post.title field
# the ones of length 1 or 2 are likely non-observations

iaEntries3 <- sapply(iaEntries2,
                    function(e){
                      le <- length(e)
                      if (le>5) {
                        pt <- paste(e[6:le],sep=" ",collapse=" ") # not trying to avoid bringing in Hired Index.No into Post title - few postings have the Hired Index.No
                        hin <- as.numeric(as.character(e[le]))
                        return(c(Index.NO=e[1],
                                 Organization=e[2],
                                 Requisition.No=e[3],
                                 System=e[4],
                                 Post.No=e[5],
                                 Post.title=pt,
                                 Hired.Index.NO=hin))
                      }
                    })

iaEntries.df <- data.frame(Index.NO=unlist(sapply(iaEntries3[as.numeric(which(unlist(lapply(iaEntries3,length))>5))],function(e){e[1]})),
                          Organization=unlist(sapply(iaEntries3[as.numeric(which(unlist(lapply(iaEntries3,length))>5))],function(e){e[2]})),
                          Requisition.No=unlist(sapply(iaEntries3[as.numeric(which(unlist(lapply(iaEntries3,length))>5))],function(e){e[3]})),
                          System=unlist(sapply(iaEntries3[as.numeric(which(unlist(lapply(iaEntries3,length))>5))],function(e){e[4]})),
                          Post.No=as.numeric(unlist(sapply(iaEntries3[as.numeric(which(unlist(lapply(iaEntries3,length))>5))],function(e){e[5]}))),
                          Post.title=unlist(sapply(iaEntries3[as.numeric(which(unlist(lapply(iaEntries3,length))>5))],function(e){e[6]})),
                          Hired.Index.NO=as.numeric(unlist(sapply(iaEntries3[as.numeric(which(unlist(lapply(iaEntries3,length))>5))],function(e){e[7]})))
)
iaEntries.df$hiredID <- iaEntries.df$Hired.Index.NO
iaEntries.df$newTitle <- as.character(iaEntries.df$Post.title)
# # # left un-fixed - the 9 entries with the hidden data 
# # # the entries that have fewer than 5 values are 11 - corresponding to the observations with the hidden data
# # # their contents are all null, so we can just keep the data until the hidden data
iaProbs <- toFix[,1:5]
iaProbs$Post.title <- NA
iaProbs$newTitle <- NA
iaProbs$Hired.Index.NO <- NA
iaProbs$hiredID <- NA

# # #          (3) Recombine
fixed <- rbind(iappsOK,iaEntries.df) # 295991
intappsAll <- rbind(fixed,iaProbs) # 296000 entries - all recovered!

save(intappsAll,file="internalApplicationsCleaned.RData")
# # # cleanup
rm(list=ls(pattern="iaEntries"))
rm(list=ls(pattern="ia"))
rm(list=ls(pattern="fix"))
rm(list=ls(pattern="check"))
rm(intapps)



#########################################
# # 6. Employee Actions.txt        615282
empax <- read.delim("empax.txt") # See example of data import for process that generated "empax.txt"
empax$axDate <- as.Date(empax$Effective.Date)
empax$rank <- sapply(as.character(empax$Incumbent.Grade),function(g){
  r <- NA
  if (nchar(g)>1) {
    r <- as.numeric(substr(g,2,nchar(g)))
  }
  return(r)
})
# Drop observations of actions with corrupted / irrlevant Action data
empax <- empax[which(year(empax$axDate)<=2019),] # drop actions in the future or bad dates
empax <- empax[which(as.character(empax$Index.No) %in% as.character(empdg$Index.NO)),] # drop actions involving Index numbers not in the HR data
# # # result: 603860 observations
save(empax,file="employeeActionsCleaned.RData")


 