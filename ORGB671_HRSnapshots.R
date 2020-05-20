# Goal for this Script:
# create and demonstrate an example (plotting) using HR snapshots

# Clear Environment
rm(list=ls())
gc()

# Load libraries
library(data.table) # helps with speed when working with large data files
library(stargazer) # for formatting regression results as tables

# LOAD CLEANED DATA FILES
# Set working directory to where the data files are stored
load("employeeDemographicsCleaned.RData")
load("employeeActionsCleaned.RData")
load("locationCleaned.RData")

# ADJUST DATA
# # Reformat Incumbent Grade into a number
empax$jobrank <- sapply(as.character(empax$Incumbent.Grade),function(g){
  as.numeric(substr(g,2,nchar(g)))
})

# # Reformat Location EffDt into date format
locs$lDate <- as.Date(as.character(locs$EffDt),format="%d.%m.%Y")

# # Make a data.table version for speedier processing
ed.dt <- setDT(empdg)
ea.dt <- setDT(empax)


exitActions <- c("Retirement","Separation","Termination")

# Assume gender is static by Index No 
genderMode <- aggregate(as.numeric(empdg$Gender=="Female"),by=list(as.character(empdg$Index.NO)),mean,na.rm=T)
genderMode$Index.No <- genderMode$Group.1
genderMode$female <- round(genderMode$x,0)

#  MAKE A FUNCTION THAT RETURNS THE JOB DETAILS (Hardship, Family) FOR A GIVEN LOCATION AT A GIVEN DATE
getLocDets <- function(loc,qDate){
  # get last date for that location before qdate
  relDate <- max(locs$lDate[which(as.character(locs$DS.Location)==loc & 
                                    locs$lDate <= qDate)])
  target <- which((as.character(locs$DS.Location)==loc)&(locs$lDate == relDate))
  return(paste(
    as.character(locs$Hardship.Classification[target]),
    as.character(locs$Family.Non.Family[target]),
    sep="."))
}

# MAKE A FUNCTION THAT RETURNS THE JOB DETAILS (Hardship, Family, Rank) FOR A GIVEN INDEX.NO on a GIVEN DATE AT A GIVEN ORGANIZATION
getJob <- function(eID,qDate,org="UNICEF"){
  # Need last action before qDate but an error if that action was termination
  relDate <- max(empax$axDate[which((as.character(empax$Index.No)==eID) & (empax$axDate <= qDate) & (empax$Organization==org))],na.rm=T)
  priorAx <- ea.dt[((Index.No==eID) & (axDate == relDate) & (Organization==org)),
                   .(Index.No,Organization,Action,axDate,rank,DS.Location)]
  term <- c("","X")[as.numeric(as.character(priorAx$Action) %in% exitActions)+1] # "X" if exited, "" if still there as of qDate
  locInfo <- getLocDets(as.character(priorAx$DS.Location[dim(priorAx)[1]]),qDate)
  return(paste(priorAx$rank,locInfo,term,sep=".",collapse = "."))
}

# Maybe faster to make a function returning all hardship info on a particular date and merge?


# MAKE A FUNCTION TO GIVE AN HR SNAPSHOT FOR A DATE AND ORG
hrsnapshot <- function(sDate, org){
  hired <- ed.dt[hire <= sDate & as.character(Organization)==org,
                 .(Index.No=as.character(Index.NO),hire,Gender=as.character(Gender))]
  maxHireDate <- hired[,maxHire:=max(hire),by=Index.No]
  uHires <- unique(maxHireDate[,.(Index.No,hire=maxHire)])
  mHires <- unique(merge(uHires,genderMode,all.x=T)) # merge in gender information
  exits <- ea.dt[as.character(Index.No) %in% hired$Index.No &
                   axDate<=sDate &
                   as.character(Action) %in% exitActions &
                   Organization==org,
                 .(Index.No=as.character(Index.No),axDate)]
  remain <- mHires[which(!(mHires$Index.No %in% exits$Index.No)),]
  remain$tenure <- as.numeric(round((sDate - remain$hire)/365.2425,2))
  remain$job <- sapply(remain$Index.No,function(e){getJob(e,sDate,org)})
  return(remain)
}

# CREATE NEEDED DATA OBJECTS
# # OPTION 1: Separately

hr2010.f <- hrsnapshot(as.Date("2010-12-31"),"UNICEF")
hr2011.f <- hrsnapshot(as.Date("2011-12-31"),"UNICEF")
hr2012.f <- hrsnapshot(as.Date("2012-12-31"),"UNICEF")
hr2013.f <- hrsnapshot(as.Date("2013-12-31"),"UNICEF")
hr2014.f <- hrsnapshot(as.Date("2014-12-31"),"UNICEF")
hr2015.f <- hrsnapshot(as.Date("2015-12-31"),"UNICEF")
hr2016.f <- hrsnapshot(as.Date("2016-12-31"),"UNICEF")
hr2017.f <- hrsnapshot(as.Date("2017-12-31"),"UNICEF")
hr2018.f <- hrsnapshot(as.Date("2018-12-31"),"UNICEF")
hr2019.f <- hrsnapshot(as.Date("2019-12-31"),"UNICEF")

# # OPTION 2: As lists
Sys.time()
hr.f <- lapply(2009:2019,
               function(y){
                 hrsnapshot(as.Date(paste0(y,"-12-31")),"UNICEF")})
# just under 3 hours
Sys.time()
names(hr.f) <- paste0("UNICEF_",2009:2019)
hr.p <- lapply(2009:2019,
               function(y){
                 hrsnapshot(as.Date(paste0(y,"-12-31")),"UNDP")})
# just over 1 hour
Sys.time()
names(hr.f) <- paste0("UNDP_",2009:2019)

# Check - are the Index Numbers unique in each snapshot?
length(hr.f$UNDP_2019$Index.No)
length(unique(hr.f$UNDP_2019$Index.No))
# Answer - YES 

hrss.f <- lapply(hr.f,function(ss){
  print(dim(ss))
  ss$rank <- NA
  ss$hard <- NA
  ss$fam <- NA
  exited <- which(unlist(sapply(ss$job,function(j){tail(strsplit(j,split="")[[1]],1)=="X"})))
  terms <- do.call(rbind,lapply(ss$job,function(j){strsplit(j,split=".",fixed=T)[[1]][1:3]}))
  ss$rank <- as.numeric(terms[,1])
  ss$hard <- terms[,2]
  ss$fam <- terms[,3]
  ss$rank[exited] <- NA
  ss$hard[exited] <- NA
  ss$fam[exited] <- NA
  return(ss)
})

hrss.p <- lapply(hr.p,function(ss){
  print(dim(ss))
  ss$rank <- NA
  ss$hard <- NA
  ss$fam <- NA
  exited <- which(unlist(sapply(ss$job,function(j){tail(strsplit(j,split="")[[1]],1)=="X"})))
  terms <- do.call(rbind,lapply(ss$job,function(j){strsplit(j,split=".",fixed=T)[[1]][1:3]}))
  ss$rank <- as.numeric(terms[,1])
  ss$hard <- terms[,2]
  ss$fam <- terms[,3]
  ss$rank[exited] <- NA
  ss$hard[exited] <- NA
  ss$fam[exited] <- NA
  return(ss)
})

save(hrss.f,file="HRSnapshotsUNICEF.RData")
save(hrss.p,file="HRSnapshotsUNDP.RData")

# ILLUSTRATION OF USE
# PLOT EMPLOYEE COUNTS BY YEAR

plot(2009:2019,unlist(lapply(hr.f,function(df){dim(df)[1]})),
     type="l",ylim=c(3000,max(c(unlist(lapply(hr.f,function(df){dim(df)[1]})),
                                unlist(lapply(hr.p,function(df){dim(df)[1]}))))),col="blue",ylab="Employee Count",xlab="Year")
text(2014,dim(hr.f[[which(2009:2019==2014)]])[1],labels="UNICEF",col="blue")
lines(2009:2019,unlist(lapply(hr.p,function(df){dim(df)[1]})),col="green")
text(2014,dim(hr.p[[which(2009:2019==2014)]])[1],labels="UNDP",col="green")
abline(v=c(2015,2016),lty=3,col="gray")

plot(2009:2019,unlist(lapply(hr.f,function(df){dim(df[which(df$female==0),])[1]})),type="l",ylim=c(1500,max(c(unlist(lapply(hr.f,function(df){dim(df[which(df$female==0),])[1]})),
                                                                                                                   unlist(lapply(hr.f,function(df){dim(df[which(df$female==1),])[1]})),
                                                                                                                   unlist(lapply(hr.p,function(df){dim(df[which(df$female==0),])[1]})),
                                                                                                                   unlist(lapply(hr.p,function(df){dim(df[which(df$female==1),])[1]}))))),col="green",ylab="Employee Count",xlab="Year")
text(2016,dim(hr.f[[which(2009:2019==2016)]][female==0,])[1],labels="UNICEF-Male",col="green")
lines(2009:2019,unlist(lapply(hr.p,function(df){dim(df[which(df$female==0),])[1]})),col="cyan")
text(2016,dim(hr.p[[which(2009:2019==2016)]][female==0,])[1],labels="UNDP-Male",col="cyan")

lines(2009:2019,unlist(lapply(hr.f,function(df){dim(df[which(df$female==1),])[1]})),col="violet")
text(2012,dim(hr.f[[which(2009:2019==2012)]][female==1,])[1],labels="UNICEF-Female",col="violet")
lines(2009:2019,unlist(lapply(hr.p,function(df){dim(df[which(df$female==1),])[1]})),col="orange")
text(2012,dim(hr.p[[which(2009:2019==2012)]][female==1,])[1],labels="UNDP-Female",col="orange")

abline(v=c(2015,2016),lty=3,col="gray")

