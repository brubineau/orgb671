# ORGB 671 - Illustration of creating a Transition Matrix
# (c) Brian Rubineau 2020

# STEP 1: Create Dummy HR Data
# STEP 2: Use data to create transition matrix
# STEP 3: Illustrate projecting forward using transition matrix
#      3.a. : projecting forward and comparing with known data
#      3.b. : projecting forward beyond known data

##############################
# STEP 1: Create Dummy HR Data
# ----------------------------
# Create dummy HR data for Year 1 
# - Normal distribution of 1000 employees
# - jobCatCount = 19 job categories
jobCatCount <- 19
hr1 <- pmax( # nothing less than 1
  pmin(      # nothing greater than 19
    ceiling( # integers only
      rnorm(1000,
             mean=jobCatCount/2,
             sd=jobCatCount/4)),
    19,na.rm=T),
  1,na.rm=T)
summary(hr1) # check 1
table(hr1)   # check 2

# turn that into a function, taking both number of jobs and number of individuals as arguments
genJobs <- function(nJobs, nPpl){
  jobs <- pmax( # nothing less than 1
    pmin(      # nothing greater than 19
      ceiling( # integers only
        rnorm(nPpl,
              mean=nJobs/2,
              sd=nJobs/4)),
      nJobs,na.rm=T),
    1,na.rm=T)
  return(jobs)
}

# Create data frame with employee ID and job information
hr.df <- data.frame(empID = sample(1000:9999,1000,replace=F),
                    jobY1 = genJobs(19,1000))

# Create HR data for Year 2
# - for each job, 75% stay, 8% promoted 1 level, 4% promoted 2 levels, 2% demoted 1 level
# - cannot go below 1 or over 19
# - 10% exit
# - 100 new people enter following the distribution above in creating hr1
# - new employees need new empIDs

# update job info for current employees
hr.df$jobY2 <- sapply(hr.df$jobY1,function(j){
  jobChange <- sample(c(-1,0,1,2,NA),1,prob=c(0.02,0.75,0.08,0.04,0.1)) # NA means exit
  newJob <- max(min(19,j+jobChange),1)
})

# create new job entrants
entrants <- data.frame(empID = sample(setdiff(1000:9999,hr.df$empID),100,replace=F),
                        jobY1 = NA,
                        jobY2 = genJobs(19,100))

# bring in new entrants to the data frame
hr.df <- rbind(hr.df,entrants)


##############################################
# STEP 2: Use data to create transition matrix
# --------------------------------------------

# Use data to identify the set of job categories
jobSet <- sort(unique(as.vector(apply(hr.df[,c("jobY1","jobY2")],MAR=2,FUN=c))))

# Create a matrix of job changes - NOTE: only includes employees present in BOTH YEARS
jobChange.mat <- matrix( # make the output a matrix rather than a table
  table(hr.df[,c("jobY2","jobY1")]), # the table command sorts automatically
  nrow=length(jobSet),ncol=length(jobSet)) # ensure the dimensions are correct

# Add Column for entrants
# need to ensure we have values for each job level - number of entrants might be 0 for some jobs
fromOutside <- data.frame(jobLevel = jobSet,
                          entrantCount = sapply(jobSet,function(j){
                            return(sum(as.numeric(hr.df$jobY2[which(is.na(hr.df$jobY1))]==j),na.rm=T))
                          }))
sum(fromOutside$entrantCount) # check - should add to 100 (the number of people coming from outside)

# Add this column to jobChange.mat
jobChange.mat <- cbind(jobChange.mat,fromOutside$entrantCount)

# Add Row for exits
# need to ensure we have values for each job level - number of exiters might be 0 for some jobs
exits <- data.frame(jobLevel = jobSet,
                          exitCount = sapply(jobSet,function(j){
                            return(sum(as.numeric(hr.df$jobY1[which(is.na(hr.df$jobY2))]==j),na.rm=T))
                          }))
# Add this row to jobChange.mat
# NOTE: Must add 0 to end of exit vector, 
# because now jobChange.mat has a column (col 20) for people coming from outside, which is not a job category
# the 0 at the end ensures the dimensions match, without interfering with later matrix multiplication
jobChange.mat <- rbind(jobChange.mat,c(exits$exitCount,0)) 

rownames(jobChange.mat) <- c(paste0("A",jobSet),"Exits")
colnames(jobChange.mat) <- c(paste0("A",jobSet),"Out")

# Convert to Transition Matrix by dividing by columnSums
# we do this by matrix-multiplying the jobChange.mat matrix 
# by a diagonal matrix containing 1/(column sums from the matrix)
y1.y2.tm <- jobChange.mat %*% diag(1/colSums(jobChange.mat,na.rm=T))

table(colSums(y1.y2.tm,na.rm=T)) # check - column sums should all == 1


################################################################
# STEP 3: Illustrate projecting forward using transition matrix
#      3.a. : projecting forward and comparing with known data
#      3.b. : projecting forward beyond known data
# --------------------------------------------------------------

# Projection @ time t+1: TM %*% (hr(t) & # from outside)
y2.proj <- y1.y2.tm %*%
  as.matrix(                          # make it class matrix to allow smooth matrix multiplication
    c(                                # combine / concatenate function to add # new entrants at end
      as.vector(table(hr.df$jobY1)),  # vector of people in jobs (note - table runs the risk of omitting categories w/0 employees, which would create an error)
              100),                   # 100 new entrants at end of column vector
    nrow=(length(jobCatCount)+1),ncol=1) # set dimensions of column vector explicitly

cbind(y2.proj, c(as.vector(table(hr.df$jobY2)),NA)) # compare. Note perfect match (except didn't count exits)

# create Y3 projection from time 2 composition, then create Y3 counts using same process above. Compare
y3.proj <- y1.y2.tm %*%
  as.matrix(                          # make it class matrix to allow smooth matrix multiplication
    c(                                # combine / concatenate function to add # new entrants at end
      as.vector(table(hr.df$jobY2)),  # vector of people in jobs (note - table runs the risk of omitting categories w/0 employees, which would create an error)
      100),                   # 100 new entrants at end of column vector
    nrow=(length(jobCatCount)+1),ncol=1) # set dimensions of column vector explicitly

# update job info for current employees
hr.df$jobY3 <- sapply(hr.df$jobY2,function(j){
  jobChange <- sample(c(-1,0,1,2,NA),1,prob=c(0.02,0.75,0.08,0.04,0.1)) # NA means exit
  newJob <- max(min(19,j+jobChange),1)
})

# create new job entrants
y3entrants <- data.frame(empID = sample(setdiff(1000:9999,hr.df$empID),100,replace=F),
                       jobY1 = NA,
                       jobY2 = NA,
                       jobY3 = genJobs(19,100))

# bring in new Y3 entrants to the data frame
hr.df <- rbind(hr.df,y3entrants)

# compare
cbind(y3.proj, c(as.vector(table(hr.df$jobY3)),NA)) # compare. Note perfect match (except didn't count exits)

# summary of errors
summary(y3.proj- c(as.vector(table(hr.df$jobY3)),NA))
sd(y3.proj- c(as.vector(table(hr.df$jobY3)),NA),na.rm=T)
cor(y3.proj, c(as.vector(table(hr.df$jobY3)),NA),use="p")

# plot distribution of errors
plot(density(y3.proj- c(as.vector(table(hr.df$jobY3)),NA),na.rm=T))
