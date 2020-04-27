# Illustration of Regression to the Mean Dynamics

# NR  O1  XA  O2
# NR  O1  XB  O2

thresh <- 1.5
perf <- rnorm(1000)

# no effect
clap <- function(kpi,th){
  return(
    sum(
      sapply(1:(length(kpi)-1),
             function(i){
               as.numeric(kpi[i]>th & kpi[i+1]>=kpi[i])
             }) 
    )/sum(as.numeric(kpi[1:(length(kpi)-1)]>=th))
  )
}

yell <- function(kpi,th){
  return(
    sum(
      sapply(1:(length(kpi)-1),
             function(i){
               as.numeric(kpi[i]<(-1*th) & kpi[i+1]>=kpi[i])
             }) 
    )/sum(as.numeric(kpi[1:(length(kpi)-1)]<=(-1*th)))
  )
}
clap(perf,thresh)
yell(perf,thresh)

p2 <- rnorm(1000000)
clap(p2,thresh)
yell(p2,thresh)
clap(p2,1)
yell(p2,1)

# claps help, yelling harms effect
delta <- 0.5
proClap <- function(kpi,th){
  return(rnorm(1,mean=as.numeric(kpi > th)*delta))
}

conYell <- function(kpi,th){
  return(rnorm(1,mean=as.numeric(kpi < (-1*th))*(-1*delta)))
}

sum(sapply(p2[which(p2>thresh)],function(r){as.numeric(proClap(r,thresh)>=r)}))/sum(as.numeric(p2>thresh))
sum(sapply(p2[which(p2<(-1*thresh))],function(r){as.numeric(conYell(r,thresh)>=r)}))/sum(as.numeric(p2<(-1*thresh)))

