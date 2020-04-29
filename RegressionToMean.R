# Illustration of Regression to the Mean Dynamics

# NR  O1  XA  O2
# NR  O1  XB  O2

thresh <- 1.5
perf <- rnorm(1000) # 1000 observations of performance

# no effect
clap <- function(kpi,th){  # how often, after observing at-or-above-positive-threshold behavior
  return(                  # is positive reinforcement followed by an improvement?
    sum(
      sapply(1:(length(kpi)-1),
             function(i){
               as.numeric(kpi[i]>th & kpi[i+1]>=kpi[i])
             }) 
    )/sum(as.numeric(kpi[1:(length(kpi)-1)]>=th))
  )
}

yell <- function(kpi,th){ # how often, after observing at-or-below-negative-threshold behavior
  return(                 # is negative reinforcement followed by an improvement?
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

p2 <- rnorm(1000000) # 1M observations of performance
clap(p2,thresh)
yell(p2,thresh)
clap(p2,1)
yell(p2,1)

# claps help, yelling harms effect
delta <- 0.5
proClap <- function(kpi,th){ # give improved (by delta) performance after positive reinforcement
  return(rnorm(1,mean=as.numeric(kpi > th)*delta))
}

conYell <- function(kpi,th){ # give reduced (by delta) performance after negative reinforcement
  return(rnorm(1,mean=as.numeric(kpi < (-1*th))*(-1*delta)))
}

sum(sapply(p2[which(p2>thresh)], # for each above-threshold performance in p2,
           function(r){          # give the next (w/improved mean) performance
             as.numeric(proClap(r,thresh)>=r)
             }
           ))/                  # how often is this an improvement over the
  sum(as.numeric(p2>thresh))    # behavior prompting the positive reinforcement?

sum(sapply(p2[which(p2<(-1*thresh))],  # for each below-threshold performance in p2,
           function(r){                # give the next (w/lower mean) performance
             as.numeric(conYell(r,thresh)>=r)
             }                         # how often is the next performance an
           ))/                         # improvement over the behavior prompting the
  sum(as.numeric(p2<(-1*thresh)))      # negative reinforcement?

