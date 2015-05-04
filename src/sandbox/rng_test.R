### 
# test of random numbers for parallel tasks
###

library(doMC)


#set up parallel tasks

registerDoMC(5)

myfunc <- function(i){return(runif(10))}


set.seed(13)
ll <- foreach(i=1:5) %dopar% myfunc(i)

