setwd("D:/Assembla/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("munge/01-A.R")

# Sets the counter for processing==========================
#========================================================
# p = 0.075 -> 80%
# p = 1.5   -> 75%
# p = 3     -> 70%
# p = 4.8   -> 65%
# p = 6.5   -> 60%
# p = 8.2   -> 55%
# p = 10    -> 50%

#QS.results.40k <- iterateOverQuality (base.ds)

remove(changes.ds,non.dupli.ds,temp.ds)





}



#201 links detected
#225 possible links detected
#6526 non-links detected
#=========================================================================
