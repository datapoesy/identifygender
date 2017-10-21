setwd("D:/Assembla/identifygender/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")

setwd("D:/Assembla/identifygender/src/data/femalenames")
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply( files, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles <- myfiles[,c("Name","Count")]
  
colnames(myfiles) <- c("givenName","gender")
myfiles$gender <- "female"  
girls.ds <- unique( myfiles[ , 1:2 ] )

setwd("D:/Assembla/identifygender/src/data/malenames")
files = list.files(pattern="*.csv")
myfiles = do.call(rbind, lapply( files, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles <- myfiles[,c("Name","Count")]

colnames(myfiles) <- c("givenName","gender")
myfiles$gender <- "male"  
boys.ds <- unique( myfiles[ , 1:2 ] )


setwd("D:/Assembla/identifygender/src")

firstnames <- rbind(boys.ds,girls.ds)
firstnames$reco.id <- row.names(firstnames)

firstnames$givenName <- as.vector(trimws(firstnames$givenName, which = c("both")))
firstnames$gender <- as.vector(trimws(firstnames$gender, which = c("both")))

#gender(firstnames$Name, method = c("ssa"),countries = c("United States"))
test.results <- gender(firstnames$givenName, method = c("napp"),countries = c("United Kingdom"))

ds.count = 1000
## Sample dataset from the original 200K records
findGender.names <-  firstnames[sample(nrow(firstnames),ds.count),]
#foundNames = findGivenNames(findGender.names$givenName, textPrepare = FALSE)

#foundNames$isMatch <- ifelse(foundNames$gender==findGender.names$Gender,"yes","no")

found.gender <- genderize("findGender.names", genderDB = test.results, progress = FALSE)

found.gender$isMatch <- ifelse(found.gender$text.gender==findGender.names$gender,"yes","no")
