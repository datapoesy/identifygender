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

remove(changes.ds,non.dupli.ds,temp.ds,ds.00)

QS.results.40k <- as.data.frame(matrix(0,ncol=19,nrow=6))
colnames(QS.results.40k) <- c("No.","Total Records","Count Changes",
"Non-Duplicates","Record Pairs","Quality %",
"Us-N","Us-P","Us-L",
"Ma-N","Ma-P","Ma-L",
"St-N","St-P","St-L",
"Ml-N","Ml-P","Ml-L",
"% Records Ident.")
#-----------------------------------------------------------------
pb = txtProgressBar(min = 0, max = 6, initial = 0)
perc.value <- c(1.5, 3, 4.8, 6.5, 8.2, 10)
i = 6
set.seed(1234)
ds.00 <-  base.ds[sample(nrow(base.ds),40000),]
remove(base.ds)
setTxtProgressBar(pb,i)
p <- perc.value[i]

changes.ds <- data.frame(matrix(0, ncol = ncol(ds.00), nrow = 0))
non.dupli.ds <- data.frame(matrix(0, ncol = ncol(ds.00), nrow = 0))
colnames(changes.ds) <- colnames(ds.00)
changes.ds$some.id <- as.character(changes.ds$some.id)
changes.ds$m.nmbr <- as.character(changes.ds$m.nmbr)
changes.ds$h.nmbr <- as.character(changes.ds$h.nmbr)
changes.ds$birth.date <- as.character(changes.ds$birth.date)
changes.ds$postcode <- as.character(changes.ds$postcode)
#simulate missing email id
ds.00$email[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
#simulate missing home phone numbers
ds.00$h.nmbr[sample(nrow(ds.00), nrow(ds.00) * 40 / 100)] <- ''
###Missing Mobile phone numbers
ds.00$m.nmbr[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
#Simulate missing postcodes
ds.00$postcode[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
###Simulate Missing Address 1 from Address
ds.00$addr.1[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
###simulate Missing Address 2 from Address
ds.00$addr.2[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
###Simulate Missing County Name from Address
ds.00$county[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
###Simulate Missing City Name from Address
ds.00$cityname[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
###Duplicate records with same  address
temp.ds <- updateAddress(ds.00, p)
changes.ds <- rbind(changes.ds, temp.ds)
###Duplicate records with same  FirstName
temp.ds <- createRowWithSameFN(ds.00, p)
changes.ds <- rbind(changes.ds, temp.ds)
###Duplicate records with same  LastName
temp.ds <- createRowWithSameLN(ds.00, p)
changes.ds <- rbind(changes.ds, temp.ds)
###Duplicate Records for identical Twins living in the same address
pt <- 0.2* i
remove(temp.ds)
temp.ds <- updateTwins(ds.00, pt)
changes.ds <- rbind(changes.ds, temp.ds)
non.dupli.ds <- rbind(non.dupli.ds, temp.ds)
###Duplicate Records for Married couples living in the same address
# % of couples in the loan availing population = 50%
# % of couples living in the same address = 80%
# Combined percentage of the above = 40%
pc <- 5* i
remove(temp.ds)
temp.ds <- updateCouples(ds.00, pc)
changes.ds <- rbind(changes.ds, temp.ds)
non.dupli.ds <- rbind(non.dupli.ds, temp.ds)
###Create Typographical errors name to simulate spelling mistakes
ds.00 <- makeTypos(ds.00, "John", "Jon", pc)
ds.00 <- makeTypos(ds.00, "Thompson", "Thomson", pc)
ds.00 <- makeTypos(ds.00, "tt", "t", pc)
ds.00 <- makeTypos(ds.00, "ss", "s", pc)
#=====================================================================
changes.ds <- rbind(changes.ds, temp.ds)
ds.00 <- rbind(ds.00, changes.ds)
data.quality <-
round((nrow(ds.00) - nrow(changes.ds) - nrow(non.dupli.ds)) / nrow(ds.00), 2)
#Creating comparison vectors
rpairs.00 <-
compare.dedup(
ds.00,
blockfld = list(4),
phonetic = (3:4),
exclude = c("record.id", "birth.date")
)

#============  pre processing complete ===========================

rpairs.01 <- rpairs.00 # For unsupervised classification
#Determine Weights
rpairs.00 <- epiWeights(rpairs.00)
#========================================================================
#Manual Classification using thresholds from the previous result
manual.class.ds <- epiClassify(rpairs.00, 0.7, 0.40)
#========================================================================
#Unsupervised Classification
unsup.class.ds <- classifyUnsup(rpairs.01, method = "kmeans")
#========================================================================
###Supervised clasification
##minTrain.00 <- getMinimalTrain(rpairs.00)
#minTrain.00 <- editMatch(minTrain.00)
# load("D:/Assembla/deduplication/src/minTrain.00-10KTrainingRecords.Rdata")
model.00 <- trainSupv(minTrain.00, method = "rpart")
min.train.ds <- classifySupv(model.00, newdata = rpairs.00)
#summary(result)
#================================================================
# #Training a classifier with ML
# l = splitData(
#   rpairs.01,
#   prop = 1,
#   keep.mprop = FALSE,
#   use.pred = FALSE
# )
# us.model = trainSupv(l$train, method = "rpart", minsplit = 5)
# save(us.model, file = str_c("us.model", i))
# ml.class.ds = classifySupv(model = us.model, newdata = l$valid)
# summary(ml.class.ds)
#updating Results Dataframe
QS.results.40k[i, 1] <- i
QS.results.40k[i, 2] <- nrow(ds.00)
QS.results.40k[i, 3] <- nrow(changes.ds)
QS.results.40k[i, 4] <- nrow(non.dupli.ds)
QS.results.40k[i, 5] <- nrow(unsup.class.ds$pairs)
QS.results.40k[i, 6] <- data.quality
QS.results.40k[i, 7] <- summary(unsup.class.ds$prediction)[1]
QS.results.40k[i, 8] <- summary(unsup.class.ds$prediction)[2]
QS.results.40k[i, 9] <- summary(unsup.class.ds$prediction)[3]
QS.results.40k[i, 10] <- summary(manual.class.ds$prediction)[1]
QS.results.40k[i, 11]  <- summary(manual.class.ds$prediction)[2]
QS.results.40k[i, 12]  <- summary(manual.class.ds$prediction)[3]
QS.results.40k[i, 13]  <- summary(min.train.ds$prediction)[1]
QS.results.40k[i, 14]  <- summary(min.train.ds$prediction)[2]
QS.results.40k[i, 15]  <- summary(min.train.ds$prediction)[3]
QS.results.40k[i, 16]  <- ""
QS.results.40k[i, 17]  <- ""
QS.results.40k[i, 18]  <- ""
QS.results.40k[i, 19]  <-
round((QS.results.40k[i, 15] / QS.results.40k[i, 4]) * 100, 2) # (1- (nrow(changes.ds)-nrow(non.dupli.ds)-summary(min.train.ds$prediction)[2])/nrow(changes.ds)-nrow(non.dupli.ds))*100
QS.results.40k






#201 links detected
#225 possible links detected
#6526 non-links detected
#=========================================================================
