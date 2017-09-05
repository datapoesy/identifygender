setwd("D:/Assembla/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")

#Reading customer data
cust.rcds = read_csv(file="data/customers-100k.csv")


#Column filters for the dataset
col.filter <- c("userdetail_id","customer_id","forename","surname",
                "gender","birth_date","email","mobilenumber",
                "homenumber","address1","address2","cityname","postcode",
                "county","nis")

base.ds <- cust.rcds[col.filter]

#Renaming columns in the dataset
col.names <-  c("record.id","some.id","firstname","lastname",
                  "gender","birth.date","email","m.nmbr",
                  "h.nmbr","addr.1","addr.2","cityname","postcode",
                  "county","nis")
colnames(base.ds) <- col.names


#date formatting of the issue log
base.ds$birth.date <- as.POSIXct(base.ds$birth.date, format='%d/%m/%Y %H:%M:%S')
base.ds$birth.date <- as.Date(base.ds$birth.date, format='%d/%m/%Y')

#Column Descriptions  ----------------------------------------------------------------------------
desc.base.ds <- as.data.frame(t(base.ds[1,]))
desc.base.ds$variables <- rownames(desc.base.ds)
rownames(desc.base.ds) <- NULL
colnames(desc.base.ds)<- c("Description","Variable")
desc.base.ds$Description <- c("Unique ID in the dataset","Some Org ID","First Name","Last Name","Gender",
                              "Date of Birth","e-mail ID","Mobile Number",
                              "Home Number","Home Nummber","Street Address",
                              "Name of the City","Post Code","Country","National Insurance Number")

desc.base.ds$No. <- c(1:nrow(desc.base.ds))
desc.base.ds <-desc.base.ds[,c(3,2,1)]
#------------------------------------------------------------------------------------------------

base.ds$b.year <- format(base.ds$birth.date, format='%y')
base.ds$b.month <- format(base.ds$birth.date, format='%m')
base.ds$b.day <- format(base.ds$birth.date, format='%d')

#dropping the DoB column as it is not required
#base.ds <- base.ds[,-6]


#Convert cont.id and some.id to characters as required for string comparison
base.ds$some.id <- as.character(base.ds$some.id)
base.ds$m.nmbr <- as.character(base.ds$m.nmbr)
base.ds$h.nmbr <- as.character(base.ds$h.nmbr)
base.ds$birth.date <- as.character(base.ds$birth.date)
base.ds$postcode <- as.character(base.ds$postcode)

remove(cust.rcds)

#replicate the dataset for processing

results <- as.data.frame(matrix(0,ncol=19,nrow=6))

TS.results <- as.data.frame(matrix(0,ncol=19,nrow=6))


colnames(TS.results) <- c("No.","Total Records","Count Changes",
                          "Non-Duplicates","Record Pairs","Quality %",
                          "Us-N","Us-P","Us-L",
                          "Ma-N","Ma-P","Ma-L",
                          "St-N","St-P","St-L",
                          "Ml-N","Ml-P","Ml-L",
                          "% Records Ident.")

colnames(results) <- c("No.","Total Records","Count Changes",
                          "Non-Duplicates","Record Pairs","Quality %",
                          "Us-N","Us-P","Us-L",
                          "Ma-N","Ma-P","Ma-L",
                          "St-N","St-P","St-L",
                          "Ml-N","Ml-P","Ml-L",
                          "% Records Ident.")

# Sets the legend table for processing==========================
#========================================================  
results.desc <- as.data.frame(matrix("",ncol=3, nrow=19))
colnames(results.desc) <- c("No.","Item","Description")
results.desc[,1] <- c(1: nrow(results.desc))
results.desc[,2] <- colnames(TS.results) 
results.desc[,3] <- c("Record Order","Total Records in the Iteration","Number of Records changed",
                      "Records created to represent new entities", "Pairs identified by the comaprator engine",
                      "Quality is the percentage of fake records to the total records.  Typographical errors are not considered in this 
                      representation of quality.",
                      "Unsupervised Classification - Non Links","Potential Links","Confirmed Links",
                      "Manual Classification - Non Links","Potential Links","Confirmed Links",
                      "Supervised Classification. Training Set - Non Links","Potential Links","Confirmed Links",
                      "Supervised Classification using ML trained classifier - Non Links","Potential Links","Confirmed Links",
                      "Accuracy is no of machine identified links/ number of records from direct observation.")





