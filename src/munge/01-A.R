setwd("D:/Assembla/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")

#Reading customer data
cust.rcds = read_csv(file="data/customers-2k.csv")


#Column filters for the dataset
col.filter <- c("userdetail_id","customer_id","forename","surname",
                "gender","birth_date","email","mobilenumber",
                "homenumber","address1","address2","cityname","postcode",
                "county","nis")

base.ds <- cust.rcds[col.filter]

#Renaming columns in the dataset
col.names <-  c("record.id","some.id","forename","surname",
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
desc.base.ds <- desc.base.ds[,c(2,1)]
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




