setwd("D:/Assembla/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")

#Reading customer data
cust.rcds = read_csv(file="data/customers-200k.csv")

colnames(cust.rcds)
#Column filters for the dataset
col.filter <- c("userid","someid","firstname","lastname",
                "gender","birthdate","email","mobilenumber",
                "homenumber","address1","address2","cityname","postcode",
                "county","nis")

base.ds <- cust.rcds[col.filter]

#Renaming columns in the dataset
col.names <-  c("recoid","someid","firstname","lastname",
                  "gender","birthdate","email","mnumber",
                  "hnumber","addr1","addr2","cityname","postcode",
                  "county","nis")
colnames(base.ds) <- col.names


#date formatting of the issue log
base.ds$birthdate <- as.POSIXct(base.ds$birthdate, format='%d/%m/%Y %H:%M:%S')
base.ds$birthdate <- as.Date(base.ds$birthdate, format='%d/%m/%Y')

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
write.csv(desc.base.ds, file="desc.base.ds.csv")
#------------------------------------------------------------------------------------------------

base.ds$b.year <- format(base.ds$birthdate, format='%y')
base.ds$b.month <- format(base.ds$birthdate, format='%m')
base.ds$b.day <- format(base.ds$birthdate, format='%d')

#dropping the DoB column as it is not required
#base.ds <- base.ds[,-6]


#Convert cont.id and someid to characters as required for string comparison
base.ds$someid <- as.character(base.ds$someid)
base.ds$mnumber <- as.character(base.ds$mnumber)
base.ds$hnumber <- as.character(base.ds$hnumber)
base.ds$birthdate <- as.character(base.ds$birthdate)
base.ds$email <- as.character(base.ds$email)
base.ds$addr1 <- as.character(base.ds$addr1)
base.ds$addr2 <- as.character(base.ds$addr2)
base.ds$county <- as.character(base.ds$county)
base.ds$postcode <- as.character(base.ds$postcode)
base.ds$nis <- as.character(base.ds$nis)



write.csv(base.ds, file="data/base.ds.csv")

remove(cust.rcds)





