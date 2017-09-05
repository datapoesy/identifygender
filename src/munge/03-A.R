setwd("D:/Assembla/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("munge/01-A.R")

    i = 6
    set.seed(1234)
    
    
    ds.11 <-  base.ds[sample(nrow(base.ds),2000),]
    remove(base.ds)
    setTxtProgressBar(pb,i)
    
    perc.value <- c(1.5, 3, 4.8, 6.5, 8.2, 10)
    p <- perc.value[i]
    pb = txtProgressBar(min = 0, max = 6, initial = 0)
    
    
    
    
    changes.ds <-
      data.frame(matrix(0, ncol = ncol(ds.11), nrow = 0))
    non.dupli.ds <-
      data.frame(matrix(0, ncol = ncol(ds.11), nrow = 0))
    colnames(changes.ds) <- colnames(ds.11)
    changes.ds$some.id <- as.character(changes.ds$some.id)
    changes.ds$m.nmbr <- as.character(changes.ds$m.nmbr)
    changes.ds$h.nmbr <- as.character(changes.ds$h.nmbr)
    changes.ds$birth.date <- as.character(changes.ds$birth.date)
    changes.ds$postcode <- as.character(changes.ds$postcode)
    
    
    #simulate missing email id
    ds.11$email[sample(nrow(ds.11), nrow(ds.11) * p / 100)] <- ''
    #simulate missing home phone numbers
    ds.11$h.nmbr[sample(nrow(ds.11), nrow(ds.11) * 40 / 100)] <- ''
    ###Missing Mobile phone numbers
    ds.11$m.nmbr[sample(nrow(ds.11), nrow(ds.11) * p / 100)] <- ''
    #Simulate missing postcodes
    ds.11$postcode[sample(nrow(ds.11), nrow(ds.11) * p / 100)] <- ''
    ###Simulate Missing Address 1 from Address
    ds.11$addr.1[sample(nrow(ds.11), nrow(ds.11) * p / 100)] <- ''
    ###simulate Missing Address 2 from Address
    ds.11$addr.2[sample(nrow(ds.11), nrow(ds.11) * p / 100)] <- ''
    ###Simulate Missing County Name from Address
    ds.11$county[sample(nrow(ds.11), nrow(ds.11) * p / 100)] <- ''
    ###Simulate Missing City Name from Address
    ds.11$cityname[sample(nrow(ds.11), nrow(ds.11) * p / 100)] <- ''
    
    ###Duplicate records with same  address
    temp.ds <- updateAddress(ds.11, p)
    changes.ds <- rbind(changes.ds, temp.ds)
    ###Duplicate records with same  FirstName
    temp.ds <- createRowWithSameFN(ds.11, p)
    changes.ds <- rbind(changes.ds, temp.ds)
    ###Duplicate records with same  LastName
    temp.ds <- createRowWithSameLN(ds.11, p)
    changes.ds <- rbind(changes.ds, temp.ds)
    ###Duplicate Records for identical Twins living in the same address
    pt <- 0.2* i
    remove(temp.ds)
    temp.ds <- updateTwins(ds.11, pt)
    changes.ds <- rbind(changes.ds, temp.ds)
    non.dupli.ds <- rbind(non.dupli.ds, temp.ds)
    ###Duplicate Records for Married couples living in the same address
    # % of couples in the loan availing population = 50%
    # % of couples living in the same address = 80%
    # Combined percentage of the above = 40%
    pc <- 5* i
    remove(temp.ds)
    temp.ds <- updateCouples(ds.11, pc)
    changes.ds <- rbind(changes.ds, temp.ds)
    non.dupli.ds <- rbind(non.dupli.ds, temp.ds)
    
    ###Create Typographical errors name to simulate spelling mistakes
    ds.11 <- makeTypos(ds.11, "John", "Jon", pc)
    ds.11 <- makeTypos(ds.11, "Thompson", "Thomson", pc)
    ds.11 <- makeTypos(ds.11, "tt", "t", pc)
    ds.11 <- makeTypos(ds.11, "ss", "s", pc)
    
    #=====================================================================
    
    changes.ds <- rbind(changes.ds, temp.ds)
    ds.11 <- rbind(ds.11, changes.ds)
    
    
    
    data.quality <-
      round((nrow(ds.11) - nrow(changes.ds) - nrow(non.dupli.ds)) / nrow(ds.11), 2)
    
    
    #Creating comparison vectors
    rpairs.11 <-  compare.dedup(ds.11, blockfld = list(4), phonetic = (3:4), exclude = c("record.id", "birth.date"))
    
    
    rpairs.11 <- epiWeights(rpairs.11)
    
    minTrain.11 <- genSamples(rpairs.11,num.non = 400,des.mprop = 0.2)

    model.00 <- trainSupv(minTrain.11$valid, method = "bagging",minsplit=5,use.pred = TRUE)
               
    min.train.ds <- classifySupv(model.00, newdata = rpairs.11)
    #summary(result)
    
    #================================================================
    # #Training a classifier with ML
    # l = splitData(
    #   rpairs.22,
    #   prop = 1,
    #   keep.mprop = FALSE,
    #   use.pred = FALSE
    # )
    # us.model = trainSupv(l$train, method = "rpart", minsplit = 5)
    # save(us.model, file = str_c("us.model", i))
    # ml.class.ds = classifySupv(model = us.model, newdata = l$valid)
    # summary(ml.class.ds)
    
    
    #updating Results Dataframe
    QS.results.30k[i, 1] <- i
    QS.results.30k[i, 2] <- nrow(ds.11)
    QS.results.30k[i, 3] <- nrow(changes.ds)
    QS.results.30k[i, 4] <- nrow(non.dupli.ds)
    QS.results.30k[i, 5] <- nrow(unsup.class.ds$pairs)
    QS.results.30k[i, 6] <- data.quality
    QS.results.30k[i, 7] <- summary(unsup.class.ds$prediction)[1]
    QS.results.30k[i, 8] <- summary(unsup.class.ds$prediction)[2]
    QS.results.30k[i, 9] <- summary(unsup.class.ds$prediction)[3]
    
    QS.results.30k[i, 10] <- summary(manual.class.ds$prediction)[1]
    QS.results.30k[i, 11]  <- summary(manual.class.ds$prediction)[2]
    QS.results.30k[i, 12]  <- summary(manual.class.ds$prediction)[3]
    
    QS.results.30k[i, 13]  <- summary(min.train.ds$prediction)[1]
    QS.results.30k[i, 14]  <- summary(min.train.ds$prediction)[2]
    QS.results.30k[i, 15]  <- summary(min.train.ds$prediction)[3]
    
    QS.results.30k[i, 16]  <- ""
    QS.results.30k[i, 17]  <- ""
    QS.results.30k[i, 18]  <- ""
    
    QS.results.30k[i, 19]  <-
      round((QS.results.30k[i, 15] / QS.results.30k[i, 4]) * 100, 2) # (1- (nrow(changes.ds)-nrow(non.dupli.ds)-summary(min.train.ds$prediction)[2])/nrow(changes.ds)-nrow(non.dupli.ds))*100
    
   
    
  
  QS.results.30k
  




#201 links detected
#225 possible links detected
#6526 non-links detected
#=========================================================================
