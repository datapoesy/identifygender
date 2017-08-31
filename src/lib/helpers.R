

## This is a function that will count the number of NAs in a particular column in a dataset
NAhunter<-function(dataset) { 
  find.NA<-function(variable) { 
    if(is.numeric(variable)){ 
      n<-length(variable) 
      mean<-mean(variable, na.rm=T) 
      median<-median(variable, na.rm=T) 
      sd<-sd(variable, na.rm=T) 
      NAs<-is.na(variable) 
      total.NA<-sum(NAs) 
      percent.missing<-total.NA/n 
      descriptives<-data.frame(n,mean,median,sd,total.NA,percent.missing) 
      rownames(descriptives)<-c(" ") 
      Case.Number<-1:n 
      Missing.Values<-ifelse(NAs>0,"Missing Value"," ") 
      missing.value<-data.frame(Case.Number,Missing.Values) 
      missing.values<-missing.value[ which(Missing.Values=='Missing Value'),] 
      list("NUMERIC DATA","DESCRIPTIVES"=t(descriptives)) 
    } else{ 
      n<-length(variable) 
      NAs<-is.na(variable) 
      total.NA<-sum(NAs) 
      percent.missing<-total.NA/n 
      descriptives<-data.frame(n,total.NA,percent.missing) 
      rownames(descriptives)<-c(" ") 
      Case.Number<-1:n 
      Missing.Values<-ifelse(NAs>0,"Missing Value"," ") 
      missing.value<-data.frame(Case.Number,Missing.Values) 
      missing.values<-missing.value[ which(Missing.Values=='Missing   
        Value'),] 
      list("CATEGORICAL DATA","DESCRIPTIVES"=t(descriptives)) 
    } 
  } 
  dataset<-data.frame(dataset) 
  options(scipen=100) 
  options(digits=2) 
  lapply(dataset,find.NA) 
}  



## This function converts required variables to factor variables for analysis

convFactors  <-  function(){
  
  isbsg.trim$ind.sector <-  factor(isbsg.trim$ind.sector)
  isbsg.trim$appl.group <-   factor(isbsg.trim$appl.group)
  isbsg.trim$dev.type  <-  factor(isbsg.trim$dev.type)
  isbsg.trim$count.appr <- factor(isbsg.trim$count.appr)
  
  isbsg.trim$dev.methods <-  revalue(isbsg.trim$dev.methods, c("Agile Development;"="Scrum","Agile Development;Extreme Programming (XP);Iterative;Rapid Application Development (RAD);Scrum;Timeboxing;Unified Process;IMES OOM;" ="XP","Agile Development;Iterative;" ="Iterative","Agile Development;Joint Application Development (JAD);Multifunctional Teams;"="JAD","Agile Development;Multifunctional Teams;Scrum;"="Scrum","Agile Development;Personal Software Process (PSP);Unified Process;"="PSP","Agile Development;Scrum;"="Scrum","Agile Development;Unified Process;"="AUP","Extreme Programming (XP);"="XP","Incremental;"="Incrmental","Interactive;"="Iterative","IT Unified Process (ITUP);" ="ITUP","Iterative;"="Iterative","Iterative;Unified Process;"="Iterative","Joint Application Development (JAD);"="JAD","Joint Application Development (JAD);Multifunctional Teams;" ="JAD","Joint Application Development (JAD);Multifunctional Teams;Rapid Application Development (RAD);"="RAD","Joint Application Development (JAD);Multifunctional Teams;Rapid Application Development (RAD);Timeboxing;"="RAD","Joint Application Development (JAD);Multifunctional Teams;Timeboxing;"="RAD","Joint Application Development (JAD);Rapid Application Development (RAD);"="RAD","Joint Application Development (JAD);Rapid Application Development (RAD);Timeboxing;"="RAD","Joint Application Development (JAD);Timeboxing;"="RAD","Lean;"="Scrum","Multifunctional Teams;"="Waterfall","Multifunctional Teams;Rapid Application Development (RAD);"="RAD","Multifunctional Teams;Rapid Application Development (RAD);Timeboxing;"="RAD","Multifunctional Teams;Timeboxing;"="RAD","Multifunctional Teams;Unified Process;" ="AUP","Multifunctional Teams;Waterfall (includes Linear Processing);"="Waterfall","OCE;"="OCE","Personal Software Process (PSP);"="PSP","Personal Software Process (PSP);Unified Process;"="PSP","Rapid Application Development (RAD);"="RAD","Rapid Application Development (RAD);Timeboxing;"="RAD","Scrum;"="Scrum","Spiral;"="Spiral","Timeboxing;"="RAD","Unified Process;"  ="AUP","Waterfall (incl Linear Processing & SSADM);"="Waterfall","Waterfall (includes Linear Processing);"="Waterfall"))
  
  
  isbsg.trim$fp.stand <- revalue(isbsg.trim$fp.stand,c("Addendum to existing standards"="Others","Auto-generated"="Auto-generated","CFFP v2"="CFFP","Cfsu"="CFFP","Charismatek Manual (IFPUG 2)"="IFPUG-FP","COSMIC"="COSMIC-FP","COSMIC-FFP"="COSMIC-FP","COSMIC-FFP 2.2"="COSMIC-FP","COSMIC-FFP 2002"="COSMIC-FP","COSMIC 1.1"="COSMIC-FP","COSMIC 2"="COSMIC-FP","COSMIC 2.0"="COSMIC-FP","COSMIC 2.1"="COSMIC-FP","COSMIC 2.2"="COSMIC-FP","COSMIC 3"="COSMIC-FP","COSMIC 3.0"="COSMIC-FP","COSMIC 3.0.1"="COSMIC-FP","COSMIC 3.01"="COSMIC-FP","COSMIC 4.0"="COSMIC-FP","COSMIC FPs"="COSMIC-FP","Counter experience with some input from Phil Hain"="Others","CPM4.0;Addendum to existing standards;"="Others","Dialog Manual (based on IFPUG 2)"="IFPUG-FP","Dreger Book"="Others","Feature Points"="Feature Points","FFP"="Feature Points","FFP 1.0"="Feature Points","FiSMA"="FISMA","FP"="IFPUG-FP","IFPUG"="IFPUG-FP","IFPUG 1"="IFPUG-FP","IFPUG 2"="IFPUG-FP","IFPUG 2;IFPUG 3;IFPUG 4;"="IFPUG-FP","IFPUG 2006 B"="IFPUG-FP","IFPUG 3"="IFPUG-FP","IFPUG 3.4"="IFPUG-FP","IFPUG 3;Addendum to existing standards;"="IFPUG-FP","IFPUG 3;IFPUG 4;"="IFPUG-FP","IFPUG 3;In-house;"="IFPUG-FP","IFPUG 3;In-house;Addendum to existing standards;"="IFPUG-FP","IFPUG 4"="IFPUG-FP","IFPUG 4 & Other - Unspecified"="IFPUG-FP","IFPUG 4 / Automated"="IFPUG-FP","IFPUG 4.0"="IFPUG-FP","IFPUG 4.0 & Addendum to existing standards"="IFPUG-FP","IFPUG 4.1"="IFPUG-FP","IFPUG 4.1;customisation of rules to provide clarifications;"="IFPUG-FP","IFPUG 4.2"="IFPUG-FP","IFPUG 4.2.1"="IFPUG-FP","IFPUG 4.3"="IFPUG-FP","IFPUG 4;Addendum to existing standards;"="IFPUG-FP","IFPUG 4;In-house;"="IFPUG-FP","IFPUG FP"="IFPUG-FP","IFPUG local"="IFPUG-FP","IFPUG Unspecified"="IFPUG-FP","IFPUG/Hans Vonk"="IFPUG-FP","IFPUG/NESMA"="IFPUG-FP","In-house"="In-house","ISO/IEC 19761:2002"="COSMIC-FP","ISO/IEC FDIS 19761:2002"="COSMIC-FP","ISO/IEC FDIS 19761:2002(E)"="COSMIC-FP","Mark II"="Mark2-FP","Mk II"="Mark2-FP","NEFPUG"="Nesma-FP","NESMA"="Nesma-FP","NESMA 2.0"="Nesma-FP","NESMA 2.2"="Nesma-FP","Nesma Onderh."="Nesma-FP","Nesma onderhoud"="Nesma-FP","not given"="Not-given","not given said to be Mark II"="Mark2-FP","Original Albrecht's Method"="IFPUG-FP","Other"="Others","SLOC;OO count;"="Others","SPR backfile"="Others","Symons Mark II"="Mark2-FP","Symons Mark II;UFPUG Guidelines;"="Mark2-FP","UFPUG mk2"="Mark2-FP"))
  
  isbsg.trim$fp.stand <- factor(isbsg.trim$fp.stand)
  
  isbsg.trim$team.size.gp <- revalue(isbsg.trim$team.size.gp,c("101+"="Above 101"))
  
  isbsg.trim$team.size.gp <-factor(isbsg.trim$team.size.gp,c("1","2","3-4","5-8","9-14","15-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100","Above 101"))
  
}


## Replaces NAs with the given string  ---------------------------------
NAreplace <- function(x, replString){  
    {
      x[is.na(x)] <- replString
      replace(x, is.na(x), replString) 
      return(x)
    }
}

# Create a confusion matrix from the given outcomes, whose rows correspond
# to the actual and the columns to the predicated classes.
createConfusionMatrix <- function(act, pred) {
  # You've mentioned that neither actual nor predicted may give a complete
  # picture of the available classes, hence:
  numClasses <- max(act, pred)
  # Sort predicted and actual as it simplifies what's next. You can make this
  # faster by storing `order(act)` in a temporary variable.
  pred <- pred[order(act)]
  act  <- act[order(act)]
  sapply(split(pred, act), tabulate, nbins=numClasses)
}


# Function that returns Root Mean Squared Error
rmse <- function(actual, predicted)
   
  
{
  # Calculate error
  error <- actual - predicted
  
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error  ---------------------------------
mae <- function(actual, predicted)
{
  
  # Calculate error
  error <- actual - predicted
  mean(abs(error))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Function to Remove Outliers ----------------------------------------------------------
rem_outliers <- function(x, na.rm = TRUE,...){
  qnt <- quantile (x, probs=c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5* IQR (x, na.rm=na.rm)
  y <-x
  y[x<(qnt[1] - H)] <- mean(x)
  y[x > (qnt[2] + H)] <- mean(x)
  y
}


helper.function <- function()
{
  return(1)
}


## Find outliers
findOutlier <- function(data, cutoff = 3) {
  ## Calculate the sd
  sds <- apply(data, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  result <- mapply(function(d, s) {
    which(d > cutoff * s)
  }, data, sds)
  result
}

##Remove outliers by marking them as NA
removeOutlier <- function(data, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <- NA
    return(res)
  }, data, outliers)
  return(as.data.frame(result))
}


## Function to remove outliers for numerical columns only ------------------------------
remove_ol <- function (data){
  for (i in names(data)) {
    if(is.numeric(data[[i]])){
      data[[i]] <- rem_outliers(data[[i]])
    }
  }
  data
}

transform_lognormal <- function(data){
  for (i in names(data)) {
    if(is.numeric(data[[i]])){
      data[[i]] <- log(data[[i]])
    }
  }
  data
}

## Function to make profile plot
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose ’numvariables’ random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else { points(vectori, col=colouri,type="l") }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}


## Function to show most highly correlated data ---------------------------------
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}


## function to fill missing VAF ------------- ---------------------------------
fillmissingVAF = function(x){
  
  for(i in 1:nrow(x)){
    
    if(is.na(x[i,7])){
      if(!is.na(x[i,5])&&!is.na(x[i,6])){
        #value.af = adj.funct.size/funct.size
        x[i,7] <- x[i,5]/x[i,6]
      }
    }
    
    if(is.na(x[i,6])){
      if(!is.na(x[i,5])&&!is.na(x[i,7])){
        # funct.size = adj.funct.size / value.af
        x[i,6] <- x[i,5]/x[i,7]
      }
      
    }
    
  }
  x
  
}

#Function to remove rows based on NA values in specific columns---------------------------------

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}



#Function to retrieve number of rows based on a percentage -------------------------------------
#input dataframe and percentage value in real number

giveRandomRows = function(df,p){
  return(df[sample(nrow(df),p*nrow(df)/100),])
}


#Function to change column values with the value of the first row-------------------------------
#input dataframe 
#input column name
#input percentage of the dataset to be changed in % real number


changeColumnValue = function(df, c.name,p){
  
  temp <- (df[sample(nrow(df),p*nrow(df)/100),])
  fn <- temp[1,c.name]
  temp[,c.name]<- fn
  temp
  
}


#Function to change column values with the value of the first row-------------------------------
#input dataframe 
#input column name
#input percentage of the dataset to be changed in % real number
removeColumnValue = function(df,col.num,p){
  
  df[sample(nrow(df), nrow(df)*p/100), ][col.num] <- ''
}

#Function to change column values with a given string -------------------------------
#input dataframe 
#input column name
#input new value as a string
#input percentage of the dataset to be changed in % real number

updateColumnValue = function(df, c.name,newValue,p){
  
  temp <- (df[sample(nrow(df),p*nrow(df)/100),])
  temp[c.name]<- newValue
  temp
}


#Function to change address values with a given string -------------------------------
#input dataframe 
#input column name
#input new value as a string
#input percentage of the dataset to be changed in % real number

updateAddress = function(df, p){
  
  temp <- (df[sample(nrow(df),p*nrow(df)/100),]) # Percentage of rows
  result <- data.frame(matrix(0, ncol = ncol(df), nrow = 0))
  colnames(result) <- colnames(df)
  
  for(i in 1:nrow(temp)){
    firstRow =  temp[i,]
    firstRow$addr.1 <- as.character(temp[1,c("addr.1")]) # Take a random name from the forename column
    firstRow$addr.2 <- as.character(temp[1,c("addr.2")])
    firstRow$cityname <- as.character(temp[1,c("cityname")]) 
    firstRow$postcode <- as.character(temp[1,c("postcode")])
    firstRow$county <- as.character(temp[1,c("county")])
    firstRow$forename <- df$forename[sample(nrow(df),1)] # Take a random name from the forename column
    firstRow$Surname <- df$surname[sample(nrow(df),1)] # Take a random name from the forename column
    firstRow$cust.id  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$gender <- if(firstRow$gender=="male") "female" else "male"
    firstRow$record.id <- paste(temp$record.id[i],"Ad")
    result[i,]<- firstRow
  }
  result

}


#Function to create twins living in the same address with the same last name

updateTwins = function(df, p){
  
  temp <- (df[sample(nrow(df),p*nrow(df)/100),]) # Percentage of rows
  result <- data.frame(matrix(0, ncol = ncol(df), nrow = 0))
  colnames(result) <- colnames(df)
  
  for(i in 1:nrow(temp)){
    firstRow =  temp[i,]
    firstRow$forename <- df$forename[sample(nrow(df),1)] # Take a random name from the forename column
    firstRow$cust.id  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$record.id <- paste(temp$record.id[i],"T")
    result[i,]<- firstRow
  }
  result
}



#Function to create twins living in the same address with the same last name

updateCouples = function(df, p){
  
  temp <- (df[sample(nrow(df),p*nrow(df)/100),]) # Percentage of rows
  result <- data.frame(matrix(0, ncol = ncol(df), nrow = 0))
  colnames(result) <- colnames(df)
  
  for(i in 1:nrow(temp)){
    firstRow =  temp[i,]
    firstRow$forename <- df$forename[sample(nrow(df),1)] # Take a random name from the forename column
    firstRow$cust.id  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$gender <- if(firstRow$gender=="male") "female" else "male"
    firstRow$b.month <- df$b.month[sample(nrow(df),1)] # Take a random month of birth
    firstRow$b.day <- df$b.day[sample(nrow(df),1)] # Take a random month of birth
    firstRow$b.year <- df$b.year[sample(nrow(df),1)] # Take a random month of birth
    firstRow$record.id <- paste(temp$record.id[i],"C")
    result[i,]<- firstRow
  }
  result
}



#Function to alter names to suggested ones
#input name to be changed
#input suggested format

alterFirstName = function(df,old, new){
  
  temp <- df[like(df$forename,old),]
  #temp <- df[grep(df$forename, old), ]
  temp <- (temp[(nrow(temp)/2),]) # Percentage of rows
  result <- data.frame(matrix(0, ncol = ncol(df), nrow = 0))
  colnames(result) <- colnames(df)
  
  for(i in 1:nrow(temp)){
    firstRow =  temp[i,]
    firstRow$cust.id  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$record.id <- paste(temp$record.id[i],"NFN")
    firstRow$forename <- gsub(old, new, firstRow$forename)
    result[i,]<- firstRow
  }
  result
}


#Function to alter names to suggested ones
#input name to be changed
#input suggested format

alterLastName = function(df,old, new){
  
  temp <- df[like(df$surname,old),]
  #temp <- df[grep(df$surname, old), ]
  temp <- (temp[(nrow(temp)/2),]) # Percentage of rows
  result <- data.frame(matrix(0, ncol = ncol(df), nrow = 0))
  colnames(result) <- colnames(df)
  
  for(i in 1:nrow(temp)){
    firstRow =  temp[i,]
    firstRow$cust.id  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$record.id <- paste(temp$record.id[i],"NFN")
    firstRow$surname <- gsub(old, new, firstRow$surname)
    result[i,]<- firstRow
  }
  result
}


#Function to alter addr.1 field
#input name to be changed
#input suggested format

alterAddr1 = function(df,old, new){
  
  temp <- df[like(df$addr.1,old),]
 
  temp <- (temp[(nrow(temp)/2),]) # Percentage of rows
  result <- data.frame(matrix(0, ncol = ncol(df), nrow = 0))
  colnames(result) <- colnames(df)
  
  for(i in 1:nrow(temp)){
    firstRow =  temp[i,]
    firstRow$cust.id  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$record.id <- paste(temp$record.id[i],"NFN")
    firstRow$addr.1 <- gsub(old, new, firstRow$addr.1)
    result[i,]<- firstRow
  }
  result
}


