# Copyright Sreekumar Pillai, 2017
# This is the server logic of a Shiny web application. 

setwd("/srv/shiny-server/findgender/fg")

library(shiny)
library(plyr)
library(dplyr)
library(pryr)
library(dtplyr)
library(gender)
library(genderizeR)


# set source directory and identify famele csv files
setwd("/srv/shiny-server/findgender/fg/femalenames")
files = list.files(pattern="*.csv")

# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply( files, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles <- myfiles[,c("Name","Count")]

# name the columns  
colnames(myfiles) <- c("givenName","gender")
myfiles$gender <- "female"  

# take only the unique names
girls.ds <- unique( myfiles[ , 1:2 ] )

#set source directory and identify famele csv files
# repeat the same for male names
setwd("/srv/shiny-server/findgender/fg/malenames")
files = list.files(pattern="*.csv")
myfiles = do.call(rbind, lapply( files, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles <- myfiles[,c("Name","Count")]
colnames(myfiles) <- c("givenName","gender")
myfiles$gender <- "male"  
boys.ds <- unique( myfiles[ , 1:2 ] )

# ----  Test data complete ---

setwd("/srv/shiny-server/findgender/fg")

# combine names from both sources
firstnames <- rbind(boys.ds,girls.ds)
firstnames$reco.id <- row.names(firstnames)

# strip names of blank spaces
firstnames$givenName <- as.vector(trimws(firstnames$givenName, which = c("both")))
firstnames$gender <- as.vector(trimws(firstnames$gender, which = c("both")))

#gender(firstnames$Name, method = c("ssa"),countries = c("United States"))
#genderDB <- gender(firstnames$givenName, method = c("napp"),countries = c("United Kingdom"))
ds.count = 50

## Sample dataset from the original 35K records
findGender.names <-  firstnames[sample(nrow(firstnames),ds.count),]


## Function to return the gender given a firstname

#genderDB <- read.csv(file="sas_names.csv", stringsAsFactors = FALSE)
genderDB <- genderdata::ssa_national

genderDB <- genderDB %>%
  group_by(name) %>%
  summarise(female = sum(female),
            male = sum(male)) %>%
  mutate(proportion_female = round((female / (male + female)), digits = 4),
         proportion_male = round((male / (male + female)), digits = 4), 
         count=(male+female),
         gender = ifelse(proportion_female >0.5,"female","male"),
         probability = ifelse(gender=="female",proportion_female,proportion_male))


genderDB <- genderDB[,c(1,6,7,8)]


#------------------------------
findGender <- function(.name, .genderDB){
  f.gender <- genderize(.name, .genderDB, progress = TRUE)
  f.gender <- f.gender[,-c(1)]
  colnames(f.gender) <- c("name","gender","probability")
  f.gender$message <- "my own db"
  
  if(is.na(f.gender$gender)){
    f.gender = findGivenNames(.name, textPrepare = FALSE)
    f.gender <- f.gender[,-c(4)]
    colnames(f.gender) <- c("name","gender","probability")
    f.gender$message <- "an external API"
  }
  
  if(nrow(f.gender)==0){
    f.gender <- f.gender[,-c(1)]
    colnames(f.gender) <- c("name","gender","probability")
    f.gender$name <- "Not known"
    f.gender$gender <- " not known to me. "
    f.gender$message <- " neither source "
  }
  
 return(f.gender)
}

setwd("/srv/shiny-server/findgender/fg")


shinyServer(function(input, output) {
  output$gender <- renderText({
    # Simply accessing input$goButton here makes this reactive
    # object take a dependency on it. That means when
    # input$goButton changes, this code will re-execute.
    
    input$goButton
    
    # Use isolate() to avoid dependency on input$n
    isolate({
      if(input$n !=""){
        found.gender <- findGender(input$n,genderDB)
        
        output.text <- paste("The name: ",found.gender[[1]],"is a ", found.gender[[2]], ".  The probability is : ", found.gender[[3]], ".",
                             "I used ", found.gender[[4]], " to resolve this !" )
        output.text
      }else{
      
      input$n
      }
    })
   
  })
})
