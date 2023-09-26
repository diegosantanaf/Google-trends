rm(list  = ls())
install.packages("gtrendsR")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("curl")
install.packages("readxl")
install.packages("writexl")


library(writexl)
library(gtrendsR)
library(tidyverse)
library(lubridate)
library(curl)
library(readxl)

getwd()

#List of items that you want to download, 
#List of items that you want to download, 
#the first one is always going to be used as a benchmark index
list.items <- read_excel("List of items.xlsx")

list.of.items <- list.items$all

compiled.data.items.w <- tibble() #Create dataframe to save the data in
segment.items <- c() #Empty vector in which we are going to separate the items for download
i <- 1 #for testing
z <- 2 #Counting variable, so the code can keep track in which step of the counting process we are in
list.of.items
for(i in z:(length(list.of.items))){
  
  segment.items <- c(list.of.items[1]) #Always using the first item as a benchmark
  z <- i+1
  #This variable(z) is used to keep check in which part of the process we left off
  #In the case Google stops the downloading process
  segment.items <- c(segment.items, list.of.items[i])
  #The segmented list of items (one benchamark word + 1 word)
  
  
  #Downloading the data
  get.item <- gtrends(
    keyword = segment.items,
    geo = "VE",
    time = "2012-10-01 2022-08-08",
    gprop = c("web", "news", "images", "froogle", "youtube"),
    category = 0,
    hl = "en-US",
    compared_breakdown = FALSE,
    low_search_volume = FALSE,
    cookie_url = "http://trends.google.com/Cookies/NID",
    tz = 0,
    onlyInterest = TRUE)
  
  #Setting the dates for the data
  if(dim(compiled.data.items.w)[2] == 0){
    compiled.data.items.w <- tibble(date = as.Date(get.item[["interest_over_time"]][["date"]])) #Quité [1:260]
    compiled.data.items.w$date <- ymd(compiled.data.items.w$date)
  }
  
  #Selecting the relevant information from the original list
  data <- tibble("hits" = get.item[["interest_over_time"]][["hits"]],
                 "word" = get.item[["interest_over_time"]][["keyword"]])
  
  #Unnesting the data in order to transform it to a dataframe with more columns
  data <-  pivot_wider(data, names_from = "word", values_from = "hits")
  data <- unnest(data)
  #Setting the values as numeric
  data <- data.frame(lapply(data,as.numeric))
  #Saving as a variable the first value of the benchamark item in order to create the index
  first.value <- data[1,1]
  #Transforming the data to a standarized value according to the benchmark item 
  #First value = 100 (13-8-2017)
  data <- data.frame(lapply(data, function(x) {
    x/first.value* 100 
  }))
  
  #Adding the downloaded items to the compiled dataframe
  compiled.data.items.w <- cbind(compiled.data.items.w, data)
  #Emptying the segmented.items vector for the next download
  segment.items <- c()
}

#Find Duplicate Column Names
duplicated_names <- duplicated(colnames(compiled.data.items.w))

#Remove Duplicate Column Names
compiled.data.items.w <- compiled.data.items.w[!duplicated_names]

#Susbtitute NA for 1
compiled.data.items.w[is.na(compiled.data.items.w)] <- 0


data.weeklyitems <- compiled.data.items.w[2:length(compiled.data.items.w)] %>% select_if(function(col) sum(col)>260)


#For loop for calculating PCAs for all items and by categories####

list.by.category <- list() #Empty list
list.for.rotation <- list()
list.for.summary <- list()
list.for.pcas <- list()
m <- 1 #For testing

for(m in 1:ncol(list.items)) {
  #Selecting the items according to their category
  for_pca <- data.weeklyitems[, (colnames(data.weeklyitems) %in% list.items[[m]])] 
  #Create PCA
  pca.items <- prcomp(for_pca[-1], scale. = )
  
  #Creating a dataframe with the main results of the PCA
  results <- tibble("date" = compiled.data.items.w[,1],
                    for_pca, "pca_1" = pca.items$x[,1], "pca_2" = pca.items$x[,2],
                    "pca_3" = pca.items$x[,3], "pca_4" = pca.items$x[,4])
  
  #Normalizing the first two principal components 
  results <- results %>% mutate("pc1_t" = 
                                  100*(pca_1-(min(pca_1)))/
                                  (max(pca_1)-(min(pca_1)))) %>% mutate("pc2_t" = 100*(pca_2-(min(pca_2)))/
                                                                          (max(pca_2)-(min(pca_2)))) %>% mutate("pc3_t" = 100*(pca_3-(min(pca_3)))/
                                                                                                                  (max(pca_3)-(min(pca_3)))) %>% mutate("pc4_t" = 100*(pca_4-(min(pca_4)))/
                                                                                                                                                          (max(pca_4)-(min(pca_4))))
 
  results[results == 0] <- 1
  
   #Adding the dataframe and PCA analysis to the different lists
  
  t.summary <- summary(pca.items)
  
  list.by.category[m] <- list(results)
  list.for.rotation[m] <- list(cbind(row.names(pca.items$rotation),pca.items$rotation))
  list.for.pcas[m] <- list(results$date,pca.items$x)
  list.for.summary[m] <- list(cbind(row.names(t.summary$importance),t.summary$importance))
}

#Naming each element of the list according to its category
names(list.by.category) <- c(colnames(list.items)[1:length(list.by.category)])

names(list.for.rotation) <- c(colnames(list.items)[1:length(list.for.rotation)])

names(list.for.pcas) <- c(colnames(list.items)[1:length(list.for.pcas)])

names(list.for.summary) <- c(colnames(list.items)[1:length(list.for.summary)])


#Transforming the list elements to dataframes

list.for.rotation <- list.for.rotation %>% lapply(data.frame)

list.for.summary <- list.for.summary %>% lapply(data.frame)

list.for.pcas <- list.for.pcas %>% lapply(data.frame)

#Downloading to excel the results

options(scipen = 999) #Changing the exponential notation

write_xlsx(list.by.category, "results.xls")

write_xlsx(list.for.rotation, "pca_rotation.xls")

write_xlsx(list.for.summary, "pca_summary.xls")

write_xlsx(list.for.pcas, "pca factors.xls")





