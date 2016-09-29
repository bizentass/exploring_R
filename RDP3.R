library(ggplot2)
library(doBy)
library(plyr)

data_brooklyn <- read.csv(file="~/Desktop/Grad_Studies/Semester_2/CSE_587/Projects/Dataset/rollingsales_brooklyn.csv", skip = 4)
data_bronx <- read.csv(file="~/Desktop/Grad_Studies/Semester_2/CSE_587/Projects/Dataset/rollingsales_bronx.csv", skip = 4)

cleanupDataSet <- function(given_dataset) {
  # removing the dollar sign
  given_dataset$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",given_dataset$SALE.PRICE))
  
  # handling missing values
  count(is.na(given_dataset$SALE.PRICE.N))
  
  # convert any case data to lower case
  names(given_dataset) <- tolower(names(given_dataset))
  
  ## clean/format the data with regular expressions
  given_dataset$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                              given_dataset$gross.square.feet))
  given_dataset$land.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                             given_dataset$land.square.feet))
  
  given_dataset$sale.date <- as.Date(paste(given_dataset$sale.date), "%m/%d/%Y")
  given_dataset$year.built <- as.numeric(as.character(given_dataset$year.built))
  
  return (given_dataset)
}

data_brooklyn <- cleanupDataSet(data_brooklyn)
data_bronx <- cleanupDataSet(data_bronx)

# -------------------- Single Data Set Analysis ---------------------------
head(data_brooklyn)
summary(data_brooklyn)

# attach data_brooklyn to the values
attach(data_brooklyn)

hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(gross.sqft[sale.price.n==0])

# detach data_brooklyn to the values
detach(data_brooklyn)

data_brooklyn.sale <- data_brooklyn[data_brooklyn$sale.price.n!=0,]

ggplot() + 
  geom_point(data=data_brooklyn.sale, aes(gross.sqft, sale.price.n)) + 
  xlab("Gross Square Feet") +
  ylab("Sale Price")

ggplot() + 
  geom_point(data=data_brooklyn.sale, aes(log(gross.sqft), log(sale.price.n))) + 
  xlab("Gross Square Feet - Log") +
  ylab("Sale Price - Log")

data_brooklyn.homes <- data_brooklyn.sale[which(grepl("FAMILY", data_brooklyn.sale$building.class.category)),]
ggplot() + 
  geom_point(data=data_brooklyn.homes, aes(log(gross.sqft), log(sale.price.n))) + 
  xlab("Gross Square Feet of Homes - Log") +
  ylab("Sale Price of Homes - Log")

data_brooklyn.homes[which(data_brooklyn.homes$sale.price.n<100000),][order(data_brooklyn.homes[which(data_brooklyn.homes$sale.price.n<100000),]$sale.price.n),]

data_brooklyn.apts <- data_brooklyn.sale[which(grepl("APARTMENT", data_brooklyn.sale$building.class.category)),]
ggplot() + 
  geom_point(data=data_brooklyn.apts, aes(log(gross.sqft), log(sale.price.n))) + 
  xlab("Gross Square Feet of Apartments - Log") +
  ylab("Sale Price of Apartments - Log")

# remove outliers
data_brooklyn.homes$outliers <- (log(data_brooklyn.homes$sale.price.n) <=5) + 0
data_brooklyn.homes <- data_brooklyn.homes[which(data_brooklyn.homes$outliers==0),]

ggplot() + 
  geom_point(data=data_brooklyn.homes, aes(log(gross.sqft), log(sale.price.n))) + 
  xlab("Gross Square Feet of Homes - Log") +
  ylab("Sale Price of Homes - Log")

# -------------------- Cumulative Data Set Analysis (of Brooklyn and Bronx ) ---------------------------
data_cumulative <- rbind(data_brooklyn, data_bronx)
summary(data_cumulative)

data_brooklyn_huge <- subset(data_brooklyn, gross.sqft >20000)
data_brooklyn_huge$Borough_Name <- "Brooklyn"

data_bronx_huge <- subset(data_bronx, gross.sqft >20000)
data_bronx_huge$Borough_Name <- "Bronx"

ggplot() +
  geom_point(data=data_brooklyn_huge, aes(log(gross.sqft), log(sale.price.n), color=Borough_Name)) +
  geom_point(data=data_bronx_huge, aes(log(gross.sqft), log(sale.price.n), color=Borough_Name)) +
  xlab("Gross Square Feet of Huge Listings - Log") +
  ylab("Sale Price of Huge Listings - Log")
