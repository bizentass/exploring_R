library(ggplot2)
library("doBy")
data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

head(data1)
data1$agecat <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

# Only logged in users have age and gender info.

data_logged_in <- subset(data1, Signed_In > 0)

siterange <- function(x){c(Length =length(x), Min =min(x), Mean =mean(x), Maximum =max(x), SD =sd(x), Variance =var(x))}
summaryBy(Age~agecat, data =data_logged_in, FUN=siterange)

CTR <- subset(data_logged_in, C )

data_men <- subset(data_logged_in, Gender == 1)
data_men$agecat <-cut(data_men$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
data_women <- subset(data_logged_in, Gender == 0)
data_women$agecat <-cut(data_women$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

data_all_under18 <- subset(data_logged_in,Age < 18)

data_clicks <- subset(data1, Clicks > 0)
data_clicks$agecat <-cut(data_clicks$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
data_impressions <- subset(data1, Impressions > 0)
data_impressions$agecat <-cut(data_impressions$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

data_logged_in$userSegment[data_logged_in$Impressions==0] <- "NoImps,NoClicks"
data_logged_in$userSegment[data_logged_in$Impressions >0] <- "Imps,NoClicks"
data_logged_in$userSegment[data_logged_in$Clicks >0] <- "Imps,Clicks"

data1$Actual_Gender[data1$Gender==0] <- "Female"
data1$Actual_Gender[data1$Gender==1] <- "Male"
data1$Logged_In[data1$Signed_In == 0] <- "False"
data1$Logged_In[data1$Signed_In == 1] <- "True"
data_logged_in$Actual_Gender[data_logged_in$Gender==0] <- "Female"
data_logged_in$Actual_Gender[data_logged_in$Gender==1] <- "Male"


#Clicks vs Age Category
ggplot(data_clicks, aes(x=Clicks, fill=agecat)) +geom_histogram(binwidth=1)
ggplot(data_clicks, aes(x=Clicks, y=agecat, fill=agecat)) +geom_boxplot()

#Impressions vs Age Category
ggplot(data_impressions, aes(x=Impressions, fill=agecat)) +geom_histogram(binwidth=1)
ggplot(data_impressions, aes(x=agecat, y=Impressions, fill=agecat)) +geom_boxplot()

#Men vs Age Category
ggplot(data_men, aes(x=Gender, fill=agecat)) +geom_histogram(binwidth=1)
ggplot(data_men, aes(x=Impressions, fill=agecat)) +geom_density() +ggtitle("Male Age Category & Impressions Density")

#Women vs Age Category (why e?)
ggplot(data_women, aes(x=Gender, fill=agecat)) +geom_histogram(binwidth=1)
ggplot(data_women, aes(x=Impressions, fill=agecat)) +geom_density() +ggtitle("Female Age Category & Impressions Density")

# Men Under 18 vs Women Under 18
ggplot(data_all_under18, aes(x=Impressions, fill=Gender)) +geom_histogram(binwidth=1)
ggplot(data=data_all_under18, aes(x=Gender)) +geom_bar(stat="bin") +guides(fill=FALSE)

# Logged In vs Logged Out
ggplot(data=data1, aes(x=Logged_In)) +geom_bar(stat="count") +guides(fill=FALSE)

ggplot(data=data1, aes(x=Logged_In, y=Impressions, fill=agecat )) +geom_bar(stat="identity", position=position_dodge())

ggplot(data=data_logged_in, aes(x=Actual_Gender, y=Impressions, fill=agecat )) +geom_bar(stat="identity", position=position_dodge())

ggplot(subset(data_logged_in, Clicks>0), aes(x=Actual_Gender, y=Clicks/Impressions, fill=agecat )) +ylab("Click_Through_Rate") +geom_bar(stat="identity", position=position_dodge())

ggplot(subset(data_logged_in, Clicks>0), aes(x=Clicks/Impressions, colour=agecat)) +xlab("Click Through Rate") +ylab("Density") +geom_density() +ggtitle("Click Through Rate & Age Category when Clicks > 0 for logged in users")

# CTR and Age CAtegory

data_logged_in$Gender = factor(data_logged_in$Gender, levels=c(0,1), labels = c("female", "male"))

data_logged_in_summarised = na.omit(data_logged_in) %>% group_by(agecat, userSegment, Gender) %>% summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))
head(data_logged_in_summarised)

ggplot(data=subset(data_logged_in, userSegment == "Imps,Clicks"), aes(x=agecat, y=Clicks/Impressions, fill=Gender)) + 
  geom_bar(colour="black", stat="identity", position=position_dodge(), size=0.3) +
  scale_fill_hue(name="Sex of user segment") +
  labs(
    x = "Age Category",
    y = "CTR",
    color = "Cylinders"
  )
