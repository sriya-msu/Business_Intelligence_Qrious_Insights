# Load the data
setwd("C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data")
shopper_data <- read.csv("export_shopper.txt",header=TRUE,sep="\t")

# 1. What are adjacent categories being shopped and purchased?
library(dplyr)
shopper_data <- read.csv("export_shopper.txt",header=TRUE,sep="\t")

# Extract only the relevant columns
relevant_cols <- c("panelistid", "eventtype", "category")
shopper_data <- shopper_data[,relevant_cols]

# Remove rows where category is NA
shopper_data <- shopper_data[!is.na(shopper_data$category),]

# Group the data by session
session_groups <- export_shopper %>%
  group_by(panelistid, capturemethod, eventtype) %>%
  arrange(eventtimeutc)

# Identify adjacent categories being shopped and purchased
adj_categories <- shopper_data %>% 
  arrange(panelistid, eventtype) %>%
  group_by(panelistid) %>%
  mutate(prev_category = lag(category)) %>%
  filter(eventtype %in% c("Add to Basket", "Purchase")) %>%
  filter(category != prev_category) %>%
  summarise(adj_category = paste(prev_category, category, sep = " > ")) %>%
  ungroup() %>%
  distinct()
# Identify the previous and next categories being shopped and purchased
copurchase_behaviour <- session_groups %>%
  mutate(prev_category = lag(category),
         next_category = lead(category),
         prev_eventtype = lag(eventtype),
         next_eventtype = lead(eventtype)) %>%
  filter(eventtype %in% c("Add to Basket", "Purchase") & category == "Electronics") %>%
  select(panelistid, prev_category, category, next_category, prev_eventtype, eventtype, next_eventtype)

# View the result
write.table(copurchase_behaviour,"copurchase_behaviour.txt",sep="\t",row.names=FALSE,col.names=TRUE)

# Aggregate all the previous and next adjacent purchases and plot them in Tableau

# 2. What is the purchase journey in-app
# In-app shopping journey
library(dplyr)
data <- read.table("catshopper.txt", header = TRUE, sep = "\t")
data <- data %>% group_by(panelistid) %>% arrange(shoppertime)
sequences <- data %>% summarize(sequence = paste(eventtype, collapse = " > "))
sequence_counts <- sequences %>% group_by(sequence) %>% summarize(count = n())
top_sequences <- sequence_counts %>% arrange(desc(count))
write.table(top_sequences,"purchase_journey(all).txt",sep="\t",row.names=FALSE,col.names=TRUE)


# journeys leading to purchase
library(dplyr)
data <- read.table("catshopper.txt", header = TRUE, sep = "\t")
data <- data %>% group_by(panelistid) %>% arrange(shoppertime)
sequences <- data %>% summarize(sequence = paste(eventtype, collapse = " > "))
sequences <- sequences %>% filter(grepl("Purchase", sequence))
sequence_counts <- sequences %>% group_by(sequence) %>% summarize(count = n())
top_sequences <- sequence_counts %>% arrange(desc(count))
write.table(top_sequences,"journey_to_purchase.txt",sep="\t",row.names=FALSE,col.names=TRUE)

# How often is the app used and what percent of time is the category being shopped among all shopping trips

# Percentage of Electronics shopped among all categories by a panelist
category_freq_shopped <- export_shopper %>%
  group_by(panelistid) %>%
  summarize(electronics_count = sum(category == "Electronics"), 
            total_count = n()) %>%
  mutate(electronics_percentage = electronics_count/total_count * 100)

category_freq_shopped <- category_freq_shopped%>%
  arrange(desc(electronics_percentage))

dim(category_freq_shopped)

View(category_freq_shopped)

library(ggplot2)

ggplot(category_freq_shopped, aes(x = panelistid, y = electronics_percentage)) +
  geom_point() +
  labs(x = "Panelist ID", y = "Electronics Percentage")

write.table(category_freq_shopped, "C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/category_freq_shopped.txt", sep="\t", row.names=FALSE, col.names=TRUE)

# Percentage of Electronics purchased among all categories by a panelist
category_frequency_purchase <- export_shopper %>%
  group_by(panelistid) %>%
  filter(eventtype == "Purchase")%>%
  summarize(electronics_count = sum(category == "Electronics"), 
            total_count = n()) %>%
  mutate(electronics_percentage = electronics_count/total_count * 100)

category_frequency_purchase <- category_frequency_purchase%>%
  arrange(desc(electronics_percentage))

dim(category_frequency_purchase)
View(category_frequency_purchase)

write.table(category_frequency_purchase, "C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/category_frequency_purchase.txt",sep="\t", row.names=FALSE, col.names=TRUE)

# If we were to advertise this category or products, where would be ideal (i.e., where else are these shoppers spending their time digitally, what interests do they have) 

# What is the decision hierarchy (i.e., where does this category fall in the shopping process – do they start with this in mind (at the retailer site or in the search engine) or do they start at a retailer for other items and then later shop this category)

# What is the overall category conversion rate

panel_data <- `export_panelist=2023.02.22`

check_null_panel <- `export_panelist=2023.02.22` %>% 
  filter((is.null(panelistid))|
           is.null(received)|
           is.null(alldemosanswered)|
           is.null(locationtrackingenabled)|
           is.null(accessibility_enabled)|
           is.null(visits)|
           is.null(web)|
           is.null(app)|
           is.null(shopper)|
           is.null(social)|
           is.null(media))
## returns no NULL

panel_id_unique <- unique(panel_data$panelistid)
panel_id_unique


panel_data=panel_data%>%arrange(panelistid,received)
View(input)
table(panel_data$alldemosanswered)
table(panel_data$locationtrackingenabled)
table(panel_data$accessibility_enabled)
panel_data$use=(panel_data$alldemosanswered==1)*(panel_data$locationtrackingenabled==1)*(panel_data$accessibility_enabled==1)

#select those user-days with all three answers 1 (excluding observations with NAs)
panel_data_2=panel_data[(panel_data$use==1)&(!is.na(panel_data$use)),c("panelistid","received")]

write.table(panel_data_2,"export_userbasis.txt",sep="\t",row.names=FALSE,col.names=TRUE)

# Creating CSV
write.csv(panel_data_2, "C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/panel_data_2.csv", row.names=FALSE)

## media table further cleaning

media_data <- export_media

check_null_media <- export_media %>%
  filter((is.null(panelistid))|
           is.null(osname)|
           is.null(devicemanufacturer)|
           is.null(devicetype)|
           is.null(mediaplayer)|
           is.null(advert)|
           is.null(title)|
           is.null(category)|
           is.null(channel)|
           is.null(visittimeutc)|
           is.null(visittimelocal)|
           is.null(visitduration)|
           is.null(mediaduration))
## returns no NULL

media_unique <- as.data.frame(unique(media_data$panelistid))
View(media_unique)
Media_unique_table <- unique(media_data$panelistid)

# Can we gain a deep understanding of search terms used to inform SEO strategies.
# What are key search terms used in-app (are they by product description, brand name, product type/specifics, etc.)

# Textual Analysis for Shopper Search Terms

setwd("C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/Data")

catshopper <- read.csv("catshopper.txt", sep="\t")
View(as.data.frame(unique(catshopper$searchterm)))
View(catshopper)
write.csv(catshopper$searchterm, "C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/Data/Shopper_search/searchterm.txt")

library(tm)
library(SnowballC)
library(topicmodels)
library(wordcloud)

docs=Corpus(DirSource("C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/Data/Shopper_search"))
docs

writeLines(as.character(docs[[1]]))

# Remove punctuation
# Create a corpus object

#show pre-processing methods
getTransformations()
#create the toSpace content transformer
toSpace=content_transformer(function(x,pattern){return(gsub(pattern," ",x))})
docs1=tm_map(docs,toSpace,"-")
docs2=tm_map(docs1,toSpace,":")

#remove punctuation
docs3=tm_map(docs2,removePunctuation)
#transform to lower case
docs4=tm_map(docs3,content_transformer(tolower))
#remove numbers
docs5=tm_map(docs4,removeNumbers)
#remove stop-words
docs6=tm_map(docs5,removeWords,stopwords("english"))
#remove additional whitespaces
docs7=tm_map(docs6,stripWhitespace)
#stem documents
docs8=tm_map(docs7,stemDocument)

mat=DocumentTermMatrix(docs8,control=list(wordLengths=c(3,20),weighting=weightTfIdf))
mat
dim(mat)
mat=DocumentTermMatrix(docs8,control=list(wordLengths=c(3,20),weighting=weightTf))
dim(mat)
print(docs8)
inspect(mat)
findFreqTerms(mat,lowfreq=40)

#create a word cloud
set.seed(12)
par(mfrow=c(1,1))
options(repr.plot.width=100, repr.plot.height=100)
for(k in 1){
  wordcloud(names(as.matrix(mat)[k,]),as.matrix(mat)[k,],min.freq=3,max.words=200,colors=brewer.pal(6,"Dark2"))}

dim(mat)

mat2 = as.matrix((mat))[1,]
# Convert mat to a data.frame
df <- data.frame(word = names(mat2), count = as.numeric(as.matrix(mat)))

# Sort the data.frame by count in descending order
df_sorted <- df[order(-df$count), ]

# Select the top 10 words with the highest count
top_n <- 300
top_words <- head(df_sorted, n = top_n)

# Print the top words with their counts
print(top_words)

set.seed(12)
par(mfrow=c(1,1))
options(repr.plot.width=300, repr.plot.height=300)
for(k in 1){
  wordcloud(top_words[,1],top_words[,2],min.freq=3, max.words=100,colors=brewer.pal(6,"Dark2"))}

# Textual Analysis for Web Search Terms

setwd("C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/Data")

relatedweb <- read.csv("relatedweb.txt", sep="\t")
View(as.data.frame(unique(relatedweb$searchterm)))
View(relatedweb)
write.csv(relatedweb$searchterm, "C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/Data/Web/searchterm.txt")

library(tm)
library(SnowballC)
library(topicmodels)
library(wordcloud)

docs=Corpus(DirSource("C:/Users/91782/OneDrive/Documents/ACC Project/Data/Processed_data/Data/web"))
docs

writeLines(as.character(docs[[1]]))

# Remove punctuation
# Create a corpus object

#show pre-processing methods
getTransformations()
#create the toSpace content transformer
toSpace=content_transformer(function(x,pattern){return(gsub(pattern," ",x))})
docs1=tm_map(docs,toSpace,"-")
docs2=tm_map(docs1,toSpace,":")


#remove punctuation
docs3=tm_map(docs2,removePunctuation)
#transform to lower case
docs4=tm_map(docs3,content_transformer(tolower))
#remove numbers
docs5=tm_map(docs4,removeNumbers)
#remove stop-words
docs6=tm_map(docs5,removeWords,stopwords("english"))
#remove additional whitespaces
docs7=tm_map(docs6,stripWhitespace)
#stem documents
docs8=tm_map(docs7,stemDocument)

mat=DocumentTermMatrix(docs8,control=list(wordLengths=c(3,20),weighting=weightTfIdf))
mat
dim(mat)
mat=DocumentTermMatrix(docs8,control=list(wordLengths=c(3,20),weighting=weightTf))
dim(mat)
print(docs8)
inspect(mat)
findFreqTerms(mat,lowfreq=80)

#create a word cloud
set.seed(12)
par(mfrow=c(1,1))
options(repr.plot.width=100, repr.plot.height=100)
for(k in 1){
  wordcloud(names(as.matrix(mat)[k,]),as.matrix(mat)[k,],min.freq=3,max.words=200,colors=brewer.pal(6,"Dark2"))}


dim(mat)

mat2 = as.matrix((mat))[1,]
# Convert mat to a data.frame
df <- data.frame(word = names(mat2), count = as.numeric(as.matrix(mat)))

# Sort the data.frame by count in descending order
df_sorted <- df[order(-df$count), ]

# Select the top 10 words with the highest count
top_n <- 300
top_words <- head(df_sorted, n = top_n)

# Print the top words with their counts
print(top_words)

set.seed(12)
par(mfrow=c(1,1))
options(repr.plot.width=500, repr.plot.height=500)
for(k in 1){
  wordcloud(top_words[,1],top_words[,2],min.freq=3, max.words=300,colors=brewer.pal(6,"Dark2"))}

# What are the top products viewed but not purchased
# What are the top products viewed and purchased

setwd("C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data")

shopper=read.csv("catshopper.txt",header=TRUE,sep="\t")
View(shopper)

purchase=read.csv("purchase.txt",header=TRUE,sep="\t")
View(purchase)

relatedapp=read.csv("relatedapp.txt",header=TRUE,sep="\t")
View(relatedapp)

relatedmedia=read.csv("relatedmedia.txt",header=TRUE,sep="\t")
View(relatedmedia)

relatedmedia_3hrs=read.csv("relatedmedia_3hours.txt",header=TRUE,sep="\t")
View(relatedmedia_3hrs)

relatedsocial=read.csv("relatedsocial.txt",header=TRUE,sep="\t")
View(relatedsocial)

relatedvisits=read.csv("relatedvisits.txt",header=TRUE,sep="\t")
View(relatedvisits)

relatedweb=read.csv("relatedweb.txt",header=TRUE,sep="\t")
View(relatedweb)

df = subset(shopper, select = c(panelistid, eventtype, productname, shopperdate, shoppertime))
View(df)

unique_event <- unique(df$eventtype)
unique_event

num_unique <- length(unique(df$eventtype))
num_unique

num_event <- sum(df$eventtype == "")
num_event

df2 <- df[!(df$eventtype == ""),]
View(df2)

unique_pid = unique(df$panelistid)
unique_pid

# Number of "Add to basket", "Product page view" and "purchase" across shopper data
freq_table <- table(df2$eventtype)
freq_table

library(stringr)
df2[c('event', 'a','b')] <- str_split_fixed(df2$eventtype, ' ', 3)
View(df2)

freq_table1 <- table(df2$event)
freq_table1

df3 = subset(df2, select = -c(eventtype,a,b))
View(df3)

df4 = subset(df3, select = -c(productname, shopperdate, shoppertime))
View(df4)

# Get the unique values in column "event"
unique_values <- unique(df4$event)
unique_values

# Create new columns for each unique value
df5 = df4 %>% mutate(productpage = ifelse(event == "Product", 1, 0),
                     add_to_basket = ifelse(event == "Add", 1, 0),
                     purchase = ifelse(event == "Purchase", 1, 0))
View(df5)

df6 = subset(df5, select = -c(event))
View(df6)

df7 = df6 %>%
  group_by(panelistid) %>%
  summarize(productpage = sum(productpage))
View(df7)

df8 = df6 %>%
  group_by(panelistid) %>%
  summarize(add_to_basket = sum(add_to_basket))
View(df8)

df9 = df6 %>%
  group_by(panelistid) %>%
  summarize(purchase = sum(purchase))
View(df9)

df10 <- inner_join(df7, df8, by = "panelistid") %>%
  inner_join(df9, by = "panelistid")
View(df10)

freq_table2 <- table(df10$purchase)
freq_table2

#table with all unique panelistid making purchases)
df11 = subset(df10, purchase %in% c(1,2,3,4,5,6,7,8,9))
View(df11)

sum_productpage <- sum(df11$productpage)
sum_productpage

sum_addtobasket <- sum(df11$add_to_basket)
sum_addtobasket

sum_purchase <- sum(df11$purchase)
sum_purchase


df12 = subset(df2, select = c(panelistid,eventtype,productname))
View(df12)

freq_table4 <- table(df12$eventtype)
freq_table4

df13 = df12 %>% mutate(productpage = ifelse(eventtype == "Product Page View", 1, 0),
                       add_to_basket = ifelse(eventtype == "Add to Basket", 1, 0),
                       purchase = ifelse(eventtype == "Purchase", 1, 0))
View(df13)

df14 = subset(df13, select = -c(eventtype))
View(df14)

df15 = df14 %>%
  group_by(productname) %>%
  summarize(productpage = sum(productpage))
View(df15)

df16 = subset(df14, select = -c(add_to_basket))
View(df16)

df17 = df16 %>%
  group_by(productname) %>%
  summarize(productpage = sum(productpage))
View(df17)

df18 = df16 %>%
  group_by(productname) %>%
  summarize(purchase = sum(purchase))
View(df18)

df19 <- inner_join(df17, df18, by = "productname")
View(df19)

df20 = subset(df18, purchase == 0)
View(df20)

df21 = inner_join(df17, df20, by = "productname")
View(df21)

sorted_data <- df21[order(df21$productpage, decreasing = TRUE),]
View(sorted_data)

top_5 <- head(sorted_data, 5)
top_5

# How often is this category purchased

setwd("C:/Users/sriya/OneDrive/Documents/BDSA/Qrious Insights/Processed Data/Data")

data <- read.csv("purchase.txt", sep="\t")

View(data)
str(data)
dim(data)

# Convert the shopper time column to a datetime object
data$shoppertime <- as.POSIXct(data$shoppertime, format = "%Y-%m-%d %H:%M:%S")

# Group the data by panelist ID and calculate the time difference between consecutive purchases
data <- data %>% 
  arrange(panelistid, shoppertime) %>% 
  group_by(panelistid) %>% 
  mutate(time_diff = difftime(shoppertime, lag(shoppertime), units = "days"))

View(data)

# Calculate the overall average time difference between purchases
overall_avg_time_diff <- mean(data$time_diff, na.rm = TRUE)

# View the result
overall_avg_time_diff

data <- read.csv("purchase.txt", sep="\t")
# Count the number of purchases for each panelist
purchases <- data %>%
  group_by(panelistid) %>%
  summarise(num_purchases = n())

# Count the number of returning customers
returning_customers <- purchases %>%
  filter(num_purchases > 1) %>%
  count()

# Calculate the re-purchase frequency as the proportion of returning customers
repurchase_freq <- returning_customers$n / nrow(purchases)

# View the result
repurchase_freq

data <- read.csv("purchase.txt", sep="\t")

# Convert the shopper time column to a datetime object
data$shoppertime <- as.POSIXct(data$shoppertime, format = "%Y-%m-%d %H:%M:%S")

data$month <- format(data$shoppertime, "%m-%Y")

# Group the data by panelist ID and calculate the time difference between consecutive purchases
data2 <- data %>% 
  arrange(shoppertime) %>% group_by(month) %>%
  mutate(time_diff = difftime(shoppertime, lag(shoppertime), units = "day"))

data2 <- na.omit(data2)

# Calculate the overall average time difference between purchases
overall_avg_time_diff <- data2 %>% group_by(month) %>% summarize(mean = mean(time_diff))

# View the result
overall_avg_time_diff

overall_avg_time_diff$month = as.Date(paste0("01-", overall_avg_time_diff$month), format = "%d-%m-%Y")
overall_avg_time_diff$mean = as.numeric(overall_avg_time_diff$mean)

library(ggplot2)

ggplot(data = overall_avg_time_diff) + geom_line(mapping = aes(x = month, y = 1/mean)) + ylab("Purchases per Day") + xlab("Month")

#plot(overall_avg_time_diff$month, overall_avg_time_diff$mean, type="l")

overall_avg_time_diff

write.csv(as.data.frame(overall_avg_time_diff), "purchase_freq.txt")