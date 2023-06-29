# Data Cleaning and Pre-processing

# Cleaning

#0. load the packages and set working directory
library(jsonlite)
library(dplyr)
library(tcltk)


#setwd("/Data/panelist_oct_to_feb")
#setwd("/Data/app_oct_to_feb")
#setwd("/Data/shopper_oct_to_feb")
#setwd("/Data/visits_oct_to_feb")
#setwd("/Data/media_oct_to_feb")
setwd("/Data/social_oct_to_feb")

#1. process each folder (day) of data, and save them as a tab delimited text file.
dir=list.dirs()
n=length(dir)
pb <- tkProgressBar(title = "progress bar", min = 0,max = n, width = 500)

for (k in 2:n){
  setTkProgressBar(pb,k-1,label=paste(round((k-1)/n*100, 0),"% done"))
  print(dir[k])
  files=list.files(dir[k])
  len=length(files)
  
  for (i in 1:len){
    con=paste(dir[k],"/",files[i],sep="")
    if (i==1) data=stream_in(file(con))
    else data=rbind(data,stream_in(file(con)))
  }
  data=data%>%select(-instanceid)%>%
    select(-ends_with("epoch"))%>%
    select(-starts_with("duplicate"))%>%
    select(-ends_with("version"))
  
   #for social data
  data=data[,!(colnames(data) %in% c("osname","devicemodel","devicemodel"))]
  vars=c("advertiser","metadata","promotiontype")
  for (i in 1:length(vars)){
    data[,vars[i]] = gsub("[[:punct:]]", " ", data[,vars[i]])
    data[,vars[i]] = gsub("\\s+", " ", data[,vars[i]])
  }
  data$metadata=substr(data$metadata,start=1,stop=128)
  
  
  write.table(data,paste(substr(dir[k],start=3,stop=nchar(dir[k])),".txt",sep=""),sep="\t",row.names=FALSE,col.names=TRUE)
}
close(pb)
print("done!")


# 2. load each processed tab delimited data, combine them together, and save the data
setwd("/Users/sriya/Data/social_oct_to_feb")

inter=list.files()
inter=inter[substr(inter,start=nchar(inter)-2,nchar(inter))=="txt"]

n=length(inter)

for (i in 1:n){
  print(i)
  input=read.csv(inter[i],header=TRUE,sep="\t")
  if (i==1) output=input
  else output=rbind(output,input)
}

write.table(output,"export_social.txt",sep="\t",row.names=FALSE,col.names=TRUE)


#check saved data imported correctly
setwd("/Users/sriya/Data")
input=read.csv("export_visits.txt",header=TRUE,sep="\t")

# Define user basis data

# Loading Panelist data
setwd("C:/Users/sriya/OneDrive/Documents/ACC Project/Data")
#for panelist data
input=read.csv("export_panelist=2023-02-22.txt",header=TRUE,sep="\t")

input=input%>%arrange(panelistid,received)
View(input)
table(input$alldemosanswered)
table(input$locationtrackingenabled)
table(input$accessibility_enabled)

input$use=(input$alldemosanswered==1)*(input$locationtrackingenabled==1)*(input$accessibility_enabled==1)
#select those user-days with all three answers 1 (excluding observations with NAs)
output=input[(input$use==1)&(!is.na(input$use)),c("panelistid","received")]

write.table(output,"export_userbasis.txt",sep="\t",row.names=FALSE,col.names=TRUE)

# Data Pre-processing

#0. load the packages and set working directory
library(dplyr)

setwd("C:/Users/sriya/OneDrive/Documents/ACC Project/Data")

# 0. process the panelist data
# inner join with the userbasis data (only keep those user-day records in the userbasis table)
userbasis=read.csv("export_userbasis.txt",header=TRUE,sep="\t")
panelist=read.csv("export_panelist=2023-02-22.txt",header=TRUE,sep="\t")
View(userbasis)
#you have to standardize the date format, before inner join
panelist$received=as.Date(panelist$received,format="%m/%d/%y")
userbasis$received=as.Date(userbasis$received,format="%m/%d/%y")
View(userbasis)
panelist=panelist%>%inner_join(userbasis,by=colnames(userbasis))%>%
  arrange(panelistid,received)
panelist=panelist[!duplicated(panelist),]

setwd("C:/Users/sriya/OneDrive/Documents/ACC Project/Data")
# 1. check the other data
shopper=read.csv("export_shopper.txt",header=TRUE,sep="\t")

setwd("D:/Data")
media=read.csv("export_media.txt",header=TRUE,sep="\t")
visits=read.csv("export_visits.txt",header=TRUE,sep="\t")
social=read.csv("export_social.txt",header=TRUE,sep="\t")


shopper=shopper%>%arrange(panelistid,eventtimeutc)

catshopper=shopper%>%filter(category %in% c("Electronics"))
length(unique(catshopper$productname))

# rule 2, if there are keywords computer in the product name, using case insensitive.
# the whitespace before and after the words make sure the words alone are presented
index1=grepl(" computer ",tolower(shopper$productname))

# if any of the two keywords showed in product name
catshopper2=shopper[index1,]
catshopper=rbind(catshopper,catshopper2)
# remove duplicated records
catshopper=catshopper[!duplicated(catshopper),]
rm(catshopper2)

catshopper$shopperdate=substring(catshopper$eventtimeutc,first=1,last=10)
catshopper$shoppertime=strptime(catshopper$eventtimeutc,"%Y-%m-%d %H:%M:%S",tz="UTC")
catshopper=catshopper%>%select(-starts_with("eventtime"))

length(unique(catshopper$panelistid))

write.table(catshopper,"catshopper.txt",sep="\t",row.names=FALSE,col.names=TRUE)

purchase=catshopper%>%filter(eventtype=="Purchase")%>%
  arrange(panelistid,shoppertime)%>%
  select(panelistid,shoppertime,shopperdate)

# we find 458 purchases and 297 focused users out of 4525 users in total for our category
write.table(purchase,"purchase.txt",sep="\t",row.names=FALSE,col.names=TRUE)
setwd("D:/Data")
purchase = read.csv("purchase.txt",sep="\t", header=TRUE)


users=purchase%>%group_by(panelistid)%>%summarize()
View(users)
# 1.1. process the media behavior before purchase
# define focused users as the users who purchased at least one item in a certain category
catmedia=users%>%inner_join(media,by="panelistid")
catmedia$mediatime=strptime(catmedia$visittimeutc,"%Y-%m-%d %H:%M:%S",tz="UTC")
catmedia=catmedia%>%select(-starts_with("visittime"))

#we join the purchase and media behavior, and restrict the time window to "media behavior must occur 0 to 6 hours before purchase."
#finally, we get only 522 media behavior records, so it is easy to analyze the results
#if you reduce the time window to 0 to 1/2/3 hours, there will be fewer records
relatedmedia=purchase%>%inner_join(catmedia,by="panelistid")%>%
  arrange(panelistid,shoppertime,mediatime)%>%
  mutate(timegap=as.numeric(difftime(shoppertime,mediatime,units="hours")))%>%
  filter(timegap>0&timegap<6)
write.table(relatedmedia,"relatedmedia.txt",sep="\t",row.names=FALSE,col.names=TRUE)
View(relatedmedia)

relatedmedia2=purchase%>%inner_join(catmedia,by="panelistid")%>%
  arrange(panelistid,shoppertime,mediatime)%>%
  mutate(timegap=as.numeric(difftime(shoppertime,mediatime,units="hours")))%>%
  filter(timegap>0&timegap<3)
write.table(relatedmedia2,"relatedmedia_3hours.txt",sep="\t",row.names=FALSE,col.names=TRUE)
View(relatedmedia2)

# 1.2. process the visits behavior before purchase
# we get only 671 visits behavior records
catvisits=users%>%inner_join(visits,by="panelistid")
catvisits$visitstime=strptime(catvisits$arrivaltime,"%Y-%m-%d %H:%M:%S",tz="UTC")
catvisits=catvisits%>%select(-arrivaltime,-departuretime)

relatedvisits=purchase%>%inner_join(catvisits,by="panelistid")%>%
  arrange(panelistid,shoppertime,visitstime)%>%
  mutate(timegap=as.numeric(difftime(shoppertime,visitstime,units="hours")))%>%
  filter(timegap>0&timegap<6)
write.table(relatedvisits,"relatedvisits.txt",sep="\t",row.names=FALSE,col.names=TRUE)
View(relatedvisits)
length(relatedvisits$venueid)

# 1.3. process the social behavior before purchase
# we get only 1903 social behavior records
catsocial=users%>%inner_join(social,by="panelistid")
catsocial$socialtime=strptime(catsocial$eventtimeutc,"%Y-%m-%d %H:%M:%S",tz="UTC")
catsocial=catsocial%>%select(-starts_with("eventtime"))

relatedsocial=purchase%>%inner_join(catsocial,by="panelistid")%>%
  arrange(panelistid,shoppertime,socialtime)%>%
  mutate(timegap=as.numeric(difftime(shoppertime,socialtime,units="hours")))%>%
  filter(timegap>0&timegap<6)
write.table(relatedsocial,"relatedsocial.txt",sep="\t",row.names=FALSE,col.names=TRUE)

View(relatedsocial)

nrow(users)
length(unique(panelist$panelistid))

# 1.4. process the app behavior before purchase
# since app data is separated in each day, we have to use a loop to get the app behavior of focused users
# the loop, we re-use the storage memory of data object "input" 

# setwd("/Users/sriya/Data/app_oct_to_feb")
# inter=list.files()
# inter=inter[substr(inter,start=nchar(inter)-2,nchar(inter))=="txt"]
# n=length(inter)
# 
# for (i in 1:n){
#   print(i)
#   input=read.csv(inter[i],header=TRUE,sep="\t")
#   input=input%>%inner_join(users,by="panelistid")
#   if (i==1) catapp=input
#   else catapp=rbind(catapp,input)
# }

# write.table(catapp,"catapp.txt",sep="\t",row.names=FALSE,col.names=TRUE)

# No Need to run above code every time, since the data is saved, run the following code
setwd("C:/Users/sriya/OneDrive/Documents/ACC Project/Data")
catapp=read.csv("catapp.txt",header=TRUE,sep="\t")
# process app data of focused users
# we get only 9606 app behavior records
catapp$apptime=strptime(catapp$starttimeutc,"%Y-%m-%d %H:%M:%S",tz="UTC")
catapp=catapp[,!(colnames(catapp) %in% c("sessionstarttime","sessionendtime","starttimeutc","endtimeutc"))]

relatedapp=purchase%>%inner_join(catapp,by="panelistid")%>%
  arrange(panelistid,shoppertime,apptime)%>%
  mutate(timegap=as.numeric(difftime(shoppertime,apptime,units="hours")))%>%
  filter(timegap>0&timegap<6)
write.table(relatedapp,"relatedapp.txt",sep="\t",row.names=FALSE,col.names=TRUE)


# 1.5. process the web behavior before purchase
# setwd("/Users/sriya/Data/web_oct_to_feb")
# inter=list.files()
# inter=inter[substr(inter,start=nchar(inter)-2,nchar(inter))=="txt"]
# n=length(inter)
# 
# for (i in 1:n){
#   print(i)
#   input=read.csv(inter[i],header=TRUE,sep="\t")
#   input=input%>%inner_join(users,by="panelistid")
#   if (i==1) catweb=input
#   else catweb=rbind(catweb,input)
# }
# 
# setwd("/Users/sriya/Data")
# write.table(catweb,"catweb.txt",sep="\t",row.names=FALSE,col.names=TRUE)

# No need to run the above code every time, since the data is saved, run the following code
catweb=read.csv("catweb.txt",header=TRUE,sep="\t")
# process web data of focused users
# we get only 1714 web behavior records
catweb$webtime=strptime(catweb$starttimeutc,"%Y-%m-%d %H:%M:%S",tz="UTC")
catweb=catweb[,!(colnames(catweb) %in% c("sessionstarttime","starttimeutc"))]

relatedweb=purchase%>%inner_join(catweb,by="panelistid")%>%
  arrange(panelistid,shoppertime,webtime)%>%
  mutate(timegap=as.numeric(difftime(shoppertime,webtime,units="hours")))%>%
  filter(timegap>0&timegap<6)

write.table(relatedweb,"relatedweb.txt",sep="\t",row.names=FALSE,col.names=TRUE)/
  
  View(relatedweb)
analyze <- relatedweb %>% filter(pageduration>10)
View(analyze)