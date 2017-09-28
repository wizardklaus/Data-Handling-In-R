install.packages("lubridate")
install.packages("Rcmdr", dependencies = T)
install.packages("JGR")
install.packages("ggplot2")
install.packages("GrapheR")
install.packages("rattle")

library(Rcmdr)

bak <- read.csv("data/bakery1_csv.csv",header=T,encoding="CP949")
str(bak)
head(bak,4)
year(bak$date)
library(lubridate)
year(bak$date)
bak$year <-year(bak$date)
bak
head(bak,3)
bak$month<-month(bak$date)
head(bak,3)
bak$day<-day(bak$date)
bak$wday<-wday(bak$date, label=T)
head(bak,3)
bak$bread<-bak$cake
bak$snack<-bak$pie+bak$cookie
bak$beverage<-bak$smoothie+bak$coffee
bak$total<-with(bak, bread+snack+beverage)
head(bak,3)
a <- c("Tues","Wed","Thurs","Fri","Sat","Sun", "Mon")
b <- c("????", "????", "????", "????", "?ָ?", "?ָ?", "????")
weekend<- data.frame(a,b)
weekend
names(weekend)
names(weekend)<-c("wday","weekends")
weekend
head(bak,2)
merge(bak,weekend, by="wday",all.x=T)

rm(list=ls())
bak <- read.csv("bakery1_csv.csv",header=T,encoding="CP949")
head(bak,3)
library(lubridate)
str(bak)
bak$date <- as.Date(bak$date,format="%Y-%m-%d")
head(bak)
bak$year <- year(bak$date)
bak$month <- month(bak$date)
bak$day <- day(bak$date)
bak$wday <- wday(bak$date,label=T)
bak$bread <- with(bak,cake)
bak$snack <- with(bak,cookie+pie)
bak$beverage <- with(bak,smoothie+coffee)
bak$total <- with(bak,bread,snack,beverage)
head(bak,3)
table(bak$wday)
a <- c("Sun","Mon","Tues","Wed","Thus","Fri","Sat")
b <- c("Y","N","N","N","N","N","Y")
weekend <- data.frame("wday"=a,"weekendYN"=b)
bak <- merge(bak,weekend,by="wday",all.x=T)

ta <- read.csv("ta.csv",encoding = "CP949")
rn <- read.csv("rn.csv",encoding = "CP949")
ta[,1]
head(ta,2)
names(ta) <- c("date","local","tempav","mintemp","maxtemp")
ta <- ta[,c(1,3)]
head(ta,2)
head(rn,3)
names(rn) <- c("date","local","rain")
rn <- rn[,c(1,3)]
str(ta)
ta$date <- as.Date(ta$date,format="%Y-%m-%d")
rn$date <- as.Date(rn$date,format="%Y-%m-%d")
bak <- merge(bak,ta,by="date",all=T)
head(bak,2)
aggregate(total~promotion+weekendYN,bak,mean)
? aggregate

library(Rcmdr)

install.packages("JGR")
install.packages("rJava")
install.packages("Deducer")
library(JGR)
library(Deducer)


install.packages("ggmap")
library(ggmap)
addr <- c("????Ư???? ?????? ???????? 611",
          "????Ư???? ?????? ???ַ?93?? 25",
          "????Ư???? ?????? ?????? 411",
          "????Ư???? ?????? ???ַ?136?? 10",
          "???? ?????? ?Ͽ??? 687-2")
name <- c("a","b","c","d","e")
gc <- geocode(enc2utf8(addr))
df <- data.frame(name,gc$lon,gc$lat)
center <- c(mean(gc$lon),mean(gc$lat))

map <- get_googlemap(center=center,maptype="roadmap",zoom=10,marker=gc)
ggmap(map,extent = "device")
