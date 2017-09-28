install.packages("readxl")
dim(x) <- c(3,2)
x <- c(3,2,4,5)
? dim()
x <- c(1:10)
x <- 1:12; dim(x) <- c(3,4)
x
dim(x)
adply(iris, 1, function(row){row$Sepal.Length >= 5.0 & row$Species == "setosa"})
library(plyr)
ddply(baseball,.(id), function(sub){mean(sub$g)})
head(ddply(baseball, .(id), mutate, cyear=year-min(year)+1, log_cyear=log(cyear)))
french_fries
head(french_fries)
library(reshape2)
(m <- melt(french_fries, id.vars=1:4))
french_fries[!complete.cases(french_fries),]
dcast(m, time~treatment+variable, mean, na.rm=TRUE)
iris[1,1]
iris1 <- as.data.table(iris)
library(data.table)
iris1[1,1]
DF <- read.csv("data/example_studentlist.csv")
Sys.setlocale(category="LC_COLLATE", "ko_KR.UTF-8");
(BMI <- DF$weight/DF$height^2)
cbind(DF,BMI)
DF <- read.csv("data/example_studentlist.csv")
Freq <- table(DF$bloodtype)
(ReleativeFreq <- prop.table(Freq))
(Table <- rbind(Freq,ReleativeFreq))
(Table <- addmargins(Table))
(FactorOfHeight <- cut(DF$height, breaks=4))
(FreqOfHeight <- table(FactorOfHeight))
(FreqOfHeight <- rbind(FreqOfHeight, prop.table(FreqOfHeight)))
rownames(FreqOfHeight)[2] <- 'RelativeFreq'
(cumuFreq <- cumsum(FreqOfHeight[2,]))








