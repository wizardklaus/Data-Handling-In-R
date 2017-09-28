## 실습 전 준비 ####

## 라이브러리 설치
install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')

## 라이브러리 불러오기
library(dplyr)
library(tidyr)
library(ggplot2)

## 데이터 불러오기
data <- read.csv('data/bank_example.csv')

## dplyr ####
## select
newdata <- data%>%
  select(balance, age, y)

newdata <- data%>%
  select(-balance, -age, -y)

newdata <- data%>%
  select(starts_with('p'))

newdata <- data%>%
  select(y, everything())

newdata <- data%>%
  select(BALANCE=balance, AGE=age, CLASS=y)

##예제) select
# default, education, housing, y 선택하기 

## mutate
newdata <- data%>%
  mutate(balanceperage = balance/age)

newdata <- data%>%
  mutate(balance = balance^2)

## filter
newdata <- data%>%
  filter(balance > 0)

## group_by
newdata <- data%>%
  group_by(month)

newdata <- data%>%
  group_by(month)%>%
  summarise(sum_balance=sum(balance))

newdata <- data%>%
  group_by(month, job)%>%
  summarise(sum_balance=sum(balance))

newdata <- data%>%
  select(month, balance, duration)%>%
  group_by(month)%>%
  summarise_all(funs(sum, mean))

##예제) group_by, filter, summarise
# 전체 데이터에서 NA값을 제거 후, y를 그룹으로 balance의 평균 구하기
# balance의 평균을 구하는 함수: mean(balance)

## tidyr ####
newdata <- data[1:2,]%>%select(month, age, balance, duration)%>%
  gather(., key='variable', value='value', age, balance, duration)

## 위와 같은 결과
newdata <- data[1:2,]%>%select(month, age, balance, duration)%>%
  gather(., variable, value, -month)

##예제)
# y는 그대로 두고 default, education, housing를 하나로 쌓기

## ggplot2 ####
## scatter plot
p <- ggplot(data=data, aes(x=balance, y=duration))
p + geom_point()

## histogram
p <- ggplot(data=data, aes(x=balance))
p + geom_histogram(bins=10)

p <- ggplot(data=data, aes(x=balance))
p + geom_histogram(bins=1000)

## bar
p <- ggplot(data=data, aes(x=job))
p + geom_bar()

## box
p <- ggplot(data=data, aes(x=job, y=age))
p + geom_boxplot()

## color
p <- ggplot(data=data, aes(x=balance, y=duration, color=y))
p + geom_point()

p <- ggplot(data=data, aes(x=job, fill=y))
p + geom_bar()

p <- ggplot(data=data, aes(x=job, fill=y))
p + geom_bar(position='fill')

## facet
p <- ggplot(data=data, aes(x=marital))
p + geom_bar()+ facet_wrap(~y)

## pretty
p <- ggplot(data=data, aes(x=job, fill=y))
p + geom_bar(position='fill')+
  labs(title="직업별 정기예금 가입자 비율 바 그래프",
       x="직업",
       y="가입자 비율")+
  scale_fill_manual(values=c('#B2DF8A','#33A02C'))+
  theme(title=element_text(size=24),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=14, angle=270, hjust=0),
        axis.text.y=element_text(size=14)) +
  theme_bw()

## all together
# marital과 education은 서로 다른 level을 가지고 있기 때문에 워닝이 발생함
data%>%
  select(marital, education, y)%>%
  gather(., key='variable', value='value', marital, education)%>%
  ggplot(., aes(x=y, fill=value))+
  geom_bar(position='fill')+
  facet_wrap(~variable)+
  theme_bw()

# practice
data%>%
  select(default, education, housing, job, loan, marital, y) %>%
  gather(., key='variable', value='value', default, education, housing, job, loan, marital) %>%
  ggplot(., aes(x=y, fill=value)) +
  geom_bar(position='fill') +
  facet_wrap(~ variable) +
  theme_bw()

library(tidyr)
library(RColorBrewer)
df.p <- data%>%
  select(job, marital, education, default, housing, loan, y)%>%
  gather(., key='variable', value='value', job, marital, education, default, housing, loan)

# 색 정의
colPal <- c(brewer.pal(n = 9, name = "Set1")[1],
            brewer.pal(n = 9, name = "Set1")[2:4],
            brewer.pal(n = 11, name = "Set3")[1:2],
            brewer.pal(n = 9, name = "Set1")[5],
            brewer.pal(n = 11, name = "Set3")[3:5],
            brewer.pal(n = 9, name = "Set1")[6],
            brewer.pal(n = 12, name = "Paired")[1],
            brewer.pal(n = 11, name = "Set3")[6:8],
            brewer.pal(n = 9, name = "Set1")[7],
            brewer.pal(n = 11, name = "Set3")[9:11],
            brewer.pal(n = 12, name = "Paired")[2])

# 그래프 생성
df.p%>%
  ggplot(., aes(x=y, fill=value))+
  geom_bar(position='fill')+
  scale_fill_manual(values=colPal)+
  facet_wrap(~variable)+
  theme_bw()

ggsave('plot/실습_예제.png', width=200, height=100,units='mm')

# t-test
head(iris)
x <- iris$Sepal.Length[iris$Species=="setosa"]
y <- iris$Sepal.Length[iris$Species=="versicolor"]

t.test(x,y,alternative = "two.sided")

# correlation analysis
head(iris)
cor(iris[,-5])

# regression
lm(data=iris,Sepal.Length~Petal.Width+Petal.Length)


library(tidyr)
library(RColorBrewer)
df.p <- data%>%
  select(job, marital, education, default, housing, loan, y)%>%
  gather(., key='variable', value='value', job, marital, education, default, housing, loan)

# 색 정의
colPal <- c(brewer.pal(n = 9, name = "Set1")[1],
            brewer.pal(n = 9, name = "Set1")[2:4],
            brewer.pal(n = 11, name = "Set3")[1:2],
            brewer.pal(n = 9, name = "Set1")[5],
            brewer.pal(n = 11, name = "Set3")[3:5],
            brewer.pal(n = 9, name = "Set1")[6],
            brewer.pal(n = 12, name = "Paired")[1],
            brewer.pal(n = 11, name = "Set3")[6:8],
            brewer.pal(n = 9, name = "Set1")[7],
            brewer.pal(n = 11, name = "Set3")[9:11],
            brewer.pal(n = 12, name = "Paired")[2])

# 그래프 생성
df.p%>%
  ggplot(., aes(x=y, fill=value))+
  geom_bar(position='fill')+
  scale_fill_manual(values=colPal)+
  facet_wrap(~variable)+
  theme_bw()

ggsave('plot/실습_예제.png', width=200, height=100,units='mm')