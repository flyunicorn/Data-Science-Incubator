
# Q1
randomJump <- function(pos){
  if (pos == 0){sample(c(4,6),1)
  } else if (pos == 1){sample(c(6,8),1)
  } else if (pos == 2){sample(c(7,9),1)
  } else if (pos ==3){sample(c(4,8),1)
  } else if (pos == 4){sample(c(0,3,9),1)
  } else if (pos == 6){sample(c(0,1,7),1)
  } else if (pos == 7){sample(c(2,6),1)
  } else if (pos == 8){sample(c(1,3),1)
  } else if (pos == 9){sample(c(2,4),1)
  }}

options(digits=10)
sampleSize <- 10001
Nreps <- 10
totalAnswer <- 0
expect <- rep(0, sampleSize)
for (i in 1:sampleSize){
  oldpos <- 0
  newpos <- 0
  sumpos <- 0
  for (j in 1:Nreps){
    newpos <- randomJump(oldpos)
    sumpos <- sumpos + newpos
    oldpos <- newpos}
  expect[i] <- sumpos %% Nreps
}
mean(expect)
sd(expect)



options(digits=10)
sampleSize <- 10001
Nreps <- 1024
totalAnswer <- 0
expect <- rep(0, sampleSize)
for (i in 1:sampleSize){
  oldpos <- 0
  newpos <- 0
  sumpos <- 0
  for (j in 1:Nreps){
    newpos <- randomJump(oldpos)
    sumpos <- sumpos + newpos
    oldpos <- newpos}
  expect[i] <- sumpos %% Nreps
}
mean(expect)
sd(expect)



options(digits=10)
sampleSize <- 10001
Nreps <- 10
totalAnswer <- 0
expect <- rep(0, sampleSize)
prob1 <- rep(0, sampleSize)
prob2 <- rep(0, sampleSize)
for (i in 1:sampleSize){
  oldpos <- 0
  newpos <- 0
  sumpos <- 0
  for (j in 1:Nreps){
    newpos <- randomJump(oldpos)
    sumpos <- sumpos + newpos
    oldpos <- newpos}
  expect[i] <- sumpos %% Nreps
  prob1[i] <- sumpos %% 35
  prob2[i] <- sumpos %% 7
}
sum(prob1==0)/sum(prob2==0)


options(digits=10)
sampleSize <- 10001
Nreps <- 1024
totalAnswer <- 0
expect <- rep(0, sampleSize)
prob1 <- rep(0, sampleSize)
prob2 <- rep(0, sampleSize)
for (i in 1:sampleSize){
  oldpos <- 0
  newpos <- 0
  sumpos <- 0
  for (j in 1:Nreps){
    newpos <- randomJump(oldpos)
    sumpos <- sumpos + newpos
    oldpos <- newpos}
  expect[i] <- sumpos %% Nreps
  prob1[i] <- sumpos %% 667
  prob2[i] <- sumpos %% 29
}
sum(prob1==0)/sum(prob2==0)




# Q2
detach(package:plyr)
library(readr)
library(dplyr)
library(magrittr)
library(broom)
home <- read_csv('Historic_Secured_Property_Tax_Rolls.csv')
head(home)
which.max(table(home$`Property Class Code`)) # Dwelling is the most common class
sum(home$`Property Class Code`=='D', na.rm=T)/length(home$`Property Class Code`)


home %>% filter(`Closed Roll Assessed Improvement Value`!=0 & !is.na(`Closed Roll Assessed Improvement Value`) & !is.na(`Neighborhood Code`)) %>% 
  group_by(`Neighborhood Code`) %>% summarise(mean=mean(`Closed Roll Assessed Improvement Value`,na.rm=T)) %>% summarise(diff=max(mean)-min(mean))

home %>% filter(`Closed Roll Assessed Improvement Value`!=0 & !is.na(`Closed Roll Assessed Improvement Value`) & !is.na(`Block and Lot Number`)) %>% 
  group_by(`Block and Lot Number`) %>% filter(`Closed Roll Fiscal Year`==max(`Closed Roll Fiscal Year`,na.rm=T)) %>% 
  ungroup %>% summarise(value=median(`Closed Roll Assessed Improvement Value`))

growth <- home %>% filter(`Closed Roll Assessed Land Value`!=0 & !is.na(`Closed Roll Assessed Land Value`)) %>% 
  mutate(logP=log(`Closed Roll Assessed Land Value`)) %>% select(`Closed Roll Fiscal Year`,`logP`)
lmfit <- lm(`logP`~`Closed Roll Fiscal Year`,growth)  
tidy(lmfit)
                
area <- home %>% filter(!is.na(`Location`) & !is.na(`Neighborhood Code`)) %>% select(`Neighborhood Code`,`Location`) %>% 
  mutate(longitude=gsub('\\((.*),.(.*)\\)', '\\1', `Location`),latitude=gsub('\\((.*),.(.*)\\)', '\\2', `Location`)) %>% 
  group_by(`Neighborhood Code`) %>% summarise(a=sd(longitude)*111.321, b=sd(latitude)*111) %>% 
  mutate(area=a*b*pi) %>% arrange(desc(area))
  
ratio <- home %>% filter(!is.na(`Number of Bedrooms`) & !is.na(`Number of Units`)) %>% 
  filter(`Number of Bedrooms`!=0 & `Number of Units`!=0) %>% group_by(`Zipcode of Parcel`) %>% 
  summarise(bed=mean(`Number of Bedrooms`), unit=mean(`Number of Units`)) %>% 
  mutate(ratio=`bed`/`unit`) %>% arrange(desc(ratio)) 

newbldg <- home %>% filter(!is.na(`Year Property Built`) & !is.na(`Number of Units`) & `Number of Units`!=0) %>% 
  filter(`Year Property Built` >= 1950) %>% group_by(`Block and Lot Number`) %>% 
  filter(`Closed Roll Fiscal Year`==min(`Closed Roll Fiscal Year`,na.rm=T)) %>% ungroup %>% summarise(unit=mean(`Number of Units`))
oldbldg <- home %>% filter(!is.na(`Year Property Built`) & !is.na(`Number of Units`) & `Number of Units`!=0) %>% 
  filter(`Year Property Built` < 1950 & `Year Property Built` > 1500) %>% group_by(`Block and Lot Number`) %>% 
  filter(`Closed Roll Fiscal Year`==min(`Closed Roll Fiscal Year`,na.rm=T)) %>% ungroup %>% summarise(unit=mean(`Number of Units`))
unit_diff <- newbldg - oldbldg

prop_ratio <- home %>% filter(!is.na(`Property Area in Square Feet`) & `Property Area in Square Feet`!=0) %>% 
  filter(!is.na(`Lot Area`) & `Lot Area`!=0) %>% group_by(`Zipcode of Parcel`) %>% 
  summarise(ratio=sum(`Property Area in Square Feet`)/sum(`Lot Area`)) %>% arrange(desc(ratio)) 



# Q3 
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(XLConnect)
library(ggplot2)
library(ggmap)
library(maps)
library(geosphere)
library(magrittr)
setwd("~/Documents/Data Science Incubator")
fileNames <- paste0(as.character(order(Sys.glob("*.csv"))),'.csv')
datalist = lapply(fileNames, function(x){read.csv(x,header=T)})
sample<-do.call(rbind, datalist)
sample<-sample[,-19]

wb <- loadWorkbook("carriers.xls")
carriers = readWorksheet(wb, sheet = "carriers.csv", header=T)

wb <- loadWorkbook("airports new.xls")
airports = readWorksheet(wb, sheet = "airports.csv", header=T)

# Check on missing values + define functions + merge multiple datasets 
# Note: As function result shows that none of the NA% is over 5%, we will simply delete these rows, 
# otherwise, the MICE package will be employed for more sophisticated imputation for missing values.   
coverage <- sapply(sample, function(x) {
  coverage <- round(1 - sum(is.na(x)) / length(x),2)
})

sum(!complete.cases(sample))
fulldata<-sample[complete.cases(sample),]
fulldata$MONTH=factor(fulldata$MONTH)

# create route ID by combining origin-dest airports
fulldata$route <- paste(fulldata$ORIGIN, fulldata$DEST, sep='-')
detach(package:plyr)
frequency <- count(fulldata, route)

substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
substrLeft <- function(x, n){substr(x, 1, n)}

origin = data.frame(substrLeft(frequency$route,3))
names(origin)='air.code'
origin$originX=airports[match(origin$air.code,airports$iata),6]
origin$originY=airports[match(origin$air.code,airports$iata),7]
origin=cbind(origin,frequency)
dest = data.frame(substrRight(frequency$route,3))
names(dest)='air.code'
dest$destX=airports[match(dest$air.code,airports$iata),6]
dest$destY=airports[match(dest$air.code,airports$iata),7]
dest=cbind(dest,frequency)
names(origin)=c('air.code','X','Y','route','flights')
names(dest)=c('air.code','X','Y','route','flights')
data=rbind(origin,dest)
data$route=factor(data$route)


# Data Exploration and Visualization
# Overall data plotting to understand monthly data pulling from the website
list = sapply(datalist, function(x){nrow(x)})
plot(list, type='b', col='purple', main="Flight Frequency of 2014", xlab="Month", ylab="Flights") 
# Unsurprisingly, February is the least traveled period, whereas July and August have the most flights.

# Delve deeper into data exploration by month -- Departure delay
d2 <- fulldata %>% group_by(MONTH) %>% summarize(lower = quantile(DEP_DELAY, probs = .2),upper = quantile(DEP_DELAY, probs = .8), median = quantile(DEP_DELAY, probs = .5), average=round(mean(DEP_DELAY),2))
kable(d2)

ggplot(fulldata, aes(x = DEP_DELAY)) + facet_wrap(~MONTH, ncol=3) + geom_density(aes(fill = MONTH, colour = MONTH))+xlim(-10,30)+ geom_vline(data=d2, aes(xintercept=average, na.rm=T))+xlab('Departure Delay Minutes') + ggtitle("Departure Delay Distribution (Outliers Removed) By Month With Mean Line")
# Note: I used mid 80% percentile to avoid outlier skew. 
# The distribution for majority delay time by month has similar shape, despite variance in average line. 
# It may be worth time to experiment different metrics such as mean and median with different percentiles to derive a better understanding of the departure delay time. Due to time limit, I used simple average since it manifests wider variance between months for better visual presentation. 
# Graph shows January has the longest departure delay, while September is "on time"" relatively. Is it because the 9/11 effect makes airports and airlines staff more alert on the job so that time schedule is more respected?  
# It puzzles me from initial exploration that how come departure delay can be negative. Aren't flights not supposed to depart early but only late? It needs more time for investigation. 

# Delve deeper into data exploration by carrier -- Departure delay
d3 <- fulldata %>% group_by(UNIQUE_CARRIER) %>% summarize(lower = quantile(DEP_DELAY, probs = .2),upper = quantile(DEP_DELAY, probs = .8), median= quantile(DEP_DELAY, probs = .5), average= round(mean(DEP_DELAY),2))
d3$carrier=carriers[carriers$Code%in%unique(fulldata$UNIQUE_CARRIER),2]
options(dplyr.width = Inf)
d3[11,6]='US Airways Inc.'
kable(d3)

ggplot(fulldata, aes(x = DEP_DELAY)) + facet_wrap(~UNIQUE_CARRIER, ncol=3) + 
  scale_fill_discrete(name="Airline Carrier (Ordered)", breaks=d3$UNIQUE_CARRIER, labels=d3$carrier)+ 
  geom_density(aes(fill = UNIQUE_CARRIER)) + xlim(-10,30) + 
  geom_vline(data=d3, aes(xintercept=average, na.rm=T)) + xlab('Departure Delay Minutes') + 
  ggtitle("Departure Delay Distribution (Outliers Removed) By Carrier") 
  
# Same treatment as analysis by month. However, the distribution shape has more variance by carriers. 
# The winner for longest departure delay goes to Frontier Airlines; whereas the "real"" winner for least departure delay is US Airways. But it can be attributed to the fact that it was later merged with America West. Therefore, it is worth noting that simply comparing delay average may not be an adequate way to judge flight performance. After all, number of flights managed, flying path and airports can all be confounding variables.
# That said, I'm surprised to see Virgin America and American Airlines's performance. As a big fan for Jetblue, its number satisfies me. As a frequently delayed customer for Southwest, I obtained "more faith" from the analysis.  

# Delve deeper into data exploration by month -- Arrival delay
d4 <- fulldata %>% group_by(MONTH) %>% summarize(lower = quantile(ARR_DELAY, probs = .2),upper = quantile(ARR_DELAY, probs = .8), median = quantile(ARR_DELAY, probs = .5), average=round(mean(ARR_DELAY),2))
kable(d4)

ggplot(fulldata, aes(x = ARR_DELAY)) + facet_wrap(~MONTH, ncol=3) + geom_density(aes(fill = MONTH, colour = MONTH))+xlim(-20,40)+ geom_vline(data=d4, aes(xintercept=average, na.rm=T))+xlab('Arrival Delay Minutes') + ggtitle("Arrival Delay Distribution (Outliers Removed) By Month With Mean Line")

# Likewise, January has the longest arrival delay as departure; September has the shortest arrival delay.

# Delve deeper into data exploration by carrier -- Arrival delay
d5 <- fulldata %>% group_by(UNIQUE_CARRIER) %>% summarize(lower = quantile(ARR_DELAY, probs = .2),upper = quantile(ARR_DELAY, probs = .8), median= quantile(ARR_DELAY, probs = .5), average= round(mean(ARR_DELAY),2))
d5$carrier=carriers[carriers$Code%in%unique(fulldata$UNIQUE_CARRIER),2]
d5[11,6]='US Airways Inc.'
kable(d5)

ggplot(fulldata, aes(x = ARR_DELAY)) + facet_wrap(~UNIQUE_CARRIER, ncol=3) + geom_density(aes(fill = UNIQUE_CARRIER, colour = UNIQUE_CARRIER))+xlim(-20,50) + geom_vline(data=d5, aes(xintercept=average, na.rm=T))+xlab('Arrival Delay Minutes')+ggtitle("Arrival Delay Distribution (Outliers Removed) By Carrier")

# Again, Frontier Airlines claims championship in arrival delay. Similar rank applies to arrival delay as departure. 
# One black horse worth noting is Delta Airlines. Despite mediocre performance for departure delay, it has great arrival performance. 
# Delta seems good at catching up on air in spite of taking off late!
  
# Most popular routes - top 10 billboard
billboard=data.frame(unique(data[head(order(data$flights, decreasing =T), 40),c(4,5)]))
for (i in 1:length(substrLeft(as.character(billboard$route),3))){
  billboard$name1[i]=airports[airports$iata==substrLeft(as.character(billboard$route),3)[i],2]
}
for (i in 1:length(substrRight(as.character(billboard$route),3))){
  billboard$name2[i]=airports[airports$iata==substrRight(as.character(billboard$route),3)[i],2]
}
billboard %<>% mutate(fullname=paste(name1,name2, sep='——')) %>% select(route,flights,fullname)
kable(billboard)


# After second thought, it makes sense to me why the counts of back and forth routes don't equal since this is analysis for one year of data. The discrepancy should average out over the years. 
# So with two-way route, we see from the chart 'top 10' most popular paths in 2014. 

# Statistical t-test on two samples 
t.test(d3$average,d5$average, var.equal = T)
wilcox.test(d3$average,d5$average)


# P-value of 0.13 shows there isn't significant difference between carrier's departure and arrival delays.
# Caveat: Due to small sample size, nonparametric test like Wilcoxon (Mann-Whitney) test may be used.
# Wilcoxon test produced p-value of 0.06 which confirmed t-test result.

# Data Visualization with US Map
p=ggmap(get_googlemap(center = 'us', zoom = 4, maptype = 'roadmap'), extent = 'device') 
p+geom_point(data=airports,alpha=.3, col='deeppink', aes(x=long,y=lat,show_guide=TRUE))+ggtitle("US Airports Density With Scaled Transparency")+theme(plot.title = element_text(lineheight=3, face="bold")) 

p+geom_line(data=data, aes(x = Y, y = X, group = route), size = 0.3, alpha = 0.3, col = 'red') + geom_point(data=data, aes(x = Y, y = X, size = flights), colour = "blue", alpha = 0.3)+scale_size(range=c(0,15))+ggtitle("Flight Routes From And To New York City")+theme(plot.title = element_text(lineheight=4, face="bold")) 


