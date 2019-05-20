library(chron)
library(plyr)
library(doBy)
library(reshape2)
library(dplyr)
library(readr)
library(psych)


#file format changed and saved as communities.csv

Crimes <- read_csv("C://Users//Assignments12//communities_data.csv")
str(Crimes)
View(Crimes)

names(Crimes) <- c("Case", "Number", "Date", "Block", "IUCR", "Primary Type", "Description",
                   "Location Desc", "Arrest", "Domestic", "Beat", "District", "Ward", "Community Area",
                   "FBI Code", "X Coordinate", "Y Coordinate", "Year", "Updated On", 
                   "Latitude", "Longitude", "Location")
head(Crimes)
str(Crimes)


#a. Find out top 5 attributes having highest correlation (select only Numeric features).
Crimes <- na.omit(Crimes)
names(Crimes)
c <- cor(Crimes[c(11,12,13,14,18,20,21)])
c
m <- melt(c)
m
top <- m%>%select(Var1, Var2, value)%>%filter(value != 1)
top[order(top$value, decreasing = T)[1:10],]


#b Find out top 3 reasons for having more crime in a city.
x <- as.data.frame(table(Crimes$Description))
x[order(x$Freq, decreasing = T)[1:3],]


#c Which all attributes have correlation with crime rate?

crime <- Crimes
head(crime)
table(is.na(crime))

crime$Date <- as.POSIXlt(crime$Date, format= "%m/%d/%Y %H:%M:%S")
crime$`Updated On` <- as.POSIXlt(crime$`Updated On`, format= "%m/%d/%Y %H:%M:%S")


crime$Time <- time(format(crime$Date,"%H:%M:%S"))
crime$Date <- as.POSIXct(crime$Date)
crime$`Updated On` <- as.POSIXct(crime$`Updated On`)

# crime maybe more at certain time and vice versa

time.tag <-  chron::chron(time=c("00:00:00", "06:00:00", "12:00:00", "18:00:00","23:59:00"))
time.tag
crime$time.tag <- cut(crime$Time, breaks= time.tag,
                      labels= c("00-06","06-12", "12-18", "18-00"), include.lowest =TRUE)
table(crime$time.tag)

crime$date <- as.POSIXlt(strptime(crime$Date, format = "%Y-%m-%d"))
crime$date <- as.POSIXct(crime$date)

crime$day <- as.factor(weekdays(crime$Date, abbreviate = TRUE))
crime$month <- as.factor(months(crime$Date, abbreviate = TRUE))
str(crime$day)
str(crime$month)

#changing arrest to numeric
crime$Arrest <- ifelse(as.character(crime$Arrest) == "true",1,0)

crime$crime <- as.character(crime$`Primary Type`)
crime$crime <- ifelse(crime$crime %in% c("CRIM SEXUAL ASSAULT","PROSTITUTION", "SEX OFFENSE","HUMAN TRAFFICKING"), 'SEX', crime$crime)
crime$crime <- ifelse(crime$crime %in% c("MOTOR VEHICLE THEFT"), "MVT", crime$crime)
crime$crime <- ifelse(crime$crime %in% c("GAMBLING", "INTERFEREWITH PUBLIC OFFICER", "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION",
                                         "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION",
                                         "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)","NON - CRIMINAL"),
                      "NONVIO", crime$crime)
crime$crime <- ifelse(crime$crime == "CRIMINAL DAMAGE", "DAMAGE",crime$crime)
crime$crime <- ifelse(crime$crime == "CRIMINAL TRESPASS","TRESPASS", crime$crime)
crime$crime <- ifelse(crime$crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", crime$crime)
crime$crime <- ifelse(crime$crime == "DECEPTIVE PRACTICE","FRAUD", crime$crime)
crime$crime <- ifelse(crime$crime %in% c("OTHER OFFENSE", "OTHEROFFENSE"), "OTHER", crime$crime)
crime$crime <- ifelse(crime$crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", "CONCEALED CARRY LICENSE VIOLATION","OFFENSE INVOLVING CHILDREN"), "VIO", crime$crime)
table(crime$crime)


temp <- aggregate(crime$crime, by=list(crime$crime, crime$time.tag), FUN=length)
names(temp) <- c("crime", "time.tag", "count")
temp <- ddply(crime, .(crime, day), summarise, count = length(date))


crime.agg <- ddply(crime, .(crime, Arrest, Beat, date, `X Coordinate`, `Y Coordinate`, time.tag, day, month),
                   summarise, count=length(date), .progress='text')

beats <- sort(unique(crime.agg$Beat))
dates <- sort(as.character(unique(crime.agg$date)))
temp <- expand.grid(beats, dates)
names(temp) <- c("Beat", "date")

model.data <- aggregate(crime.agg[, c('count', 'Arrest')], by=
                          list(crime.agg$Beat, as.character(crime.agg$date)), FUN=sum)
names(model.data) <- c("Beat", "date", "count", "Arrest")
model.data <- merge(temp, model.data, by= c('Beat', 'date'), all.x= TRUE)
View(model.data)
model.data$count[is.na(model.data$count)] <- 0
model.data$Arrest[is.na(model.data$Arrest)] <- 0
model.data$day <- weekdays(as.Date(model.data$date), abbreviate= TRUE)
model.data$month <- months(as.Date(model.data$date), abbreviate= TRUE)
pastDays <- function(x) {c(0, rep(1, x))}
model.data$past.crime.1 <- ave(model.data$count, model.data$Beat,
                               FUN=function(x) filter(x, pastDays(1), sides= 1))
model.data$past.crime.7 <- ave(model.data$count, model.data$Beat,
                               FUN=function(x) filter(x, pastDays(7), sides= 1))
model.data$past.crime.30 <- ave(model.data$count, model.data$Beat,
                                FUN=function(x) filter(x, pastDays(30), sides= 1))

meanNA <- function(x){mean(x, na.rm= TRUE)}
model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1),
                                  meanNA(model.data$past.crime.1), model.data$past.crime.1)
model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7),
                                  meanNA(model.data$past.crime.7), model.data$past.crime.7)
model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30),
                                   meanNA(model.data$past.crime.30), model.data$past.crime.30)
model.data$past.arrest.30 <- ave(model.data$Arrest, model.data$Beat,
                                 FUN= function(x) filter(x, pastDays(30), sides= 1))
model.data$past.arrest.30 <- ifelse(is.na(model.data$past.arrest.30),
                                    meanNA(model.data$past.arrest.30), model.data$past.arrest.30)
# number of arrests per crime
model.data$policing <- ifelse(model.data$past.crime.30 == 0, 0,
                              model.data$past.arrest.30/model.data$past.crime.30)

# trend of crime
model.data$crime.trend <- ifelse(model.data$past.crime.30 == 0, 0,
                                 model.data$past.crime.7/model.data$past.crime.30)

# season could be another reason
model.data$season <- as.factor(ifelse(model.data$month %in% c("Mar", "Apr", "May"), "spring",
                                      ifelse(model.data$month %in% c("Jun", "Jul", "Aug"), "summer",
                                             ifelse(model.data$month %in% c("Sep", "Oct","Nov"), "fall", "winter"))))

model.cor <- cor(model.data[, c("count", "past.crime.1", "past.crime.7",
                                "past.crime.30","policing", "crime.trend")])
model.cor
psych::cor.plot(model.cor)