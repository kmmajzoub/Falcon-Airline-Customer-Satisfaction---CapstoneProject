setwd("C:/Users/kmmaj/Desktop/PGBA/Capstone")
flightdata = read.csv("Flight data.csv")
surveydata = read.csv("Surveydata.csv") 
class(flightdata)
class(surveydata)
dim(flightdata)
dim(surveydata)
head(flightdata)
str(flightdata)
## in "Flightdata" here I will have to fix the variables into the correct format 
flightdata$Gender = as.factor(flightdata$Gender)
flightdata$CustomerType = as.factor(flightdata$CustomerType)  
flightdata$TypeTravel = as.factor(flightdata$TypeTravel)
flightdata$Class = as.factor(flightdata$Class)

str(flightdata)
summary(flightdata)
## there are more loyal customersa than disloyal -- so results might be biased.
## there are more bsssines travel than personal 
## bussiness and eco are almost equal 
## max distance is 6950 - and 3rd Qu is 2542 
## delaying in departure 3rd qu is 12 and max is 1592 - obvious outliers 
## arrival delay 3rd qu is 12 and max is 1584.

##### Now i ll check surveydata #####
head(surveydata)
str(surveydata)
### i will have to change all variables except for id to factors
surveydata$Satisfaction = as.factor(surveydata$Satisfaction)
surveydata$Seat_comfort = as.factor(surveydata$Seat_comfort)
surveydata$Departure.Arrival.time_convenient = as.factor(surveydata$Departure.Arrival.time_convenient)
surveydata$Food_drink = as.factor(surveydata$Food_drink)
surveydata$Gate_location = as.factor(surveydata$Gate_location)
surveydata$Inflightwifi_service = as.factor(surveydata$Inflightwifi_service)
surveydata$Inflight_entertainment = as.factor(surveydata$Inflight_entertainment)
surveydata$Online_support = as.factor(surveydata$Online_support)
surveydata$Ease_of_Onlinebooking = as.factor(surveydata$Ease_of_Onlinebooking)
surveydata$Onboard_service = as.factor(surveydata$Onboard_service)
surveydata$Leg_room_service = as.factor(surveydata$Leg_room_service)
surveydata$Baggage_handling = as.factor(surveydata$Baggage_handling)
surveydata$Checkin_service = as.factor(surveydata$Checkin_service)
surveydata$Cleanliness = as.factor(surveydata$Cleanliness)
surveydata$Online_boarding = as.factor(surveydata$Online_boarding)

str(surveydata)
summary(surveydata)

### Merging data sets.
fli_sur= merge(flightdata,surveydata, by.x = "ID", 
               by.y = "Id", all.x = TRUE, all.y =TRUE)

str(fli_sur)

fli_sur = fli_sur[,-1]
### we have missing values in arrival time and food drink and onboard service I will have to fill
anyNA(fli_sur)
## we have NA in arrival delayin 
sum(is.na(fli_sur))
284/90806
sapply(fli_sur,function(x) sum(is.na(x)))
## 0.003 will not affect the our predictions so will have to get rid of it.
flightdata = na.omit(flightdata)
fli_sur = na.omit(fli_sur)
## as shown the flightdata obs decreased to 90633 
anyNA(fli_sur)

## Now I will have to fill the dashes 
fli_sur$CustomerType = as.character(fli_sur$CustomerType)
fli_sur$CustomerType[fli_sur$CustomerType==''] = 'neutral'
fli_sur$CustomerType = as.factor(fli_sur$CustomerType)

fli_sur$Departure.Arrival.time_convenient = as.character(fli_sur$Departure.Arrival.time_convenient)
fli_sur$Departure.Arrival.time_convenient[fli_sur$Departure.Arrival.time_convenient==''] = 'neutral'
fli_sur$Departure.Arrival.time_convenient = as.factor(fli_sur$Departure.Arrival.time_convenient)

fli_sur$Food_drink = as.character(fli_sur$Food_drink)
fli_sur$Food_drink[fli_sur$Food_drink==''] = 'neutral'
fli_sur$Food_drink = as.factor(fli_sur$Food_drink)


fli_sur$Onboard_service = as.character(fli_sur$Onboard_service)
fli_sur$Onboard_service[fli_sur$Onboard_service==''] = 'neutral'
fli_sur$Onboard_service = as.factor(fli_sur$Onboard_service)

### now the data is ready , i will start with uni analysis:
colnames(fli_sur)
attach(fli_sur)

#EDA

## Univariate   
par(mfrow= c(2,2))

hist(Age)
hist(Flight_Distance)
hist(DepartureDelayin_Mins)
hist(ArrivalDelayin_Mins)
## age is normal distrubution and it is spread 
## flight distance is also normal distrubution with outliers for long flights 
## departure and arrivals delays as expected mostly in the first bin where delays at minumum , some flights have long delays 
## since customer satisfaction is the main variable , it is expected that long delays will unsatisfy the customers, we will know 

plot(density(Age))
plot(density(Flight_Distance))
plot(density(DepartureDelayin_Mins))
plot(density(ArrivalDelayin_Mins))
## age is almost a normal distrubution 
## flight distance also a normal distrubution with little skewness duw to outliers
## departure and arrival delays are skewed 

boxplot(Age, xlab = "age", horizontal = TRUE)
boxplot(Flight_Distance , xlab= "flight distance", horizontal = TRUE)
boxplot(DepartureDelayin_Mins, xlab = "departure delay mins", horizontal = TRUE)
boxplot(ArrivalDelayin_Mins, xlab = "arrival delay mins", horizontal = TRUE)
## age is normal without outliers 
## flight distance have a lot of outliers points
## delays in mins have outliers because the mean is close to zero as it should be.
## I will not exclude the outliers for flight distance , I dont think it is relative to customer satisfaction because they know the long distance
## as for outliers in delays either departure or arrival , 

par(mfrow= c(2,5))
plot(Gender, xlab= "gender")
## females are more the males , almost equal 
plot(CustomerType, xlab = "customer type")
## we have loyal customers more than disloyal
plot(TypeTravel, xlab = "typetravel")
## business travel is more than personal 
plot(Class, xlab = "class")
## business class and eco is almost equal
plot(Satisfaction, xlab = "satisfaction")
## there are more satisfied customers than neutral or disatisfied 
plot(Seat_comfort, xlab = "seat comfort")
## more into positive is more than poor 
plot(Departure.Arrival.time_convenient, xlab = "departure arival time convenient")
## extremely poor is little 
plot(Food_drink, xlab = "food & drink")
## the food n drink looks not that well -- it may affects the satisfaction 
plot(Gate_location, xlab = "gate location")
## positive and negative feedback almost equal 
plot(Inflightwifi_service, xlab = "inflight wifi")
## the wifi more into positive feedback 
plot(Inflight_entertainment, xlab = "infloight entertainment")
## entertainmet more into positive feedback 
plot(Online_support, xlab = "online support")
## online support more into positive feedback 
plot(Ease_of_Onlinebooking,xlab = "ease of online booking")
## ease of online more into positive 
plot(Onboard_service , xlab = "onboard service")
## onboard service more into negative 
plot(Leg_room_service, xlab = "leg room ")
## leg room more into positive 
plot(Baggage_handling, xlab = "baggage handling")
## handling baggages is more into positive 
plot(Checkin_service, xlab = "checkin service")
## check in is more into positive 
plot(Cleanliness, xlab = "cleanliness")
## cleanliness is more into positive 
plot(Online_boarding, xlab = "online boarding")
## online boarding is more into positive 

### now some corelations between variables - Bivariate 
par(mfrow= c(2,2))
## numeric & categorical 
plot(Satisfaction,Age, ylab = "satisfaction" , xlab = "age", horizontal = TRUE)
## satisfied and disatisfied are spread all over the ages 
plot(Satisfaction,Flight_Distance, ylab = "satisfaction" , xlab = "Flight Distance", horizontal = TRUE)
## satisfied majority more than disatisfied 
plot(Satisfaction,ArrivalDelayin_Mins, ylab = "satisfaction", xlab = "Arrival Delay", horizontal = TRUE)
## Majority distatisfied due to arrival delays 
plot(Satisfaction,DepartureDelayin_Mins,ylab = "satisfaction" , xlab = "Departure Delay", horizontal = TRUE)
## Same as arrival , more disatisfed than satisfied 

hist(Flight_Distance[Satisfaction == "satisfied"])
hist(Flight_Distance[Satisfaction == "neutral or dissatisfied"])
## both normal distrubution 
hist(Age[Satisfaction == "satisfied"])
hist(Age[Satisfaction == "neutral or dissatisfied"])
## also as in age , normal distrubution 
hist(ArrivalDelayin_Mins[Satisfaction == "satisfied"])
hist(ArrivalDelayin_Mins[Satisfaction == "neutral or dissatisfied"])
## same distrubution 
hist(DepartureDelayin_Mins[Satisfaction == "satisfied"])
hist(DepartureDelayin_Mins[Satisfaction == "neutral or dissatisfied"])
## same distrubution 

## 2 numerics
par(mfrow= c(1,1))
plot(Age,Flight_Distance)
abline(lm(Age~Flight_Distance), col=c("red"))
## there is not any significant correlations 
plot(Flight_Distance,DepartureDelayin_Mins)
abline(lm(Flight_Distance~DepartureDelayin_Mins), col=c("red"))
## the longer the flight distance the more the departure 
## departure delays has something to do with long distances 
plot(Flight_Distance,ArrivalDelayin_Mins)
abline(lm(Flight_Distance~ArrivalDelayin_Mins), col=c("red"))
## the longer the flight the more delays in arrivals 
plot(Age,DepartureDelayin_Mins)
abline(lm(Age~DepartureDelayin_Mins), col=c("red"))
## not any correlations 
## it has nothing to do with arrival as well , obvious 

## Pivot table 
library(rpivotTable)
rpivotTable(fli_sur)


## Corplot
par(mfrow= c(1,1))
library(corrplot)
corrplot(cor(fli_sur[,c(3,6,7,8)]))
## negative and little correlation between age and flight distance 
## Positive and strong corelation between delays departure and arrivals.
## positive and little correlation between flight distance and departure and arrival 
cor(fli_sur[,c(3,6,7,8)])

par(mfrow= c(1,3))
plot(aggregate(Flight_Distance~Satisfaction,data = fli_sur,sum), type="b")
## more satisfied in longer flights than disatsified 
plot(aggregate(ArrivalDelayin_Mins~Satisfaction,data = fli_sur,sum), type="b")
## more disatisified for delays than satisifed 
plot(aggregate(DepartureDelayin_Mins~Satisfaction,data = fli_sur,sum), type="b")
## more disatisifed in delays than satisifed 

### now we have full understanding of the data and all variables , corelations.

remove(flightdata)
remove(surveydata)
fli_sur2 = fli_sur
anyNA(fli_sur2)

############################                                 ##############################
############################   Removal of unwanted variables ##############################
############################                                 ##############################

## when i tried KNN for 24 variables it took a lot to load , then error , i will reduce as much as i can 
## I will remove based on my intuition , all variables that will not affect the main objective which is customer satisfaction 
## i will keep only : departure delay , arrival delay , satisfaction, time convenient, food drinks, wifi, enter,online , ease, onboard , baggage, check in,cleaniss, online boarding)
## total variables = 14 

fli_sur2 = fli_sur2[-c(1,2,3,4,5,6,10,13,19)]
library(summarytools)

View(dfSummary(fli_sur2))
summary(fli_sur2)

############################                                 ##############################
############################   Missing values treatment      ##############################
############################                                 ##############################
## I will improvise and name the blanks based on my intuition 
fli_sur2$Departure.Arrival.time_convenient = as.character(fli_sur2$Departure.Arrival.time_convenient)
fli_sur2$Departure.Arrival.time_convenient[fli_sur2$Departure.Arrival.time_convenient=='NaN'] = "neutral"
fli_sur2$Departure.Arrival.time_convenient = as.factor(fli_sur2$Departure.Arrival.time_convenient)

fli_sur2$Food_drink = as.character(fli_sur2$Food_drink)
fli_sur2$Food_drink[fli_sur2$Food_drink=='NaN'] = "neutral"
fli_sur2$Food_drink = as.factor(fli_sur2$Food_drink)

fli_sur2$Onboard_service = as.character(fli_sur2$Onboard_service)
fli_sur2$Onboard_service[fli_sur2$Onboard_service=='NaN'] = "neutral"
fli_sur2$Onboard_service = as.factor(fli_sur2$Onboard_service)
fli_sur2 = fli_sur2[c(1:14)]
summary(fli_sur2)
str(fli_sur2)

############################                       ##############################
############################   Outlier Treatment   ##############################
############################                       ##############################

fli_sur2.scaled = scale(fli_sur2[,1:2])
fli_sur2.scaled
seed = 1
set.seed(seed)
clust = kmeans(x=fli_sur2.scaled, centers = 7, nstart = 5)
clust
library(cluster)
par(mfrow= c(1,1))
clusplot(fli_sur2.scaled, clust$cluster, color = TRUE, shade = TRUE, labels = 2 , lines = 1)


library(NbClust)
set.seed(seed)
nc= NbClust(fli_sur2.scaled , min.nc = 5, max.nc = 10, method = "kmeans")
## Error: cannot allocate vector of size 30.6 Gb
## I couldnt fix the memory issue 
## I will try on testing till i find the optimum numbers 
####### after testing 7 centers gave me the best solution #######

fli_sur2$cluster = clust$cluster
head(fli_sur2)
str(fli_sur2)
fli_sur2$cluster = as.factor(fli_sur2$cluster)

summary(fli_sur2$cluster)
## I will keep only labels 1&2
fli_sur2_clean = fli_sur2[fli_sur2$cluster == "2"  |
                            fli_sur2$cluster == "1" , ] 


summary(fli_sur2_clean)
str(fli_sur2_clean)
anyNA(fli_sur2_clean)
par(mfrow= c(1,2))
boxplot(fli_sur2_clean$DepartureDelayin_Mins,fli_sur2_clean$ArrivalDelayin_Mins , xlab = "after treatment")
boxplot(fli_sur2$DepartureDelayin_Mins, fli_sur2$ArrivalDelayin_Mins, xlab = "before treatment")

### as shown we reduced the outliers a lot #####

## Now standardizing the names
library(janitor)
fli_sur2_clean = clean_names(fli_sur2_clean)

############################                             ##############################
############################   Variable Transformation   ##############################
############################                             ##############################


## having many factor levels , I will group them and make them 2 levels only 
## excellent, good , acceptable will be grouped to positive
## extremely poor , poor, need improvment and  neutral will be  negative 

#positive
fli_sur2_clean$departure_arrival_time_convenient = as.character(fli_sur2_clean$departure_arrival_time_convenient)
fli_sur2_clean$departure_arrival_time_convenient[fli_sur2_clean$departure_arrival_time_convenient=='excellent'] = "positive"
fli_sur2_clean$departure_arrival_time_convenient[fli_sur2_clean$departure_arrival_time_convenient=='good'] = "positive"
fli_sur2_clean$departure_arrival_time_convenient[fli_sur2_clean$departure_arrival_time_convenient=='acceptable'] = "positive"
# negative
fli_sur2_clean$departure_arrival_time_convenient[fli_sur2_clean$departure_arrival_time_convenient=='extremely poor'] = "negative"
fli_sur2_clean$departure_arrival_time_convenient[fli_sur2_clean$departure_arrival_time_convenient=='poor'] = "negative"
fli_sur2_clean$departure_arrival_time_convenient[fli_sur2_clean$departure_arrival_time_convenient=='need improvement'] = "negative"
fli_sur2_clean$departure_arrival_time_convenient[fli_sur2_clean$departure_arrival_time_convenient=='neutral'] = "negative"
fli_sur2_clean$departure_arrival_time_convenient = as.factor(fli_sur2_clean$departure_arrival_time_convenient)


#positive
fli_sur2_clean$food_drink = as.character(fli_sur2_clean$food_drink)
fli_sur2_clean$food_drink[fli_sur2_clean$food_drink=='excellent'] = "positive"
fli_sur2_clean$food_drink[fli_sur2_clean$food_drink=='good'] = "positive"
fli_sur2_clean$food_drink[fli_sur2_clean$food_drink=='acceptable'] = "positive"
# negative
fli_sur2_clean$food_drink[fli_sur2_clean$food_drink=='extremely poor'] = "negative"
fli_sur2_clean$food_drink[fli_sur2_clean$food_drink=='poor'] = "negative"
fli_sur2_clean$food_drink[fli_sur2_clean$food_drink=='need improvement'] = "negative"
fli_sur2_clean$food_drink[fli_sur2_clean$food_drink=='neutral'] = "negative"
fli_sur2_clean$food_drink = as.factor(fli_sur2_clean$food_drink)

#positive
fli_sur2_clean$inflightwifi_service = as.character(fli_sur2_clean$inflightwifi_service)
fli_sur2_clean$inflightwifi_service[fli_sur2_clean$inflightwifi_service=='excellent'] = "positive"
fli_sur2_clean$inflightwifi_service[fli_sur2_clean$inflightwifi_service=='good'] = "positive"
fli_sur2_clean$inflightwifi_service[fli_sur2_clean$inflightwifi_service=='acceptable'] = "positive"
# negative
fli_sur2_clean$inflightwifi_service[fli_sur2_clean$inflightwifi_service=='extremely poor'] = "negative"
fli_sur2_clean$inflightwifi_service[fli_sur2_clean$inflightwifi_service=='poor'] = "negative"
fli_sur2_clean$inflightwifi_service[fli_sur2_clean$inflightwifi_service=='need improvement'] = "negative"
fli_sur2_clean$inflightwifi_service[fli_sur2_clean$inflightwifi_service=='neutral'] = "negative"
fli_sur2_clean$inflightwifi_service = as.factor(fli_sur2_clean$inflightwifi_service)

#positive
fli_sur2_clean$inflight_entertainment = as.character(fli_sur2_clean$inflight_entertainment)
fli_sur2_clean$inflight_entertainment[fli_sur2_clean$inflight_entertainment=='excellent'] = "positive"
fli_sur2_clean$inflight_entertainment[fli_sur2_clean$inflight_entertainment=='good'] = "positive"
fli_sur2_clean$inflight_entertainment[fli_sur2_clean$inflight_entertainment=='acceptable'] = "positive"
# negative
fli_sur2_clean$inflight_entertainment[fli_sur2_clean$inflight_entertainment=='extremely poor'] = "negative"
fli_sur2_clean$inflight_entertainment[fli_sur2_clean$inflight_entertainment=='poor'] = "negative"
fli_sur2_clean$inflight_entertainment[fli_sur2_clean$inflight_entertainment=='need improvement'] = "negative"
fli_sur2_clean$inflight_entertainment[fli_sur2_clean$inflight_entertainment=='neutral'] = "negative"
fli_sur2_clean$inflight_entertainment = as.factor(fli_sur2_clean$inflight_entertainment)

#positive
fli_sur2_clean$online_support = as.character(fli_sur2_clean$online_support)
fli_sur2_clean$online_support[fli_sur2_clean$online_support=='excellent'] = "positive"
fli_sur2_clean$online_support[fli_sur2_clean$online_support=='good'] = "positive"
fli_sur2_clean$online_support[fli_sur2_clean$online_support=='acceptable'] = "positive"
# negative
fli_sur2_clean$online_support[fli_sur2_clean$online_support=='extremely poor'] = "negative"
fli_sur2_clean$online_support[fli_sur2_clean$online_support=='poor'] = "negative"
fli_sur2_clean$online_support[fli_sur2_clean$online_support=='need improvement'] = "negative"
fli_sur2_clean$online_support = as.factor(fli_sur2_clean$online_support)

#positive
fli_sur2_clean$ease_of_onlinebooking = as.character(fli_sur2_clean$ease_of_onlinebooking)
fli_sur2_clean$ease_of_onlinebooking[fli_sur2_clean$ease_of_onlinebooking=='excellent'] = "positive"
fli_sur2_clean$ease_of_onlinebooking[fli_sur2_clean$ease_of_onlinebooking=='good'] = "positive"
fli_sur2_clean$ease_of_onlinebooking[fli_sur2_clean$ease_of_onlinebooking=='acceptable'] = "positive"
# negative
fli_sur2_clean$ease_of_onlinebooking[fli_sur2_clean$ease_of_onlinebooking=='extremely poor'] = "negative"
fli_sur2_clean$ease_of_onlinebooking[fli_sur2_clean$ease_of_onlinebooking=='poor'] = "negative"
fli_sur2_clean$ease_of_onlinebooking[fli_sur2_clean$ease_of_onlinebooking=='need improvement'] = "negative"
fli_sur2_clean$ease_of_onlinebooking = as.factor(fli_sur2_clean$ease_of_onlinebooking)

#positive
fli_sur2_clean$onboard_service = as.character(fli_sur2_clean$onboard_service)
fli_sur2_clean$onboard_service[fli_sur2_clean$onboard_service=='excellent'] = "positive"
fli_sur2_clean$onboard_service[fli_sur2_clean$onboard_service=='good'] = "positive"
fli_sur2_clean$onboard_service[fli_sur2_clean$onboard_service=='acceptable'] = "positive"
# negative
fli_sur2_clean$onboard_service[fli_sur2_clean$onboard_service=='extremely poor'] = "negative"
fli_sur2_clean$onboard_service[fli_sur2_clean$onboard_service=='poor'] = "negative"
fli_sur2_clean$onboard_service[fli_sur2_clean$onboard_service=='need improvement'] = "negative"
fli_sur2_clean$onboard_service[fli_sur2_clean$onboard_service=='neutral'] = "negative"
fli_sur2_clean$onboard_service = as.factor(fli_sur2_clean$onboard_service)


#positive
fli_sur2_clean$baggage_handling = as.character(fli_sur2_clean$baggage_handling)
fli_sur2_clean$baggage_handling[fli_sur2_clean$baggage_handling=='excellent'] = "positive"
fli_sur2_clean$baggage_handling[fli_sur2_clean$baggage_handling=='good'] = "positive"
fli_sur2_clean$baggage_handling[fli_sur2_clean$baggage_handling=='acceptable'] = "positive"
# negative
fli_sur2_clean$baggage_handling[fli_sur2_clean$baggage_handling=='poor'] = "negative"
fli_sur2_clean$baggage_handling[fli_sur2_clean$baggage_handling=='need improvement'] = "negative"
fli_sur2_clean$baggage_handling = as.factor(fli_sur2_clean$baggage_handling)

#positive
fli_sur2_clean$checkin_service = as.character(fli_sur2_clean$checkin_service)
fli_sur2_clean$checkin_service[fli_sur2_clean$checkin_service=='excellent'] = "positive"
fli_sur2_clean$checkin_service[fli_sur2_clean$checkin_service=='good'] = "positive"
fli_sur2_clean$checkin_service[fli_sur2_clean$checkin_service=='acceptable'] = "positive"
# negative
fli_sur2_clean$checkin_service[fli_sur2_clean$checkin_service=='extremely poor'] = "negative"
fli_sur2_clean$checkin_service[fli_sur2_clean$checkin_service=='poor'] = "negative"
fli_sur2_clean$checkin_service[fli_sur2_clean$checkin_service=='need improvement'] = "negative"
fli_sur2_clean$checkin_service = as.factor(fli_sur2_clean$checkin_service)


#positive
fli_sur2_clean$cleanliness = as.character(fli_sur2_clean$cleanliness)
fli_sur2_clean$cleanliness[fli_sur2_clean$cleanliness=='excellent'] = "positive"
fli_sur2_clean$cleanliness[fli_sur2_clean$cleanliness=='good'] = "positive"
fli_sur2_clean$cleanliness[fli_sur2_clean$cleanliness=='acceptable'] = "positive"
# negative
fli_sur2_clean$cleanliness[fli_sur2_clean$cleanliness=='extremely poor'] = "negative"
fli_sur2_clean$cleanliness[fli_sur2_clean$cleanliness=='poor'] = "negative"
fli_sur2_clean$cleanliness[fli_sur2_clean$cleanliness=='need improvement'] = "negative"
fli_sur2_clean$cleanliness = as.factor(fli_sur2_clean$cleanliness)

#positive
fli_sur2_clean$online_boarding = as.character(fli_sur2_clean$online_boarding)
fli_sur2_clean$online_boarding[fli_sur2_clean$online_boarding=='excellent'] = "positive"
fli_sur2_clean$online_boarding[fli_sur2_clean$online_boarding=='good'] = "positive"
fli_sur2_clean$online_boarding[fli_sur2_clean$online_boarding=='acceptable'] = "positive"
# negative
fli_sur2_clean$online_boarding[fli_sur2_clean$online_boarding=='extremely poor'] = "negative"
fli_sur2_clean$online_boarding[fli_sur2_clean$online_boarding=='poor'] = "negative"
fli_sur2_clean$online_boarding[fli_sur2_clean$online_boarding=='need improvement'] = "negative"
fli_sur2_clean$online_boarding = as.factor(fli_sur2_clean$online_boarding)

fli_sur2_clean = fli_sur2_clean[c(1:14)]
summary(fli_sur2_clean)

############################         ##############################
############################   EDA   ##############################
############################         ##############################

### now the data is ready , i will start with uni analysis:
attach(fli_sur2_clean)

## Univariate 
par(mfrow= c(2,2))


hist(departure_delayin_mins)
hist(arrival_delayin_mins)
boxplot(departure_delayin_mins, xlab = "departure delay mins", horizontal = TRUE)
boxplot(arrival_delayin_mins, xlab = "arrival delay mins", horizontal = TRUE)


par(mfrow= c(2,6))
plot(satisfaction, xlab = "satisfaction")
## there are more satisfied customers than neutral or disatisfied 
plot(departure_arrival_time_convenient, xlab = "departure arival time convenient")
## more positive than negative
plot(food_drink, xlab = "food & drink")
## more positive than negative
plot(inflightwifi_service, xlab = "inflight wifi")
## more positive than negative
plot(inflight_entertainment, xlab = "infloight entertainment")
## more positive than negative
plot(online_support, xlab = "online support")
## more positive than negative
plot(ease_of_onlinebooking,xlab = "ease of online booking")
## more positive than negative
plot(onboard_service , xlab = "onboard service")
## more positive than negative
plot(baggage_handling, xlab = "baggage handling")
## more positive than negative
plot(checkin_service, xlab = "checkin service")
## more positive than negative
plot(cleanliness, xlab = "cleanliness")
## more positive than negative
plot(online_boarding, xlab = "online boarding")
## more positive than negative

plot(table(fli_sur2_clean$satisfaction,fli_sur2_clean$food_drink))
2846/7966

2446/0.263

table(fli_sur2_clean$satisfaction,fli_sur2_clean$food_drink)
17759/(17759+18282    )
25562/(25562+19874    )  

### Bivariate 
par(mfrow= c(2,1))

## numeric & categorical 
plot(satisfaction,arrival_delayin_mins, ylab = "satisfaction", xlab = "Arrival Delay", horizontal = TRUE)
## Majority distatisfied due to arrival delays 
plot(satisfaction,departure_delayin_mins, ylab = "satisfaction" , xlab = "Departure Delay", horizontal = TRUE)
## Same as arrival , more disatisfed than satisfied 


## 2 numerics
plot(arrival_delayin_mins,departure_delayin_mins)
abline(lm(arrival_delayin_mins~departure_delayin_mins), col=c("red"))
## positive correlation
plot(departure_delayin_mins,arrival_delayin_mins)
abline(lm(departure_delayin_mins~arrival_delayin_mins), col=c("red"))
## positive correlation

## Corplot
library(corrplot)
par(mfrow= c(1,1))
corrplot(cor(fli_sur2_clean[,c(1,2)]))
## Positive and strong corelation between delays departure and arrivals.
cor(fli_sur2_clean[,c(1,2)])

library(DataExplorer)
plot_correlation(fli_sur2,maxcat = 6)
## this is hard to analyze , the 2 factors is better 
plot_correlation(fli_sur2_clean[,c(3,5)],maxcat = 2)
## digging in this we conclude:
## in regards to satisfaction:
## 1- low correlation between departure and arrival delays in mins
## 2- zero correlation between departure arrival time convinent and satisfaction 
## 3- food and satisfacton when food is negative satisfaction goes down 
## 4- also it goes to wifi , entertainment , online support , onlinebooking, onboard service , baggages, check in , cleanlines ,online boarding 
## the most important is entertainment, ease of online booking  then online boarding as shown in the plot 

### distrubution check 
mean(departure_delayin_mins)
mean(arrival_delayin_mins)
sd(departure_delayin_mins)
sd(arrival_delayin_mins)

############################                         ##############################
############################   Modeling Process     ##############################
############################                         ##############################


## data partitioning 
library(caTools)
set.seed(seed)
spl = sample.split(fli_sur2_clean$satisfaction, SplitRatio = 0.7)
train = subset(fli_sur2_clean, spl == T)
test = subset(fli_sur2_clean, spl == F)

summary(train)
summary(test)

################################          ###########################
################################    CART  ###########################
################################          ###########################

library(rpart.plot)  
library(rpart)

train.cart = train

##### we start with train data
colnames(train.cart)
## creating the first cart model - because we have 57000 row , i will go for 50 bucket and 0 cp
train.tree = rpart(formula = satisfaction ~ . , data = train.cart, method = "class" , minbucket = 50, cp = 0 )
train.tree
rpart.plot(train.tree)
printcp(train.tree)

## complexity parameter in xerror lowest at point 22 xerror = 0.48920 , cp = 1.7837e-04 , to verify
plotcp(train.tree)
## as shown at cp 23 is best - so we prune and i iwll increase minbucket to 3000
train.tree = rpart(formula = satisfaction ~ . , data = train.cart, method = "class" , minbucket = 3000, cp = 0 )
p.train.tree = prune(train.tree, cp= 1.7837e-04,"CP")
printcp(p.train.tree)
rpart.plot(p.train.tree)
path.rpart(p.train.tree,3)
## entertainment is the most variable in satisfaction in this cart it gave us 56% satisifed , increasing the entrtainment.
## second ease of online booking is the second most important , 74% have positive feedback from this variable, it is easy to be enhanced with low cost 
## third checkin service and can be enhanced easily 

### decrease to 2000
train.tree = rpart(formula = satisfaction ~ . , data = train.cart, method = "class" , minbucket = 2000, cp = 0 )
p.train.tree = prune(train.tree, cp= 1.7837e-04,"CP")
printcp(p.train.tree)
rpart.plot(p.train.tree)
## here we added the 4th variable , which is delays in arrival, it has 4% from the disatisfied , and since it is very hard to fix it, we dont work on it.
## because it is not applicable and costy 


### evaluate the model 

## predection 
train.cart$predection = predict(p.train.tree , data = train.cart , type = "class")
train.cart$score = predict(p.train.tree, data = train.cart, type = "prob")
head(train.cart)

## confusion matrix
library(caret)
confusionMatrix(train.cart$satisfaction,train.cart$predection) 

tbl.train.cart = table(train.cart$satisfaction, train.cart$predection)
tbl.train.cart

## error rate
print((tbl.train.cart[1,2]+tbl.train.cart[2,1])/57034)

### ROC
library(rattle)
library(ROCR)
pred.train.cart = prediction(train.cart$score[,2],train.cart$satisfaction )
perf.train.cart = performance(pred.train.cart,"tpr", "fpr")
plot(perf.train.cart,main = "ROC curve Train")

## AUC
auc.train.cart = performance(pred.train.cart,"auc")
auc.train.cart = as.numeric(auc.train.cart@y.values)
auc.train.cart

## KS
ks.train.cart = max(perf.train.cart@y.values[[1]]-perf.train.cart@x.values[[1]])
ks.train.cart

## gini
library(ineq)
gini.train.cart = ineq(train.cart$score, "gini")
gini.train.cart

## concordance
library(InformationValue)  
Concordance(actuals = train.cart$satisfaction,predictedScores = train.cart$score)

## checking errors
with(train.cart, table(satisfaction,predection))
nrow(train.cart)

######################  scoring Test sample and validating the same #######################################3


test.cart = test

## creating the first cart model - because we have 57000 row , i will go for 50 bucket and 0 cp
test.tree = rpart(formula = satisfaction ~ . , data = test.cart, method = "class" , minbucket = 50, cp = 0 )
test.tree
rpart.plot(test.tree)
printcp(test.tree)


## complexity parameter in xerror lowest at point 13 xerror = 0.49084  , cp = 6.4743e-04 , to verify
plotcp(test.tree)
## so we prune and i iwll increase minbucket to 2000
test.tree = rpart(formula = satisfaction ~ . , data = test.cart, method = "class" , minbucket = 1000, cp = 0 )
p.test.tree = prune(test.tree, cp= 6.4743e-04,"CP")
printcp(p.test.tree)
rpart.plot(p.test.tree)
path.rpart(p.test.tree,3)

### evaluate the model 

## predection 
test.cart$predection = predict(p.test.tree , data = test.cart , type = "class")
test.cart$score = predict(p.test.tree, data = test.cart, type = "prob")
head(test.cart)

## confusion matrix

confusionMatrix(test.cart$satisfaction,test.cart$predection) 
#Warning message:
#In Ops.factor(predictedScores, threshold) : '<' not meaningful for factors

tbl.test.cart = table(test.cart$satisfaction, test.cart$predection)
tbl.test.cart

## error rate
print((tbl.test.cart[1,2]+tbl.test.cart[2,1])/24443)

### ROC
pred.test.cart = prediction(test.cart$score[,2],test.cart$satisfaction )
perf.test.cart = performance(pred.test.cart,"tpr", "fpr")
plot(perf.test.cart,main = "ROC curve Test")

## AUC
auc.test.cart = performance(pred.test.cart,"auc")
auc.test.cart = as.numeric(auc.test.cart@y.values)
auc.test.cart

## checking errors
with(test.cart, table(satisfaction,predection))
nrow(test.cart)

######################################################################### comparing train and test 


rpart.plot(p.train.tree)
rpart.plot(p.test.tree)

path.rpart(p.train.tree,3)
path.rpart(p.test.tree,3)

with(train.cart, table(satisfaction,predection))
with(test.cart, table(satisfaction,predection))

print((tbl.train.cart[1,2]+tbl.train.cart[2,1])/57034)
print((tbl.test.cart[1,2]+tbl.test.cart[2,1])/24443)

plot(perf.train.cart,main = "ROC curve")
plot(perf.test.cart,main = "ROC curve")

auc.train.cart
auc.test.cart

confusionMatrix(train.cart$satisfaction,train.cart$predection) 
confusionMatrix(test.cart$satisfaction,test.cart$predection) 


################################                   ###########################
################################    Random Forest  ###########################
################################                   ###########################


### creating the forest using train dataset 
library(randomForest)
set.seed(seed)
train.rndf = train

train.rndforest = randomForest(satisfaction ~ ., data = train.rndf, ntree = 100 , mtry = 4 ,
                               nodesize = 10 , importance = TRUE)
print(train.rndforest)
## OOB 21% 
print(train.rndforest$err.rate)
plot(train.rndforest, main = "train rndforest")
## as shown 51 forest is more than enough 
importance(train.rndforest)
#### the highest variable in importance is
# 1- inflight entertainmanet 
# 2- food n drink
# 3- checkin servioce
# 4- easeofonlinebooking 
# 5- baggage handling
# 6- online support 
# 7- cleaniness
# 8- arrival delay mins
# 9- inflight wifi 
# 10 - onlineboarding
# 11- onboard service
# 12- departure arrival convenient 
# 13- departure delays

## tuning 
set.seed(seed)
t.train.rndforest = tuneRF(x = train.rndf[,-3], y = train.rndf$satisfaction ,mtryStart = 10 , stepFactor = 1.5, ntreeTry = 51,
                           improve = 0.0001, nodesize = 10, trace = TRUE,plot = TRUE,doBest = TRUE, importance = TRUE, main = "mtry train")

## build a refined forest ####
r.train.rndforest = randomForest(satisfaction ~ ., data = train.rndf, ntree = 51 , mtry = 4 ,
                                 nodesize = 10 , importance = TRUE)
r.train.rndforest

table(train.rndf$food_drink)
## almost 50% had negative feedback on food , enhancing the food n drinks is not very costy , with little amount might increase the satisfaction 

### evaluate the model 
r.train.rndforest$predict.class = predict(r.train.rndforest,train.rndf , type = "class")
r.train.rndforest$prob = predict(r.train.rndforest,train.rndf , type = "prob")
head(r.train.rndforest)
r.tbl.train.rndf = table(train.rndf$satisfaction, r.train.rndforest$predict.class)
r.tbl.train.rndf

##error rate
print((r.tbl.train.rndf[1,2]+r.tbl.train.rndf[2,1])/57034)
## very high 20% 
## we couldnt refine it more 
## the best reason maybe because of have the last outliers I coudlnt remove , made the model with high error rate.

## accuracy
print((r.tbl.train.rndf[1,1]+r.tbl.train.rndf[2,2])/57034)

## sensitivity 
print((r.tbl.train.rndf[1,1])/(r.tbl.train.rndf[1,1]+r.tbl.train.rndf[2,1]))

# specifity 
print((r.tbl.train.rndf[2,2])/(r.tbl.train.rndf[2,2]+r.tbl.train.rndf[1,2]))

## ROC
pred.train.rndf = prediction(r.train.rndforest$prob[,2],train.rndf$satisfaction )
perf.train.rndf = performance(pred.train.rndf,"tpr", "fpr")
plot(perf.train.rndf,main = "ROC TRAIN curve")

## AUC
auc.train.rndf = performance(pred.train.rndf,"auc")
auc.train.rndf = as.numeric(auc.train.rndf@y.values)
auc.train.rndf

## KS
ks.train.rndf = max(perf.train.rndf@y.values[[1]]-perf.train.rndf@x.values[[1]])
ks.train.rndf

## gini
gini.train.rndf = ineq(r.train.rndforest$prob, "gini")
gini.train.rndf

## concordance
Concordance(actuals = train.rndf$satisfaction,predictedScores = r.train.rndforest$prob)

confusionMatrix(train.rndf$satisfaction,as.factor(train.rndf$predict.class))


############################### now we use test dataset ########################
set.seed(seed)
test.rndf = test

test.rndforest = randomForest(satisfaction ~ ., data = test.rndf, ntree = 501 , mtry = 4 ,
                              nodesize = 10 , importance = TRUE)
print(test.rndforest)
## OOB 21% 
print(test.rndforest$err.rate)
plot(test.rndforest,main = "test rndforest")
## after 150 error rate stabelize  
importance(test.rndforest)
#### the highest variable in importance is
# 1- inflight entertainmanet --- same as train data
# 2- ease of online booking --- #4 in train
# 3- food n drink  ---- # 2 in train
# 4- check in service ---- #3 in train 
# 5- online support ---- #6 in train  
# 6- online boarding ---- #10 in train 
# 7- baggage handling ---- #5 in train 
# 8- inflight wifi ---- #9 in train 
# 9- onboardservice --- #11 in train 
# 10 - cleanliness --- #7 in train 
# 11- arrival delay mins --- #8 in train 
# 12- departure arrival convenient -- same as train 
# 13- departure delays -- same as train 
#### when comparing the choice of canceling the last 2 variables was a good approach 

## tuning 
set.seed(seed)
t.test.rndforest = tuneRF(x = test.rndf[,-3], y = test.rndf$satisfaction ,mtryStart = 10 , stepFactor = 1.5, ntreeTry = 51,
                          improve = 0.0001, nodesize = 10, trace = TRUE,plot = TRUE,doBest = TRUE, importance = TRUE)

## build a refined forest ####
## i will delete the last 2 variables (departure arrival convenient + departure delays)
## i will also change ntry to 5 
r.test.rndforest = randomForest(satisfaction ~ ., data = test.rndf, ntree = 151 , mtry = 5 ,
                                nodesize = 10 , importance = TRUE)
r.test.rndforest

### evaluate the model 
test.rndf$predict.class = predict(test.rndforest, test.rndf , type = "class")
test.rndf$prob = predict(test.rndforest,test.rndf , type = "prob")
head(r.test.rndforest)
r.tbl.test.rndf = table(test.rndf$satisfaction, test.rndf$predict.class)
r.tbl.test.rndf

##error rate
print((r.tbl.test.rndf[1,2]+r.tbl.test.rndf[2,1])/24443)
## very high 20% 
## it has the same error rate as in train  
## the best reason maybe because of have the last outliers I coudlnt remove , made the model with high error rate.

## accuracy
print((r.tbl.test.rndf[1,1]+r.tbl.test.rndf[2,2])/24443)

## sensitivity 
print((r.tbl.test.rndf[1,1])/(r.tbl.test.rndf[1,1]+r.tbl.test.rndf[2,1]))

# specifity 
print((r.tbl.test.rndf[2,2])/(r.tbl.test.rndf[2,2]+r.tbl.test.rndf[1,2]))

## ROC
pred.test.rndf = prediction(test.rndf$prob[,2],test.rndf$satisfaction )
perf.test.rndf = performance(pred.test.rndf,"tpr", "fpr")
plot(perf.test.rndf,main = "ROC Train curve")

## AUC
auc.test.rndf = performance(pred.test.rndf,"auc")
auc.test.rndf = as.numeric(auc.test.rndf@y.values)
auc.test.rndf

## KS
ks.test.rndf = max(perf.test.rndf@y.values[[1]]-perf.test.rndf@x.values[[1]])
ks.test.rndf

## gini
gini.test.rndf = ineq(test.rndf$prob, "gini")
gini.test.rndf

## concordance
Concordance(actuals = test.rndf$satisfaction,predictedScores = test.rndf$prob)

# Confusion Matrix
confusionMatrix(test.rndf$satisfaction,test.rndf$predict.class)

###################################################################### comparing train and test

r.train.rndforest
r.test.rndforest

r.tbl.train.rndf
r.tbl.test.rndf

##error rate
print((r.tbl.train.rndf[1,2]+r.tbl.train.rndf[2,1])/57034)
print((r.tbl.test.rndf[1,2]+r.tbl.test.rndf[2,1])/24443)

## accuracy
print((r.tbl.train.rndf[1,1]+r.tbl.train.rndf[2,2])/57034)
print((r.tbl.test.rndf[1,1]+r.tbl.test.rndf[2,2])/24443)

## sensitivity 
print((r.tbl.train.rndf[1,1])/(r.tbl.train.rndf[1,1]+r.tbl.train.rndf[2,1])/57034)
print((r.tbl.test.rndf[1,1])/(r.tbl.test.rndf[1,1]+r.tbl.test.rndf[2,1])/24443)

# specifity 
print((r.tbl.train.rndf[2,2])/(r.tbl.train.rndf[2,2]+r.tbl.train.rndf[1,2])/57034)
print((r.tbl.test.rndf[2,2])/(r.tbl.test.rndf[2,2]+r.tbl.test.rndf[1,2])/24443)

## ROC
plot(perf.train.rndf,main = "ROC train curve")
plot(perf.test.rndf,main = "ROC test curve")

## AUC
auc.train.rndf
auc.test.rndf

## KS
ks.train.rndf
ks.test.rndf

## gini
gini.train.rndf
gini.test.rndf

## concordance
Concordance(actuals = r.train.rndf$satisfaction,predictedScores = r.train.rndf$prob)
Concordance(actuals = r.test.rndf$satisfaction,predictedScores = r.test.rndf$prob)


confusionMatrix(r.train.rndf$satisfaction,r.train.rndf$predict.class)
confusionMatrix(r.test.rndf$satisfaction,r.test.rndf$predict.class)


## both models matches 

################################                                 ###########################
################################    logistic regression          ###########################
################################                                 ###########################

set.seed(seed)
train.lg = train

## importance
fit.train = randomForest(satisfaction ~., data = train.lg)
varImp(fit.train)
## 1- inflight entertainemnt 
## 2- ease of onlineboking 
## 3- checkin sevice 
## 4- 

## because letter S comes after Letter N so this model is for neutral or disatisfied 
lg.model.train = glm(satisfaction ~ ., data = train.lg, family = "binomial")
summary(lg.model.train)
library(rms)
vif(lg.model.train)
## all variables have less than 5 which are not inflated
lg.model.train

exp(1.7343095)

print(exp(cbind(OR = coef(lg.model.train),confint(lg.model.train))))

# for every 5 point in extra enhancment the odds of satisfaction 0.66  will increase

## fitted values
plot(train.lg$satisfaction,lg.model.train$fitted.values, main = "lg train fitted values")
## probability above 0.6 most will be satisfied
lg.predict = ifelse(lg.model.train$fitted.values<0.6, "neutral or dissatisfied", "satisfied")
summary(lg.predict)
tbl.train.lg = table(train.lg$satisfaction, lg.predict)

##error rate
print((tbl.train.lg[1,2]+tbl.train.lg[2,1])/57034)

## accuracy
print((tbl.train.lg[1,1]+tbl.train.lg[2,2])/57034)

## sensitivity 
print((tbl.train.lg[1,1])/(tbl.train.lg[1,1]+tbl.train.lg[2,1]))

# specifity 
print((tbl.train.lg[2,2])/(tbl.train.lg[2,2]+tbl.train.lg[1,2]))


## PROC
library(pROC)
roc(train.lg$satisfaction,lg.model.train$fitted.values)
plot(roc(train.lg$satisfaction,lg.model.train$fitted.values), main = "train roc")




# concordance
concordance(train.lg$satisfaction, lg.predict)

############################################################################## using test data

set.seed(seed)
test.lg = test

## importance
fit.test = randomForest(satisfaction ~., data = test.lg)
varImp(fit.test)
## 1- inflight entertainemnt 
## 2- ease of onlineboking 
## 3- checkin sevice 
## 4- 

## because letter S comes after Letter N so this model is for neutral or disatisfied 
lg.model.test = glm(satisfaction ~ ., data = test.lg, family = "binomial")
summary(lg.model.test)
vif(lg.model.test)
## all variables have less than 5 which are not inflated
lg.model.test

exp(1.7343095)
# for every 5 point in extra enhancment the odds of satisfaction 0.66  will increase
print(exp(cbind(OR = coef(lg.model.test),confint(lg.model.test))))

## fitted values
plot(test.lg$satisfaction,lg.model.test$fitted.values, main = "lg test fitted values")
## probability above 0.6 most will be satisfied
lg.predict.test = ifelse(lg.model.test$fitted.values<0.6, "neutral or dissatisfied", "satisfied")
summary(lg.predict.test)
tbl.test.lg=table(test.lg$satisfaction, lg.predict.test)

##error rate
print((tbl.test.lg[1,2]+tbl.test.lg[2,1])/24443)

## accuracy
print((tbl.test.lg[1,1]+tbl.test.lg[2,2])/24443)

## sensitivity 
print((tbl.test.lg[1,1])/(tbl.test.lg[1,1]+tbl.test.lg[2,1]))

# specifity 
print((tbl.test.lg[2,2])/(tbl.test.lg[2,2]+tbl.test.lg[1,2]))

## ROC
library(pROC)
roc(test.lg$satisfaction,lg.model.test$fitted.values)
plot(roc(test.lg$satisfaction,lg.model.test$fitted.values), main = "test roc")

# concordance
concordance(train.lg$satisfaction, lg.predict)


############################### THE END ##############################
##### BY KHALED MAJZOUB

hist(fli_sur$Age)
str(fli_sur)
