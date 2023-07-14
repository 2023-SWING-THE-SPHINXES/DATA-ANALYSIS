library(readr)
library(shape)
library(descr)
install.packages("openxlsx")
library(openxlsx)
install.packages("psych")
library(psych)
install.packages("ggplot2")
install.packages("forecast")
library(ggplot2)
library(forecast)
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)      # For data manipulation and visualization
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("zoo")
library(zoo)
install.packages("tseries")
library(tseries)
install.packages("reshape2")
library(reshape2)

#honey production data
honeyproduction <- read_csv("python/R/honey.csv")
honeyp <- read_csv("python/R/honeyProduction.csv")
honeyp <- honeyp[2:9]
honeyp <- honeyp %>% relocate(year)
ohioproduction <- honeyproduction %>% filter(honeyproduction$state =="OH")
caliproduction <- honeyproduction %>% filter(honeyproduction$state == "CA")


#air quality 
merged.AQI <- read_csv("python/R/merged_AQI.csv")
merged.AQI$Latitude <- NULL
merged.AQI$Longitude <-NULL
colnames(merged.AQI) <- c("id","state","year","month","parameter","AQI")
merged.AQI$id <-NULL


AQIS <- merged.AQI %>%
  pivot_wider(names_from = parameter, values_from = AQI)

AQIS <- AQIS %>% group_by(year,state) %>% summarise(CO = mean(CO,na.rm = T), 
                                                   NO2 = mean(NO2, na.rm=T),
                                                   Ozone = mean(Ozone,na.rm=T),
                                                   SO2 = mean(SO2, na.rm = T),
                                                   PM10 = mean(PM10, na.rm = T),
                                                   PM2.5 =mean(PM2.5,na.rm = T))
install.packages("usdata")
library(usdata)
AQIS$state <-state2abbr(AQIS$state)

#merge, AQUI and honey data
AQIS <-as.data.frame(AQIS)
honeyp <- as.data.frame(honeyp)

usint <- merge(honeyp,AQIS, all=TRUE)
usint2 <- merge(usint, food3, all = TRUE)


write_csv(usint,"temporary.csv")

corr.test(usint[3:14],method = "pearson", alpha = .05,  use = "pairwise.complete.obs")

#agriculture info and crop data


vegi <- read_csv("python/R/vegielse.csv")
vegi <- vegi[,c(4:13)]
vegi$Period <- NULL
vegi$`Geo Level`<-NULL
vegi$watershed_code <-NULL
vegi$`Domain Category`<-NULL
vegi$Domain<-NULL
colnames(vegi) <- c("year","state","commodity","item", "value")
vegi[vegi =="(D)"] <-NA
vegi$value <-as.numeric(vegi$value)


food <- read_csv("python/R/food_production.csv")
food <- as.data.frame(food[,c(2:20)])
food <-food%>%relocate(Value)
food <-food%>%relocate(Commodity)
food <-food%>%relocate('Data Item')
food <- food %>% relocate(State)
food$State <-state2abbr(food$State)
food <- food %>% relocate(Year)
food <-food[1:5]
colnames(food) <- c("year","state","item","commodity", "value")
food[food =="(D)"] <-NA
food[food=="(X)"]<-NA
food[food=="(Z)"]<-NA
food[food=="(NA)"]<-NA
food[food=="NA"]<-NA
#food <- food[complete.cases(as.numeric(food$value)), ]
write.csv(food,"food.csv",row.names = FALSE)

food <- food[!grepl("[^0-9.]", food$value), ]

food$value <- as.numeric(food$value)
food <- food %>% pivot_wider(names_from = item,values_from = value)

#vegi$value <- as.numeric(ifelse(vegi$value=="(D)",vegi$value <- NA,vegi$value <-vegi$value)) 
# Group by 'category' and 'sub_category', calculate the sum of 'value'
#data_sum <- vegi %>%
 # group_by(year, state,commodity) %>%
#  summarize(sum_value = sum(value, na.rm = TRUE))

#vegis <- vegi%>%  pivot_wider(names_from = item, values_from = value, values_fn=sum)
#vegis <-vegi %>%  pivot_wider(names_from = item, values_from = value) %>% group_by(summarise(sum(value)))
#vegis[,'ACRES HARVESTED'] <- sum(vegis[,'ACRES HARVESTED'])
#vegis <- vegi %>%group_by(commodity) %>%
 # mutate(row = row_number()) %>%
  #tidyr::pivot_wider(names_from = item, values_from = value) %>%
  #select(-row)

food1 <- read_csv("python/R/food_strTOfloat.csv")
food1 <- food1 %>% pivot_wider(names_from = item,values_from = value,values_fn = sum)
food2 <- food1 %>% mutate(sums = rowSums(across(4:14),na.rm = TRUE))%>%select(-c(4:14))
food3 <- food2  %>% pivot_wider(names_from = commodity,values_from = sums)
food4 <- food3 %>% mutate(total_food = rowSums(across(3:13),na.rm = TRUE))


pt <- read_csv("python/R/productiontotal.csv")
pt <- as.data.frame(pt[,c(2:20)])
pt <-pt%>%relocate(Value)
pt <-pt%>%relocate(Commodity)
pt <-pt%>%relocate('Data Item')
pt <- pt %>% relocate(State)
pt$State <-state2abbr(pt$State)
pt <- pt %>% relocate(Year)
pt <-pt[1:5]
colnames(pt) <- c("year","state","item","commodity", "harvtotal")
#

usint <- merge(honeyp,AQIS, all=TRUE)
usint <- merge(usint, food4, all = TRUE)


#온도

temp1 <- read_csv("python/R/temp_processed.csv")
temp1$state2 <- state2abbr(temp1$state)
temp1$state <- NULL
colnames(temp1) = c("year", "temperature","state")
temp1 <- temp1%>% relocate(state)



us_all <- merge(usint, temp1, all = TRUE)
us_all2 <- merge(us_all,pt, all = TRUE)


write.csv(us_all, "python/R/us_all.csv",row.names = FALSE)
write.csv(us_all2, "python/R/us_all2.csv")

#california(failed)
caliaqi <- read_csv("python/R/Cali_AQI.csv")
ohaqi <- read_csv("python/R/OH_AQI.csv")
caliaqi$Latitude <- NULL
caliaqi$Longitude <-NULL
colnames(caliaqi) <- c("id","state","year","month","parameter","AQI")
caliaqi$id <-NULL
inte <-read_csv("python/R/integratedus.csv")
ohaqi$Latitude <- NULL
ohaqi$Longitude <-NULL
colnames(ohaqi) <- c("id","state","year","month","parameter","AQI")
ohaqi$id <-NULL
caliaqi2 <- caliaqi %>%
  pivot_wider(names_from = parameter, values_from = AQI)
ohaqi2 <- ohaqi %>%
  pivot_wider(names_from = parameter, values_from = AQI)

ohaqi_y <- ohaqi2 %>% group_by(year) %>% summarise(CO = mean(CO,na.rm = T), 
                                                         NO2 = mean(NO2, na.rm=T),
                                                         Ozone = mean(Ozone,na.rm=T),
                                                         SO2 = mean(SO2, na.rm = T),
                                                         PM10 = mean(PM10, na.rm = T),
                                                         PM2.5 =mean(PM2.5,na.rm = T))
caliaqi_y <- caliaqi2 %>% group_by(year) %>% summarise(CO = mean(CO,na.rm = T), 
                                                       NO2 = mean(NO2, na.rm=T),
                                                       Ozone = mean(Ozone,na.rm=T),
                                                       SO2 = mean(SO2, na.rm = T),
                                                       PM10 = mean(PM10, na.rm = T),
                                                       PM2.5 =mean(PM2.5,na.rm = T))
oha <- ohaqi_y[-(1:5),]
cal <- caliaqi_y[-(1:5),]
oha$year <- NULL
cal$year <- NULL

ohaha <-round(oha,digits=2)
calha <-round(cal,digits=2)
write.csv(ohaha,"python/R/caliaqi2.csv",row.names = FALSE)
write.csv(calha,"python/R/ohaqi2.csv",row.names = FALSE)

cali
usreal <- read_csv("python/R/usreal.csv")
ts.usreal <- ts(data = usreal, start = 1980, end = 2021, frequency = 1)
plot(ts.usreal)
colony_ts <- ts(usreal$colony, start = 1980, end = 2021, frequency = 1)

#이상치 체크 및 삭제
boxplot(usint$numcol)
boxplot(usint$totalprod)
boxplot(usint$CO)
descriptive <- decriptive%>% mutate(LL=mean - 3*sd,UL = mean+3*Sd)

corr.test(us_all2[9:27],method = "pearson", alpha = .05,  use = "pairwise.complete.obs")
lm1 <- lm(total_food~CO,data = us_all)
summary(lm1)
lm2 <- lm(CO~prodvalue,data = us_all)
summary(lm2)
lm21 <- lm(CO~yieldpercol,data = us_all)
summary(lm21)
lm22 <- lm(CO~numcol,data = us_all)
summary(lm22)
corr.test(us_all$numcol,us_all$CO)
corr.test(us_all$numcol)


install.packages("dplyr")
library(dplyr)
al <- read_csv("python/R/us_all.csv")

pt <- read_csv("python/R/productiontotal.csv")
pt <- as.data.frame(pt[,c(2:20)])
pt <-pt%>%relocate(Value)
pt <-pt%>%relocate(Commodity)
pt <-pt%>%relocate('Data Item')
pt <- pt %>% relocate(State)
pt$State <-state2abbr(pt$State)
pt <- pt %>% relocate(Year)
pt <-pt[1:5]
colnames(pt) <- c("year","state","item","commodity", "harvtotal")

als <- merge(al,pt,all = TRUE)
write.csv(als,"python/R/als.csv")

als <- read_csv("python/R/als.csv")
als <- als%>% mutate(time = year-1980)
als$commodity <- NULL
als$item <- NULL
als$harvtotal <- as.numeric(gsub("[^0-9.]", "", als$harvtotal))

test <- als[complete.cases(als$harvtotal), ]

install.packages("gee")
library(gee)
fit1<-gee(harvtotal~time+CO*NO2*Ozone*SO2*PM10*PM2.5,data = test, id=state, family=gaussian,  corstr="independence",  scale.fix=TRUE) 
lm1 <-- lm
glm1 <- glm(harvtotal~time + CO*NO2*Ozone*SO2*PM10*PM2.5,family = gaussian ,data = test)
summary(glm1)

install.package("geepack")
library(geepack)

test<-als[c('year','state','CO','NO2','Ozone','SO2','PM10','PM2.5','temperature','harvtotal')]
test<- test%<% mutate(time = year - 19)

test$id  <- as.numeric(factor(test$state, levels = unique(test$state)))

gee_model1 <- geeglm(harvtotal ~ time+ CO*NO2*Ozone*SO2*PM10*PM2.5+ (time|id), data = test, family = gaussian)

glm1 <- glm(harvtotal~time + CO*NO2*Ozone*SO2*PM10*PM2.5,id = id, family = gaussian ,data = test, costr= "unstructured")
summary(glm1)

gee_model <- geeglm(harvtotal ~ time + CO * NO2 * Ozone * SO2 * PM10 * PM2.5, id = id, family = gaussian, corstr = "unstructured", data = test)

lm2 <- lm(harvtotal ~ NO2*Ozone*PM10*PM2.5 ,data = test)
summary(lm2)
plot(lm1)


lm2 <- lm(har)

stepwise_model <- step(lm1, direction = "both", k = 2)
summary(stepwise_model)
print(stepwise_model)
significant_vars <- stepwise_model$coefficients[stepwise_model$coefficients[, "Pr(>|t|)" < 0.05, ]]
print(significant_vars)



backward_model <- step(lm1, direction = "backward", k = 2)

summary(stepwise_model)
print(stepwise_model)

# Print the summary of the GEE model
summary(gee_model)

install.packages("coefplot")
library(coefplot)

# Assuming your GEE model is called 'gee_model'
# Use the coefplot() function to create the coefficient plot
coefplot(glm1)
coefplot(glm1, main = "Coefficient Plot", show.significant = TRUE)
sig_coefs <- glm1$coefficients[glm1$p.value <= 0.05]
annotate_coefplot(sig_coefs, main = "Significant Coefficients")
# Display the equation of the model
model_eqn <- paste0("Crop Production = ", round(coef(glm1), 2))
text(0, 0.9, model_eqn, cex = 0.8)

plot(test$harvtotal, predicted_values, xlab = "Observed", ylab = "Predicted", main = "Observed vs. Predicted")


glm3 <- glm(numcol~time+temperature,data = test)
summary(glm3)
lm4 <- lm(numcol ~ temperature,data = test)
summary(lm4)

lm3 <- lm(harvtotal~temperature,data = test)
summary(lm3)
lm2 <- lm(harvtotal~temperature+numcol, data = test)
summary(lm2)
lm10<-lm(harvtotal~time+numcol+totalprod,prodvalue, data = test)
summary(lm10)
lm10<-lm(harvtotal~numcol*yieldpercol, data = test)
summary(lm10)
test$ALMONDS <-NULL
test$STRAWBERRIES <-NULL
test_z <- data.frame(scale(test[-c(1,3)]))
summary(lm10)
lm10<-lm(harvtotal~numcol*yieldpercol, data = test_z)
summary(lm10)
lm11 <-lm(harvtotal~numcol, data = test_z)
summary(lm11)
lm12 <- lm(harvtotal~yieldpercol, data = test_z)
summary(lm12)
corr.test(test[,c("harvtotal","time","temperature","CO","NO2","Ozone","SO2","PM10","PM2.5","numcol","yieldpercol")])
write_csv(test,"test.csv")

plot(test$PM10,test$numcol)
lm11<-lm(harvtotal)
