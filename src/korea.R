library(readr)
library(dbplyr)
install.packages("dplyr")
library(dplyr)
library(shape)
library(descr)
install.packages("openxlsx")
library(openxlsx)
install.packages("tidyverse")
library(tidyverse)
priceindex <-read.xlsx("python/R/priceindex.xlsx",startRow = 1)
colnames(priceindex) <- c("type","name",c(1985:2012))
honey_price <- priceindex %>% filter(name == "벌  꿀")
honey_long<-t(honey_price)
years = seq(1983,2012)
honey_df <- data.frame(honey_long,row.names=NULL)
honey_df$year <- years
honey_df<- honey_df[-c(1, 2), ]
colnames(honey_df)<-c("price","year")
honey_ts <-ts(honey_df$price, start = c(1985), frequency = 1)
plot(honey_ts,ylab = "price")

all.long <-t(priceindex)
colnames(all.long) <- all.long[2,]
all.long <- all.long[-c(1,2),]
row.names(all.long)<- NULL
years = seq(1985,2012)
all.long <- all.long[,-1]
all.long[,1] <- seq(1985,2012)

#write.csv2(all.long,"crops.csv",row.names = FALSE)
write.csv2(honey_df,"honey_df.csv",row.names = FALSE)

crops <- read_csv("python/R/crop.csv")
all_ts <- ts(all.long, start = c(1985), frequency = 1)
library(ggplot2)
variable_names <- names(all.long)[-1]
all.long <- as.data.frame(all.long)
group1 <- as.data.frame(all.long[,c(66:78)])
group1$date <- seq(1985,2012)
group1 <- mutate_all(group1, as.numeric)

#
group1mean <- list(rowMeans(group1, na.rm = TRUE))
mola <- data.frame(honey_df,group1mean)
colnames(mola) <-c("price","year","mean")
mola$price <- as.numeric(mola$price)
mola$standardized <- mola$price - mola$mean

ggplot(data = all.long, aes(x = date)) +
  geom_line(aes_string(y = variable_names, color = variable_names), alpha = 0.7) +
  labs(x = "Date", y = "Value") +
  scale_color_manual(values = rainbow(length(variable_names))) +
  theme_minimal()
