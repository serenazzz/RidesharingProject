library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
setwd('/Users/van/Desktop/Didi_Research')
data = read.csv('/Users/van/Desktop/Didi_Research/sh_gulf.csv')
data = data[,2:11]

data$Hours <- format(as.POSIXct(strptime(data$start_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
data$shared = 0


# get region
summary(data$start_lat)
summary(data$start_long)
summary(data$end_lat)
summary(data$end_long)

hist(data$end_lat)

# 5 missing end_lat and end_long, take those obs out
length(data[data$end_lat == 0,])
length(data[data$end_long == 0,])
data2 = data[data$end_lat != 0,]

#summary
summary(data2$start_lat)
summary(data2$start_long)
summary(data2$end_lat)
summary(data2$end_long)

# start lat from 30.71 - 31.66, 纬度, horizontal
# start long from 121 - 121.9, 经度, vertical
# end lat from 29.93 - 32.05
# end long from 120.1 - 122

# Use same scale
# lat region 29.93 - 32.05, 10 grids,0.22
# long region 120.1 - 122, 10 small grids, 0.2
# label coordinates as a 10 * 10 matrix, 11,12,13,... long = 1, lat = 1: 10
data2$start_region = 0
data2$end_region = 0
play = data2[1:10,]
for (i in 1:171356){
  start_long_region = floor((data[i,3] - 120.1) / 0.2) + 1
  start_lat_region = floor((data[i,4] - 29.93) / 0.22) + 1
  end_long_region = floor((data[i,5] - 120.1) / 0.2) + 1
  end_lat_region = floor((data[i,6] - 29.93)/ 0.22) + 1
  data2[i,13] = start_long_region * 10 + start_lat_region
  data2[i,14] = end_long_region * 10 + end_lat_region
}





# get count for shared
for (i in 1:171356){
  if (data2[i,10] != 0){
    data2[i,12] = sum(data2$label == as.numeric(data2[i,10]))
  }
  if (i %/% 1000){
    print (i)
  }
}

data = read.csv('/Users/van/Desktop/Didi_Research/ShangHai_Data.csv')
data = data[,-1]
# get distance
data$distance = (data[,6] - data[,4])^2 + (data[,5] - data[,3]) ^ 2

# Get Travel Time in Seconds
# data 62987, end time 000000, move it
data = data[-62987,]
data$start_time = as.POSIXct(data$start_time)
data$end_time = as.POSIXct(data$end_time)
data$travel_time = data$end_time - data$start_time


counts_hour <- ddply(data, .(data$Hours, data$shared), nrow)
names(counts_hour) <- c("Hour", "Shared", "Freq")
counts_start_region <- ddply(data, .(data$start_region, data$shared), nrow)
names(counts_start_region) <- c("start_region", "Shared", "Freq")
counts_end_region <- ddply(data, .(data$start_region, data$shared), nrow)
names(counts_end_region) <- c("end_region", "Shared", "Freq")

counts <- ddply(data, .(data$Hours,data$start_region,data$end_region, data$shared), nrow)
names(counts) <- c("Hour","Start_Region","End_Region", "Shared", "Freq")
# write.csv(counts,file = "Counts_File.csv")
# write.csv(data,file = "ShangHai_Data.csv")


summary(factor(counts_start_region$start_region))

# start location to shared ride
count_1 = counts_start_region[1:17,]
count_2 = counts_start_region[18:31,]
count_3 = counts_start_region[32:43,]
count_4 = counts_start_region[44:54,]

ggplot(data=count_1, aes(x=factor(start_region), y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge(.9),vjust = 0)

ggplot(data=count_2, aes(x=factor(start_region), y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge(.9),vjust = 0)

ggplot(data=count_3, aes(x=factor(start_region), y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge(.9),vjust = 0)

ggplot(data=count_4, aes(x=factor(start_region), y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge(.9),vjust = 0)


count_1 = counts_end_region[1:17,]
count_2 = counts_end_region[18:33,]
count_3 = counts_end_region[34:54,]
# end location to shared ride
ggplot(data=count_1, aes(x=factor(end_region), y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge(.9),vjust = 0)

ggplot(data=count_2, aes(x=factor(end_region), y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge(.9),vjust = 0)

ggplot(data=count_3, aes(x=factor(end_region), y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge(.9),vjust = 0)

# distance to shared ride
data_shared = data[data$shared!=0,]
ggplot(data=data_shared, aes(x=distance, y=shared)) +
  geom_point()

#17010
willing_share = data[data$shared==1,]
ggplot(data=willing_share, aes(x=distance)) +
  geom_histogram(bins = 100)

summary(willing_share$distance)
#16983
willing_share = willing_share[willing_share$distance<0.1,]
ggplot(data=willing_share, aes(x=distance)) +
  geom_histogram(bins = 100)

succeed_share = data[data$shared == 2,]
summary(succeed_share$distance)
ggplot(data=succeed_share, aes(x=distance)) +
  geom_histogram(bins = 100)

#Travel Time
data = read.csv("ShangHai_Data.csv")

summary(data[data$shared==0,]$travel_time)
summary(data[data$shared==1,]$travel_time)
summary(data[data$shared==2,]$travel_time)
summary(data[data$shared==3,]$travel_time)
a = c('Shared_0','Shared_1','Shared_2','Shared_3')
b = c(avg0,avg1,avg2,avg3)
data.frame(a,b)

shared0=data[data$shared==0,]
shared1=data[data$shared==1,]
shared2=data[data$shared==2,]
shared3=data[data$shared==3,]
ggplot(data=shared0, aes(x=as.numeric(travel_time))) +
  geom_histogram(bins = 100)
ggplot(data=shared1, aes(x=as.numeric(travel_time))) +
  geom_histogram(bins = 100)
ggplot(data=shared2, aes(x=as.numeric(travel_time))) +
  geom_histogram(bins = 100)
ggplot(data=shared3, aes(x=as.numeric(travel_time))) +
  geom_histogram(bins = 100)

####Calculate shared(1) / shared(0), x -- hour, y -- region
data3=data[data$shared == 0 | data$shared == 1,]
counts_data3 <- ddply(data3, .(data3$Hours,data3$start_region,data3$shared), nrow)
names(counts_data3) <- c("Hour","Start_Region", "Shared", "Freq")

for (i in 0:23){
  for (j in counts_data3$Start_Region){
    if (counts_data3)
  }
}
library(data.table)
table_shared <- data.table(counts_data3)
table_shared[, Ratio := sum(Freq[Shared == 1]) / sum(Freq[Shared == 0]), by = c("Hour","Start_Region")]
table_shared$index = table_shared$Hour + character("_") + table_shared$Start_Region
shared_ratio = data.frame(table_shared)
# Get Unique lines in the data table
shared_ratio = shared_ratio[!duplicated(shared_ratio[1:2]),]
library(viridis)
ggplot(shared_ratio,aes(x=factor(Hour),y=factor(Start_Region),fill=Ratio))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title="Heatmap of Pick Up Hour and Region' Shared Ratio",x ="Hour", y = "Start_Region")+
  theme(axis.text.x=element_text(angle=70,hjust=.2,vjust=0.5))


# Do a statistical test, https://onlinecourses.science.psu.edu/statprogram/node/164
for (i in 1:length(values))
  vector[i] <- values[i]
Z_Value = rep(0,24)
for (i in 0:23){
  test_data = data3[data3$Hours == i,]
  p_hat = as.numeric(nrow(test_data[test_data$label != 0,]))/nrow(test_data)
  p = nrow(data3[data3$label != 0,])/nrow(data3)
  z = (p_hat - p) /(sqrt(p * ( 1- p)/nrow(test_data)))
  Z_Value[i] = z
}







ggplot(data=counts, aes(x=Hour, y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge()) +
  ggtitle("Bar Plot for Each Group ShangHai ---- All")


count_shared = counts[counts$Shared!=0,]
ggplot(data=count_shared, aes(x=Hour, y=Freq, fill=factor(Shared))) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=Freq),position=position_dodge()) +
  ggtitle('Bar Plot for Each Group ShangHai ---- Shared')

counts$Shared = as.factor(counts$Shared)
ggplot(counts, aes(x=Hour, y=Freq, color=Shared,group = Shared)) + 
  geom_line() +
  ggtitle("Line Plot for Each Group ShangHai ---- All")

count_shared$Shared = as.factor(count_shared$Shared)
ggplot(count_shared, aes(x=Hour, y=Freq, color=Shared,group = Shared)) + 
  geom_line()+
  ggtitle("Line Plot for Each Group ShangHai ---- Shared")



library(ggmap)
data$Hours = as.factor(data$Hours)
# ggmap(get_map("shanghai", zoom = 10, source = "stamen",maptype = "bw"))
# geom_point(data = data, aes(x =start_long, y= start_lat), alpha=0.03)

ggmap(get_map("shanghai", zoom = 10, source = "stamen",maptype = "toner")) +
  geom_point(data = data, aes(x =start_long, y= start_lat), alpha=0.03,size = .2,colour = 'blue') +
  facet_grid(Hours) +
  ggtitle('Pick Up in ShangHai All')

# Hour 0 pick up
data_Hour0 = data[data$Hours == '00',]
ggmap(get_map("shanghai", zoom = 10, source = "stamen",maptype = "toner")) +
  geom_point(data = data_Hour0, aes(x =start_long, y= start_lat), alpha=0.03,size = .2,colour = 'blue')+
  ggtitle('Pick Up in ShangHai From 0:00 - 0:59')


data_Hour19 = data[data$Hours == '19',]
ggmap(get_map("shanghai", zoom = 10, source = "stamen",maptype = "toner")) +
  geom_point(data = data_Hour19, aes(x =start_long, y= start_lat), alpha=0.03,size = .2,colour = 'blue')+
  ggtitle('Pick Up in ShangHai From 19:00 - 19:59')


# 




