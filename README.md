# Notes
### Loading and preprocessing

 activity <- read.csv("./activity.csv", header = TRUE, sep = ",")  
 activity$date <- ymd(activity$date)  
 
 ###Question 1  - Mean total number of steps per day
step_total <-  ddply(activity, .(date), summarize, Sum = sum(steps, na.rm = TRUE))  
 
 hist(step_total$Sum, breaks = "sturges") #note - need to recheck the no of breaks to be more suitable  
 mean(step_total$Sum)  
 median(step_total$Sum)  


   
###Question 2  - Average daily activity pattern

interval_total <- ddply(activity, .(interval), summarize, Average = mean(steps, na.rm = TRUE))  
interval_total$interval2 <- formatC(interval_total$interval, width = 4, format = "d", flag = "0")  
 interval_total$interval3 <- strptime(interval_total$interval2, "%H%M")  
 
 ggplot(interval_total, aes(x=interval2, y=Average)) +   
        geom_line(color="darkblue", size=1) +  
labs(x="5 minute interval", y="Average steps", title = "Average Steps per 5-Minute Intervals")+
scale_x_datetime(breaks =date_breaks("2 hour"), labels= date_format("%H:%M"))

interval_total[(which(interval_total$Average == max(interval_total$Average), arr.ind = TRUE)),1]


###Question 3  - Imputing missing values
> sum(is.na(activity$steps))
[1] 2304
> length(activity$steps)
[1] 17568



for(i in 1:nrow(activity)){

if(is.na(activity$steps[i])==TRUE){

activity$steps[i]=interval_total$Average[which(interval_total$interval==activity$interval[i])]
}
}


step_total <-  ddply(activity, .(date), summarize, Sum = sum(steps, na.rm = TRUE))  

hist(step_total$Sum, breaks = "sturges") #note - need to recheck the no of breaks to be more suitable 


mean(step_total$Sum)  
[1] 10766.19
>  median(step_total$Sum)  
[1] 10766.19

###Question 4  - Activity Pattern Differences  

activity$day <- weekdays(activity$date)   
activity$day <- gsub("Saturday|Sunday", "weekend", activity$day)  
activity$day <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", activity$day)  

activity2 %>%
	group_by(day, interval) %>%
	summarize(avg_steps = mean(steps)) %>%
	filter(grepl("weekday", day)



#New One

### Loading and preprocessing data

#### Load necessary libraries 

library(timeDate)  
library(dplyr)  
library(scales)  
library(ggplot)  

#### Store csv file as "activity", convert dates to Posixlt, add extra column "time_interval" with interval as time


activity <- read.csv("./activity.csv", header = TRUE, sep = ",")  
  
activity$date <- as.POSIXct(activity$date) 
activity$interval <- formatC(activity$interval, width = 4, format = "d", flag = "0")  
#activity$time_interval <- strptime(activity$time_interval, "%H%M")     

head(activity)


###Question 1  - Mean total number of steps per day  

####Calculation
step_total <- activity %>% 

		group_by(date)%>%
		summarize(total_steps = sum(steps, na.rm = TRUE))

####Plot histogram


ggplot(data = step_total, aes(x = total_steps))+
	geom_histogram(colour = "black", fill = "lightblue", binwidth = 2000)


####Mean & Median

mean(step_total$total_steps)  
 
median(step_total$total_steps)   

  

###Question 2  - Average daily activity pattern

####Plot 5-minute interval and average steps taken across all days



by_interval <- activity %>%
	       group_by(interval) %>%
	       summarize(avg_steps = mean(steps, na.rm = TRUE))


 by_interval$interval2 <- as.POSIXct(strptime(by_interval$interval, "%H%M"))


ggplot(by_interval, aes(x=interval2, y=avg_steps)) + theme_bw() + geom_line(color="blue", size=1) +
labs(x="5 minute interval", y="Average steps", title = "Average Steps per 5-Minute Intervals")+
scale_x_datetime(breaks =date_breaks("2 hour"), labels= date_format("%H:%M"))+


####Which 5-minute interval has highest number of steps

by_interval[(which(by_interval$avg_steps == max(by_interval$avg_steps), arr.ind = TRUE)),1]



###Question 3  - Imputing missing values
  




by_day <- activity2 %>%
	group_by(day, interval) %>%
	summarize(avg_steps = mean(steps)) %>%
	filter(grepl("weekday", day)



p<- ggplot(interval_total, aes(x=interval2, y=Average)) + geom_line(color="darkblue", size=1) 



p + facet_wrap(~day, nrow = 2) + 
labs(x="5 minute interval", y="Average steps", title = "Average Steps per 5-Minute Intervals")+
scale_x_datetime(breaks =date_breaks("2 hour"), labels= date_format("%H:%M"))







 
