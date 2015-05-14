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













 
