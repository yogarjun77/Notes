# Notes
https://rstudio-pubs-static.s3.amazonaws.com/65620_869a2c53ca55417397e60ed0993672fd.html

 activity <- read.csv("./activity.csv", header = TRUE, sep = ",")
 activity$date <- ymd(activity$date)
step_total <-  ddply(activity, .(date), summarize, Sum = sum(steps, na.rm = TRUE))
 
 hist(step_total$Sum, breaks = "sturges") #note - need to recheck the no of breaks to be more suitable
 mean(step_total$Sum)
 median(step_total$Sum)


 interval_total <- ddply(activity, .(interval), summarize, Average = mean(steps, na.rm = TRUE))
