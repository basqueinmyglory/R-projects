#Import Data Set
origin <- read.csv("~/Data Projects/redditAnniversaryConvo/Anniversary Data - Sheet1.csv", stringsAsFactors=FALSE)

#Load The Libraries
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(tidyr)

#
#Combine date column with begining time and end time to get exact dates
#
origin$start <- paste(origin$Date, " ", origin$Time.Start)
origin$start <- as.POSIXct(origin$start, tz = "EST",  "%m/%d/%Y %I:%M %p")

origin$end <- paste(origin$Date, " ", origin$Time.End)
origin$end <- as.POSIXct(origin$end, tz = "EST",  "%m/%d/%Y %I:%M %p")

#
#Clean up data set b/c there are duplicate rows
#Dropped duration in time. As it is already there in minutes. Factored the Weekdays
#
main <- origin[,c(5,7:11)]
main$Weekday <- factor(main$Weekday, levels = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = T)

main$Notes[main$Notes == ""] <- "Normal"
main$Notes[main$Notes == "TOGETHER"] <- "Together"

#
#Checking to see all is good
#
str(main)

colnames(main)[1] <- "Duration"
colnames(main)[5:6] <- c("Start", "End")

main <- main[,c(5,6,1,2,3,4)]

#These are the negative values if a call started before midnight but ended the next day
main %>% mutate(dur = End - Start) %>% filter(dur < 0)

####
####Let the Graphs Begin!
####

#x = date, y = duration, fill = Notes
main %>% ggplot(aes(Start, Duration)) + 
          geom_point(aes(color = Notes)) + 
          xlab("Date")

#x = date, y = Duration, fill = Type
main %>% ggplot(aes(Start, Duration)) + 
          geom_point(aes(color = Type)) + 
          xlab("Date")

#histogram of distributions
main %>% ggplot(aes(Duration)) + 
          geom_histogram(aes(fill = Type), binwidth = 30) + 
          ylab("Count")

#weekday distribution
main %>% count(Weekday) %>%
          ggplot(aes(Weekday, n)) + 
          geom_col(aes(fill = Weekday)) + 
          ylab("Count of Conversations")

#Violin Plot by weekday
main %>% ggplot(aes(x = Weekday)) + 
          geom_violin(aes(y = Duration)) + 
          geom_jitter(aes(y=Duration, color = Weekday))

#I want to see in what hours calls occurred the most. So I'm extracting the hour from the start time

#hourly distribution
main %>% mutate(hour = hour(Start)) %>%
         count(hour) %>%
         ggplot(aes(hour, n)) + 
         geom_col()

#Cumsum of time spent on phone by type
#running totals
main %>% spread(Type,Duration, fill = 0) %>%
         mutate(Ccumsum = cumsum(C), 
                FTcumsum = cumsum(FT), 
                Totalcumsum = Ccumsum + FTcumsum) %>%
         ggplot(aes(x = Start)) + 
          geom_line(aes(y = FTcumsum, color = "FT Running Total"), size = 1) +
          geom_line(aes(y = Ccumsum, color = "C Running Total"), size = 1) + 
          geom_line(aes(y = Totalcumsum, color = "Total"), size = 1) + 
          ylab("Running Totals") + 
          xlab("Date") +
          scale_colour_manual("", breaks = c("C Running Total", "FT Running Total", "Total"),values = c("red", "blue", "purple"))

#Heat map. x = hour, y =weekday, y = frequency
main %>% mutate(Hour = hour(Start)) %>%
         select(Duration, Type, Weekday, Notes, Hour) %>%
         group_by(Weekday) %>%
         count(Hour) %>%
         ggplot(aes(Hour, Weekday)) + 
          geom_tile(aes(fill = n)) + 
          scale_fill_gradient(low = "violetred", high = "darkblue") + 
          guides(fill=guide_legend(title="Count of \nConversations"))
        


########################################################################################
###
###EXtra lines of Code that were helpful at one point
###
#Get columsn as right data type aka Date and times.
origin$Date <- as.Date(origin$Date, "%m/%d/%Y")
origin$Time.Start <- as.POSIXct(origin$Time.Start, tz = "EST",  "%I:%M %p")
