library(dplyr)
library(ggplot2)
library(xts)

head(data)

data <- read_csv("~/Data Projects/redditGermanCrimes/data.csv")
colnames(data) <- c("sex", "birthDate", "dayOfBirth", "monthOfBirth", "yearOfBirth", "age", "crimeType", "crimeName", "sentenceDate", "Sentence")

data$birthDate <- as.Date(data$birthDate, "%d/%m/%Y")
data$sentenceMonYear <- format(as.Date(data$sentenceDate, "%m/%d/%Y"),"%m/%Y")
data$sentenceYear <- format(as.Date(data$sentenceDate, "%m/%d/%Y"),"%Y")


#Overall histogram of Age
data %>% ggplot(aes(x = age, fill = sex)) + 
        geom_histogram(binwidth = 2, color = "black") +
        xlab("Age") + 
        ylab("Count")

#Overall histogram of Age facet by Sex color is crime committed
data %>% ggplot(aes(x = age, fill = crimeType)) + 
  geom_histogram(binwidth = 2, color = "black") + 
  facet_wrap(~sex) +
  xlab("Age") + 
  ylab("Count")

#Histogram of age by crime
data %>% ggplot(aes(x = age, fill = sex)) + 
  geom_histogram(binwidth = 2, color = "black") +
  facet_wrap(~crimeType) + 
  xlab("Age") + 
  ylab("Count")


#Histogram of age by Year Committed
na.omit(data) %>% ggplot(aes(x = age, fill = crimeType)) + 
  geom_histogram(binwidth = 2, color = "black") +
  facet_wrap(~sentenceYear) + 
  xlab("Age") + 
  ylab("Count")


#Histogram of divided by months committed
counts <- data %>% group_by(sentenceMonYear) %>%
                    mutate(countMonYear = n())
counts <- counts %>% group_by(sentenceYear) %>%
                    mutate(countYear = n())
counts <- na.omit(counts)

counts %>%  ggplot(aes(x = sentenceMonYear, y = countMonYear, fill = sex)) +
            geom_col(width = 3) + 
            facet_wrap(~sentenceYear) + 
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())


#Histogram of when crimes were committed
counts %>%  ggplot(aes(x = sentenceMonYear, y = countMonYear, fill = sex)) +
  geom_col() + 
  facet_wrap(~crimeType) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())




