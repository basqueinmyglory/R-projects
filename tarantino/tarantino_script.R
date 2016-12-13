library(readr)
library("dplyr", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3")
tarantino <- read_csv("~/data-master/tarantino/tarantino.csv")

###CLEANING DATA####

tgroup <- tarantino$word
#Fucks
tgroup<-gsub(".*fuck.*", "Fucks", tgroup)

#Shits
tgroup<-gsub(".*shit.*|.*merde.*", "Shits", tgroup)

#Female Slurs
tgroup<-gsub(".*bitch.*|.*slut.*|.*cunt.*", "Female Slur", tgroup)

#Racial Slurs
tgroup<-gsub(".*jew.*|.*squaw.*|.*negro.*|.*slope.*|.*jap.*|.*gook.*|.*n-word.*|.*wetback.*", "Racial Slur", tgroup)

#Gay Slur
tgroup<-gsub(".*faggot.*|.*cocksu.*", "Gay Slur", tgroup)

#Male Genitalia
tgroup<-gsub(".*dick.*", "Male Genitalia", tgroup)

#PG-13
tgroup<-gsub(".*ass.*|.*damn.*|.*bast.*|.*cock.*|.*hell.*|.*pussy.*", "PG-13", tgroup)



######################################################################################
######################################################################################
######################################################################################

#Joining Groups to Main dataset
curses<-tarantino
curses$group <- tgroup
#Removing Deaths
curses <- curses[!is.na(curses$word),]

#add Years
year <- c(1992, 1994, 2003, 2004, 2009, 2012, 1997)
movie <- c("Reservoir Dogs", "Pulp Fiction", "Kill Bill: Vol. 1", "Kill Bill: Vol. 2", "Inglorious Basterds",  "Django Unchained", "Jackie Brown")
tityear <- cbind(movie, year)
curses <- merge(curses,tityear)


######################################################################################
######################################################################################
######################################################################################


#summary By Group
totalcurses <- curses %>% group_by(group) %>% count(group)%>% mutate(Percent = round(n/1704*100,1)) %>% arrange(desc(Percent))
colnames(totalcurses)[1:3] <- c("Group","N","Percent")

#Summary By Movie
totalmovie <- curses %>% group_by(movie) %>% count(movie)%>% mutate(Percent = round(n/1704*100,1)) %>% arrange(desc(Percent))
colnames(totalcurses)[1:3] <- c("Group","N","Percent")

#Summary by movie and group
totalCursesMovie <- curses %>% group_by(movie, group) %>% count(group)%>% mutate(PercentofTotal = round(n/1704*100,1), total = sum(n), PercentofMovie = round(n/total*100,1))

#GRAPH TIME!!!!!!!!
DistributionChart<-ggplot(data = totalCursesMovie, aes(x=movie,y=PercentofMovie)) + geom_bar(aes(fill=group), stat="identity")

######################################################################################
######################################################################################
######################################################################################

#Adding Years to data set
totalCursesMovie <- merge(totalCursesMovie,tityear)
#Factoring movie by year
totalCursesMovie$movie <- factor(totalCursesMovie$movie, levels=c("Reservoir Dogs","Pulp Fiction","Jackie Brown","Kill Bill: Vol. 1","Kill Bill: Vol. 2","Inglorious Basterds","Django Unchained"))

#Graph by Year
moviesOverTime <-ggplot(data = totalCursesMovie, aes(x=factor(movie),y=PercentofMovie)) + geom_bar(aes(fill=group), stat="identity")

totalmovieyear <- curses %>% group_by(movie,year) %>% count(movie)%>% mutate(Percent = round(n/1704*100,1)) %>% arrange(year)


######################################################################################
######################################################################################
######################################################################################

#Where in movies does the swearing occur
ggplot(data = curses, aes(x=minutes_in, y=movie)) + geom_point(aes(col=group), position = "jitter")

#Box-Whisker of curse word usage
ggplot(data = curses, aes(y=minutes_in, x=movie)) + geom_boxplot()









