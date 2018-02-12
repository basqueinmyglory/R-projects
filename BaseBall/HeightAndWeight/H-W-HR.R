#################
###Import Data###
#################
data(Batting)
Batting <- as.data.frame(Batting)
data(Master)
Master <- as.data.frame(Master)
data(Fielding)
Fielding <- as.data.frame(Fielding)
###################
###Clean Up Data###
###################
#Shrink Data Sets So my computer can actually do the Merges
Batting <- Batting %>% filter(yearID > 1980) %>% select(playerID, yearID, AB, X2B:HR, SB)
Master <- Master %>% select(playerID, nameFirst, nameLast,weight,height)
Fielding <- Fielding %>% filter(yearID > 1980) %>% select(yearID, playerID,POS, G)
#Merge to Get Batting Stats
batMast1 <- merge(Batting, Master, by = "playerID")
#Merge to get position
batMast <- merge(batMast1, Fielding, by = c("playerID","yearID"))
##################
###Look at 2014###
##################
bat14 <- batMast %>% filter(yearID == 2014, POS != "P", G > 80)
bat14$POS <- str_replace(bat14$POS,"CF", "OF")
bat14$POS <- str_replace(bat14$POS,"RF", "OF")
bat14$POS <- str_replace(bat14$POS,"LF", "OF")
#graph of Height adn Weight all together position highlighted
ggplot(bat14, aes(weight, height)) + geom_jitter(aes(color = ifelse((POS == "3B"),"a","b")), size = 6) + scale_color_manual(guide=FALSE, values=c("orange", "black"))
#graph of Height and Weight facets by POS with HR
ggplot(bat14, aes(weight,height)) + geom_jitter(aes(color = HR, size = HR)) + scale_size(range = c(0, 15)) + facet_wrap(~POS)+ scale_colour_gradient(limits=c(0, 50), high="red",low="blue")
#graph of Height and Weight facets by POS with SB
ggplot(bat14, aes(weight,height)) + geom_jitter(aes(color = SB, size = SB)) + scale_size(range = c(0, 15)) + facet_wrap(~POS)+ scale_colour_gradient(limits=c(0, 65), high="red",low="blue")
#########################################
###average Height and Weight Over Time###
#########################################
overTime <- batMast %>% filter(POS != "P",G > 80) %>% group_by(yearID,POS) %>% summarise(AvgH = round(mean(height),2), AvgW = round(mean(weight), 2), AvgHR = round(mean(HR),2))

#graph over time Hieght
ggplot(overTime, aes(x=yearID, y=AvgH,color = POS)) + geom_line(size = 2)
#graph over time Wieght
ggplot(overTime, aes(x=yearID, y=AvgW,color = POS)) + geom_line(size = 2)
#graph over time HR
ggplot(overTime, aes(x=yearID, y=AvgHR,color = POS)) + geom_line(size = 2)








