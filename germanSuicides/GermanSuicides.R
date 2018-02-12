bymethodage <- suicide %>% group_by(method, age.group) %>% summarise(Total = sum(Freq))

#FOR BLOG - Distribution of ages
menAges <- suicide %>% filter(sex == "male") %>% group_by(age) %>% summarise(Total = sum(Freq)) %>% mutate(PTotal = round(Total/sum(Total)*100,2))
ggplot(data = menAges, aes(age,PTotal)) + geom_bar(stat = "identity")

femaleAges <- suicide %>% filter(sex == "female") %>% group_by(age) %>% summarise(Total = sum(Freq)) %>% mutate(PTotal = round(Total/sum(Total)*100,2))
ggplot(data = femaleAges, aes(age,PTotal)) + geom_bar(stat = "identity")


#FOR BLOG - Suicide Methods as the sexes change
menAgesMeth <- suicide %>% filter(sex == "male") %>% group_by(method, age) %>% summarise(Total = sum(Freq))
ggplot(data = menAgesMeth, aes(age,Total)) + geom_bar(stat = "identity") + facet_wrap(~ method)

femaleAgesMeth <- suicide %>% filter(sex == "female") %>% group_by(method, age) %>% summarise(Total = sum(Freq))
ggplot(data = femaleAgesMeth, aes(age,Total)) + geom_bar(stat = "identity") + facet_wrap(~ method)

#Different Methods by gender
  bymethodsex <- suicide %>% group_by(method, sex) %>% summarise(Total = sum(Freq))
  Females <- bymethodsex %>% filter(sex == "female")
  Males <- bymethodsex %>% filter(sex == "male")
  DiffMeth <- cbind(Females[,1], "MTot" = Males$Total, "FTot" = Females$Total)
DiffMeth <- DiffMeth %>% mutate(Diff = MTot - FTot)
#Raw Difference
ggplot(data = DiffMeth, aes(method, Diff)) + geom_bar(stat = "identity")

#FOR BLOG - Percent of Method by sex
PDiff <- DiffMeth %>% mutate(GTotal = MTot + FTot, PMTot = round(100*MTot/sum(MTot), 2), FMTot = round(100*FTot/sum(FTot),2), PDiff = PMTot - FMTot) 
ggplot(data =PDiff, aes(method,PDiff)) + geom_bar(stat = "identity")  

#FOR BLOG - Difference in rates of suicides at different age groups
  byAge <- suicide %>% group_by(age.group,sex) %>% summarise(Total = sum(Freq))
  byF <- byAge %>% filter(sex == "female")
  byM <- byAge %>% filter(sex == "male")
  ageDiff <- cbind(byM[1],"MTotal" = byM$Total, "FTotal" = byF$Total)
ageDiff <- ageDiff %>% mutate(GTotal = MTotal + FTotal, PMT = round(100*MTotal/sum(MTotal),2), PFT = round(100*FTotal/sum(FTotal),2), PDiff = PMT -PFT)
ggplot(data = ageDiff, aes(age.group,PDiff)) + geom_bar(stat = "identity")  

