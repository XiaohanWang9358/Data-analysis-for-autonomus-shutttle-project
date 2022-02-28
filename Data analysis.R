##################### AV Shuttle and TAM #####################
rm(list = ls())

# Load libs -----
library(tidyverse)
#library(mediation)
library(psych)
library(umx)
# Other libs depending on output needed ----
#library(gvlma)
#library(broom) #nice for tidying tables if you don't use rmarkdown
#library(car) #Need for durbinwatson and VIF values
#library(lm.beta) #Standardized coefficients
#library(skimr) # skimr:skim(data) is similiar to psych::describe(data)

# Load data bricks ----
Brick.pre = read.csv("Data/before survey.csv",header=TRUE,na.strings = "") 
Brick.post= read.csv("Data/after data1.csv",header=TRUE,na.strings = "") 

# Clean bricks ----
Brick.pre <- Brick.pre[-c(1,2),]
Brick.post <- Brick.post[-c(1,2),] # Delete first 2 rows

beforedata<-Brick.pre[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)] # PreD = beforeinput[,-c(1:17)]
afterdata<-Brick.post[,-c(1:17)]

#AV terms
beforedata[,'Q5']=(nchar(Brick.pre[,'Q5'])+1)/2
afterdata[,'Q8']=(nchar(Brick.post[,'Q8'])+1)/2


drop_col=c("Q37_First.Click","Q37_Last.Click","Q37_Page.Submit","Q37_Click.Count",
           "Q36_First.Click","Q36_Last.Click","Q36_Page.Submit","Q36_Click.Count",
           "Q38_First.Click","Q38_Last.Click","Q38_Page.Submit","Q38_Click.Count",
           "Q39_First.Click","Q39_Last.Click","Q39_Page.Submit","Q39_Click.Count",
           "Q40_First.Click","Q40_Last.Click","Q40_Page.Submit","Q40_Click.Count",
           "Q41_First.Click","Q41_Last.Click","Q41_Page.Submit","Q41_Click.Count",
           "Q42_First.Click","Q40_Last.Click","Q42_Page.Submit","Q42_Click.Count",
           "Q43_First.Click","Q43_Last.Click","Q43_Page.Submit","Q43_Click.Count",
           "Q44_First.Click","Q44_Last.Click","Q44_Page.Submit","Q44_Click.Count")

beforedata <- beforedata[ , !names(beforedata) %in% c(drop_col)]


## Transform data
for (colname in colnames(beforedata)){
  beforedata[,colname]=as.numeric(beforedata[,colname])}

for (colname in colnames(afterdata)){
  afterdata[,colname]=as.numeric(afterdata[,colname])}


#rename colnames for before data
names(beforedata)[names(beforedata)=='Q2']<-"phone"
names(beforedata)[names(beforedata)=='Q3']<-"vehicle"
names(beforedata)[names(beforedata)=='Q4_1']<-"apps"
names(beforedata)[names(beforedata)=='Q4_2']<-"RTS"
names(beforedata)[names(beforedata)=='Q4_3']<-"walk_bike"
names(beforedata)[names(beforedata)=='Q4_4']<-"uber"
names(beforedata)[names(beforedata)=='Q5']<-"AV_terms"
names(beforedata)[names(beforedata)=='Q7']<-"comfort_ride"
names(beforedata)[names(beforedata)=='Q8_1']<-"easy_ride"
names(beforedata)[names(beforedata)=='Q8_2']<-"smooth_ride"
names(beforedata)[names(beforedata)=='Q8_3']<-"reliable_ride"
names(beforedata)[names(beforedata)=='Q8_4']<-"anxious_ride"
names(beforedata)[names(beforedata)=='Q8_5']<-"crashvehicle_ride"
names(beforedata)[names(beforedata)=='Q8_6']<-"crashped_ride"
names(beforedata)[names(beforedata)=='Q8_7']<-"cyber_ride"
names(beforedata)[names(beforedata)=='Q8_8']<-"control_ride"
names(beforedata)[names(beforedata)=='Q9']<-"comfort_ride2"
names(beforedata)[names(beforedata)=='Q10']<-"intent_ride"
names(beforedata)[names(beforedata)=='Q11']<-"comfort_drive"
names(beforedata)[names(beforedata)=='Q12_1']<-"crash_drive"
names(beforedata)[names(beforedata)=='Q12_2']<-"acc_drive"
names(beforedata)[names(beforedata)=='Q12_3']<-"follow_drive"
names(beforedata)[names(beforedata)=='Q12_4']<-"adjacent_drive"
names(beforedata)[names(beforedata)=='Q12_5']<-"comfort_change_drive"
names(beforedata)[names(beforedata)=='Q12_6']<-"signal_drive"
names(beforedata)[names(beforedata)=='Q12_7']<-"slow_drive"
names(beforedata)[names(beforedata)=='Q13']<-"comfort_drive2"
names(beforedata)[names(beforedata)=='Q14']<-"comfort_ped"
names(beforedata)[names(beforedata)=='Q15_1']<-"crash_ped"
names(beforedata)[names(beforedata)=='Q15_2']<-"easy_ped"
names(beforedata)[names(beforedata)=='Q15_3']<-"adjacent_ped"
names(beforedata)[names(beforedata)=='Q15_4']<-"cross_ped"
names(beforedata)[names(beforedata)=='Q15_5']<-"signal_ped"
names(beforedata)[names(beforedata)=='Q16']<-"comfort_ped2"
names(beforedata)[names(beforedata)=='Q19']<-"age"
names(beforedata)[names(beforedata)=='Q20']<-"education"
names(beforedata)[names(beforedata)=='Q21']<-"race"
names(beforedata)[names(beforedata)=='Q22']<-"gender"
names(beforedata)[names(beforedata)=='Q23']<-"income"
names(beforedata)[names(beforedata)=='Q24']<-"employment"

names(afterdata)[names(afterdata)=='Q1']<-"phone"
names(afterdata)[names(afterdata)=='Q2']<-"vehicle"
names(afterdata)[names(afterdata)=='Q3_1']<-"apps"
names(afterdata)[names(afterdata)=='Q3_2']<-"RTS"
names(afterdata)[names(afterdata)=='Q3_3']<-"walk_bike"
names(afterdata)[names(afterdata)=='Q3_4']<-"uber"
names(afterdata)[names(afterdata)=='Q8']<-"AV_terms"
names(afterdata)[names(afterdata)=='Q13']<-"freq_ride"
names(afterdata)[names(afterdata)=='Q19']<-"freq_drive"
names(afterdata)[names(afterdata)=='Q31']<-"freq_ped"
names(afterdata)[names(afterdata)=='Q18_1']<-"comfort_ride"
names(afterdata)[names(afterdata)=='Q18_2']<-"relax_ride"
names(afterdata)[names(afterdata)=='Q18_3']<-"satisfy_ride"
names(afterdata)[names(afterdata)=='Q23_1']<-"smooth_ride"
names(afterdata)[names(afterdata)=='Q23_2']<-"reliable_ride"
names(afterdata)[names(afterdata)=='Q23_3']<-"anxious_ride"
names(afterdata)[names(afterdata)=='Q23_4']<-"crashvehicle_ride"
names(afterdata)[names(afterdata)=='Q23_5']<-"crashped_ride"
names(afterdata)[names(afterdata)=='Q23_6']<-"control_ride"
names(afterdata)[names(afterdata)=='Q23_7']<-"attent_ride"
names(afterdata)[names(afterdata)=='Q23_8']<-"easy_ride"
names(afterdata)[names(afterdata)=='Q23_9']<-"signal_ride"
names(afterdata)[names(afterdata)=='Q27_1']<-"comfort_drive"
names(afterdata)[names(afterdata)=='Q27_2']<-"relax_drive"
names(afterdata)[names(afterdata)=='Q27_3']<-"satisfy_drive"
names(afterdata)[names(afterdata)=='Q28_1']<-"crash_drive"
names(afterdata)[names(afterdata)=='Q28_2']<-"acc_drive"
names(afterdata)[names(afterdata)=='Q28_3']<-"follow_drive"
names(afterdata)[names(afterdata)=='Q28_4']<-"adjacent_drive"
names(afterdata)[names(afterdata)=='Q28_5']<-"comfort_change_drive"
names(afterdata)[names(afterdata)=='Q28_6']<-"signal_drive"
names(afterdata)[names(afterdata)=='Q28_7']<-"slow_drive"
names(afterdata)[names(afterdata)=='Q28_8']<-"smooth_drive"
names(afterdata)[names(afterdata)=='Q28_9']<-"attent_drive"
names(afterdata)[names(afterdata)=='Q28_10']<-"easy_drive"
names(afterdata)[names(afterdata)=='Q29_1']<-"comfort_ped"
names(afterdata)[names(afterdata)=='Q29_2']<-"relax_ped"
names(afterdata)[names(afterdata)=='Q29_3']<-"satisfy_ped"
names(afterdata)[names(afterdata)=='Q30_1']<-"crash_ped"
names(afterdata)[names(afterdata)=='Q30_2']<-"easy_ped"
names(afterdata)[names(afterdata)=='Q30_3']<-"adjacent_ped"
names(afterdata)[names(afterdata)=='Q30_4']<-"cross_ped"
names(afterdata)[names(afterdata)=='Q30_5']<-"signal_ped"
names(afterdata)[names(afterdata)=='Q30_6']<-"attent_ped"
names(afterdata)[names(afterdata)=='Q33']<-"recommend"
names(afterdata)[names(afterdata)=='Q37']<-"age"
names(afterdata)[names(afterdata)=='Q38']<-"intent"
names(afterdata)[names(afterdata)=='Q39']<-"education"
names(afterdata)[names(afterdata)=='Q40']<-"race"
names(afterdata)[names(afterdata)=='Q41']<-"gender"
names(afterdata)[names(afterdata)=='Q42']<-"income"
names(afterdata)[names(afterdata)=='Q43']<-"employment"
names(afterdata)[names(afterdata)=='Q45']<-"overall_satisfaction"


#data comparison
#overall comparison
#library(arsenal)
#comparedf(beforedata,afterdata)
#summary(comparedf(beforedata,afterdata))

library(ggplot2)
library(reshape2)

#comparisons
#the question is set the same order as after survey
#Q: Do you own a smart phone?
#yes1 no2, to test the difference of means
wilcox.test(na.omit(beforedata$phone),na.omit(afterdata$phone))

#draw frequency comparison graph
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['phone']))*100)
tmp2=data.frame(prop.table(table(afterdata['phone']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),yes=c(tmp1[1,2],tmp2[1,2]),no=c(tmp1[2,2],tmp2[2,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_x_discrete(limits=c('before','after'))+geom_vline(aes(xintercept=1.5))


#Q: Do you own a vehicle that you use for traveling locally?
wilcox.test(na.omit(beforedata$vehicle),na.omit(afterdata$vehicle))

#draw frequency comparison graph
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['vehicle']))*100)
tmp2=data.frame(prop.table(table(afterdata['vehicle']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),yes=c(tmp1[1,2],tmp2[1,2]),no=c(tmp1[2,2],tmp2[2,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_x_discrete(limits=c('before','after'))+geom_vline(aes(xintercept=1.5))


#Q: How often do you use Social Media/Smart Phone Apps?
wilcox.test(na.omit(beforedata$apps),na.omit(afterdata$apps))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['apps']))*100)
tmp2=data.frame(prop.table(table(afterdata['apps']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Never=c(tmp1[1,2],tmp2[1,2]),Rarely=c(tmp1[2,2],tmp2[2,2]),
                 Occasionally=c(tmp1[3,2],tmp2[3,2]),Sometimes=c(tmp1[4,2],tmp2[4,2]), Frequently=c(tmp1[5,2],tmp2[5,2]), "Very_frequently"=c(tmp1[6,2]+tmp1[7,2],tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_x_discrete(limits=c('before','after'))+geom_vline(aes(xintercept=1.5))

#Q: How often do you use RTS for your travel locally?
wilcox.test(na.omit(beforedata$RTS),na.omit(afterdata$RTS))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['RTS']))*100)
tmp2=data.frame(prop.table(table(afterdata['RTS']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Never=c(tmp1[1,2],tmp2[1,2]),Rarely=c(tmp1[2,2],tmp2[2,2]),
                 Occasionally=c(tmp1[3,2],tmp2[3,2]),Sometimes=c(tmp1[4,2],tmp2[4,2]), Frequently=c(tmp1[5,2],tmp2[5,2]), "Very_frequently"=c(tmp1[6,2]+tmp1[7,2],tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_x_discrete(limits=c('before','after'))+geom_vline(aes(xintercept=1.5))+scale_y_continuous(limits=c(0,50))

#Q:How often do you walk or bike for your travel locally?
wilcox.test(na.omit(beforedata$walk_bike),na.omit(afterdata$walk_bike))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['walk_bike']))*100)
tmp2=data.frame(prop.table(table(afterdata['walk_bike']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Never=c(tmp1[1,2],tmp2[1,2]),Rarely=c(tmp1[2,2],tmp2[2,2]),
                 Occasionally=c(tmp1[3,2],tmp2[3,2]),Sometimes=c(tmp1[4,2],tmp2[4,2]), Frequently=c(tmp1[5,2],tmp2[5,2]), "Very_frequently"=c(tmp1[6,2]+tmp1[7,2],tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_x_discrete(limits=c('before','after'))+geom_vline(aes(xintercept=1.5))+scale_y_continuous(limits=c(0,50))


#Q:How often do you use Uber or Lyft?
wilcox.test(na.omit(beforedata$uber),na.omit(afterdata$uber))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['uber']))*100)
tmp2=data.frame(prop.table(table(afterdata['uber']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Never=c(tmp1[1,2],tmp2[1,2]),Rarely=c(tmp1[2,2],tmp2[2,2]),
                 Occasionally=c(tmp1[3,2],tmp2[3,2]),Sometimes=c(tmp1[4,2],tmp2[4,2]), Frequently=c(tmp1[5,2],tmp2[5,2]), "Very_frequently"=c(tmp1[6,2]+tmp1[7,2],tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_x_discrete(limits=c('before','after'))+geom_vline(aes(xintercept=1.5))+scale_y_continuous(limits=c(0,50))

#Q:Which of the following terms are you familiar with (select all that apply)?
beforedata$AV_terms[is.na(beforedata$AV_terms)]=0
afterdata$AV_terms[is.na(afterdata$AV_terms)]=0

wilcox.test(na.omit(beforedata$AV_terms),na.omit(afterdata$AV_terms))

#obtain the frequency table

tmp1=data.frame(prop.table(table(beforedata['AV_terms']))*100)
tmp2=data.frame(prop.table(table(afterdata['AV_terms']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Zero=c(tmp1[1,2],tmp2[1,2]),One=c(tmp1[2,2],tmp2[2,2]),
                 Two=c(tmp1[3,2],tmp2[3,2]),Three=c(tmp1[4,2],tmp2[4,2]),Four=c(tmp1[5,2],tmp2[5,2]),
                 Five=c(tmp1[6,2],tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_x_discrete(limits=c('before','after'))+geom_vline(aes(xintercept=1.5))

#Q:How often have you taken a ride in the Gainesville Autonomous Shuttle?
tmp2=data.frame(prop.table(table(afterdata['freq_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Never=c(tmp2[1,2]),Rarely=c(tmp2[2,2]),
                 Occasionally=c(tmp2[3,2]),Sometimes=c(tmp2[4,2]), Frequently=c(tmp2[5,2]), "Very_frequently"=c(tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg

#How often have you driven behind, or in front of the Gainesville Autonomous Shuttle?
tmp2=data.frame(prop.table(table(afterdata['freq_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Never=c(tmp2[1,2]),Rarely=c(tmp2[2,2]),
                 Occasionally=c(tmp2[3,2]),Sometimes=c(tmp2[4,2]), Frequently=c(tmp2[5,2]), "Very_frequently"=c(tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")

#How often have you driven behind, or in front of the Gainesville Autonomous Shuttle?
tmp2=data.frame(prop.table(table(afterdata['freq_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Never=c(tmp2[1,2]),Rarely=c(tmp2[2,2]),
                 Occasionally=c(tmp2[3,2]),Sometimes=c(tmp2[4,2]), Frequently=c(tmp2[5,2]), "Very_frequently"=c(tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")

#How often have you walked or biked near the Gainesville Autonomous Shuttle?
tmp2=data.frame(prop.table(table(afterdata['freq_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Never=c(tmp2[1,2]),Rarely=c(tmp2[2,2]),
                 Occasionally=c(tmp2[3,2]),Sometimes=c(tmp2[4,2]), Frequently=c(tmp2[5,2]), "Very_frequently"=c(tmp2[6,2]))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")

#When I am riding the Autonomous Shuttle, I am: - Comfortable
wilcox.test(na.omit(beforedata$comfort_ride),na.omit(afterdata$comfort_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['comfort_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['comfort_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))+scale_y_continuous(limits=c(0,80))

#When I am riding the Autonomous Shuttle, I am: - Relaxed

tmp2=data.frame(prop.table(table(afterdata['relax_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(tmp2[tmp2[,1]==1,2]),Moderately_disagree=c(0),
                 Somewhat_disagree=c(tmp2[tmp2[,1]==3,2]),Neutral=c(0), Somewhat_agree=c(0), 
                 Moderately_agree=c(tmp2[tmp2[,1]==6,2]),Strongly_agree=c(tmp2[tmp2[,1]==7,2]))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,80))

#When I am riding the Autonomous Shuttle, I am: - Satisfied

tmp2=data.frame(prop.table(table(afterdata['satisfy_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,80))
#The Autonomous Shuttle provides a smoother ride than a regular bus
wilcox.test(na.omit(beforedata$smooth_ride),na.omit(afterdata$smooth_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['smooth_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['smooth_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,50))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

                    
#The Autonomous Shuttle provides a more reliable service than a regular bus
wilcox.test(na.omit(beforedata$reliable_ride),na.omit(afterdata$reliable_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['reliable_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['reliable_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,50))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#The reactions of my fellow passengers make me anxious and worried
wilcox.test(na.omit(beforedata$anxious_ride),na.omit(afterdata$anxious_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['anxious_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['anxious_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,60))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

# The Autonomous Shuttle is less likely than a regular bus to crash into another vehicle on the road
wilcox.test(na.omit(beforedata$crashvehicle_ride),na.omit(afterdata$crashvehicle_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['crashvehicle_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['crashvehicle_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,60))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

# The Autonomous Shuttle is less likely than a regular bus to crash into a pedestrian or bicyclist on the road
wilcox.test(na.omit(beforedata$crashped_ride),na.omit(afterdata$crashped_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['crashped_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['crashped_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,60))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I am confident that, if needed, the service operator would be able to take over the Autonomous Shuttle in time
wilcox.test(na.omit(beforedata$control_ride),na.omit(afterdata$control_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['control_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['control_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,70))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#When riding in the Autonomous Shuttle, I pay more attention to my surroundings

tmp2=data.frame(prop.table(table(afterdata['attent_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))

#The Autonomous Shuttle is easier to use than a regular bus
wilcox.test(na.omit(beforedata$easy_ride),na.omit(afterdata$easy_ride))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['easy_ride']))*100)
tmp2=data.frame(prop.table(table(afterdata['easy_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I find that signaling the bus to stop for onboarding or offboarding is easy

tmp2=data.frame(prop.table(table(afterdata['signal_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")

#If the Autobus is operating on my typical commute route, I would use it.
tmp1=data.frame(prop.table(table(beforedata['intent_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2]),Moderately_disagree=c(tmp1[tmp1[,1]==2,2]),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2]),Neutral=c(tmp1[tmp1[,1]==4,2]), Somewhat_agree=c(tmp1[tmp1[,1]==5,2]), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2]),Strongly_agree=c(tmp1[tmp1[,1]==7,2]))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))

#When I am driving around the Autonomous Shuttle, I am: - Comfortable
wilcox.test(na.omit(beforedata$comfort_drive),na.omit(afterdata$comfort_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['comfort_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['comfort_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,80))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#When I am driving around the Autonomous Shuttle, I am: - Relaxed
tmp2=data.frame(prop.table(table(afterdata['relax_drive']))*100)
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,30))

#When I am driving around the Autonomous Shuttle, I am: - Satisfied
tmp2=data.frame(prop.table(table(afterdata['satisfy_drive']))*100)
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,80))

#The Autonomous Shuttle is less likely than a regular bus to crash into my car
wilcox.test(na.omit(beforedata$crash_drive),na.omit(afterdata$crash_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['crash_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['crash_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I would accelerate or change lanes if a  Autonomous Shuttle is right behind m
wilcox.test(na.omit(beforedata$acc_drive),na.omit(afterdata$acc_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['acc_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['acc_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

# I try to avoid following the Autonomous Shuttle
wilcox.test(na.omit(beforedata$follow_drive),na.omit(afterdata$follow_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['follow_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['follow_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I try to avoid driving around the Autonomous Shuttle
wilcox.test(na.omit(beforedata$adjacent_drive),na.omit(afterdata$adjacent_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['adjacent_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['adjacent_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I am less comfortable changing lanes and placing my vehicle in front of the Autonomous Shuttle
wilcox.test(na.omit(beforedata$comfort_change_drive),na.omit(afterdata$comfort_change_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['comfort_change_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['comfort_change_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I do not know how to communicate (signal) my driving intentions, such as lane changing, to the Autonomous Shuttle
wilcox.test(na.omit(beforedata$signal_drive),na.omit(afterdata$signal_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['signal_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['signal_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

# I drive slower around the Autonomous Shuttle than I do around a regular bus
wilcox.test(na.omit(beforedata$slow_drive),na.omit(afterdata$slow_drive))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['slow_drive']))*100)
tmp2=data.frame(prop.table(table(afterdata['slow_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I drive smoothly when driving around the Autonomous Shuttle than I do around a regular bus

tmp2=data.frame(prop.table(table(afterdata['smooth_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")

#When driving around the Autonomous Shuttle, I pay more attention to my surroundings
tmp2=data.frame(prop.table(table(afterdata['attent_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))

#Driving a car along with the Autonomous Shuttle is easier than driving along with a regular bus

tmp2=data.frame(prop.table(table(afterdata['easy_drive']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))

#When I am walking or biking around the Autonomous Shuttle, I am: - Comfortable
wilcox.test(na.omit(beforedata$comfort_ped),na.omit(afterdata$comfort_ped))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['comfort_ped']))*100)
tmp2=data.frame(prop.table(table(afterdata['comfort_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,80))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#When I am walking or biking around the Autonomous Shuttle, I am: - Relaxed
tmp2=data.frame(prop.table(table(afterdata['relax_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,80))

#When I am walking or biking around the Autonomous Shuttle, I am: - Satisfied
tmp2=data.frame(prop.table(table(afterdata['satisfy_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,50))

#The Autonomous Shuttle is less likely than a regular bus to crash into a pedestrian or bicyclist on the road
wilcox.test(na.omit(beforedata$crash_ped),na.omit(afterdata$crash_ped))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['crash_ped']))*100)
tmp2=data.frame(prop.table(table(afterdata['crash_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#As a pedestrian or bicyclist, interacting with the Autonomous Shuttle is easier than that with a conventional bus
wilcox.test(na.omit(beforedata$easy_ped),na.omit(afterdata$easy_ped))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['easy_ped']))*100)
tmp2=data.frame(prop.table(table(afterdata['easy_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

# I try to avoid biking along (in an adjacent lane) with an Autonomous Shuttle
wilcox.test(na.omit(beforedata$adjacent_ped),na.omit(afterdata$adjacent_ped))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['adjacent_ped']))*100)
tmp2=data.frame(prop.table(table(afterdata['adjacent_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I am less comfortable crossing the road at a cross walk if an Autonomous Shuttle is approaching
wilcox.test(na.omit(beforedata$cross_ped),na.omit(afterdata$cross_ped))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['cross_ped']))*100)
tmp2=data.frame(prop.table(table(afterdata['cross_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#I do not know how to communicate (signal) my intentions such as turning or entering the cross walk to the Autonomous Shuttle
wilcox.test(na.omit(beforedata$signal_ped),na.omit(afterdata$signal_ped))

#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['signal_ped']))*100)
tmp2=data.frame(prop.table(table(afterdata['signal_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#When walking or biking around the Autonomous Shuttle, I pay more attention to my surroundings
tmp2=data.frame(prop.table(table(afterdata['attent_ped']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))

#I would recommend using the Gainesville Autonomous Shuttle to others_
tmp2=data.frame(prop.table(table(afterdata['recommend']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("after"),`Strongly disagree`=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))

#Your age
wilcox.test(na.omit(beforedata$age),na.omit(afterdata$age))
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['age']))*100)
tmp2=data.frame(prop.table(table(afterdata['age']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),"Age18_25"=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),"Age26_30"=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 "Age31_35"=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),"Age36_40"=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), "Age41_45"=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 "Age46_50"=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),"Age51_55"=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)),
                 "Age56_60"=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)),"Age61_65"=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)),
                 "Age66_70"=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)),"Age71_more"=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,55))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#If the Gainesville Autonomous Shuttle is operating on my typical commute route, I would use it.
wilcox.test(na.omit(beforedata$intent),na.omit(afterdata$intent))
tmp2=data.frame(prop.table(table(afterdata['intent']))*100)
tmp1=data.frame(prop.table(table(beforedata['intent_ride']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Strongly_disagree=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(tmp1[tmp1[,1]==7,2],max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))


#Your education
wilcox.test(na.omit(beforedata$education),na.omit(afterdata$education))
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['education']))*100)
tmp2=data.frame(prop.table(table(afterdata['education']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),No_formal_education=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),High_School=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Undergraduate_Degree=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Graduate_Degree_or_more=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,50))+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#Your race
wilcox.test(na.omit(beforedata$race),na.omit(afterdata$race))
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['race']))*100)
tmp2=data.frame(prop.table(table(afterdata['race']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Caucasian=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),African_American=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Hispanic=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Asian=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)),
                 Multi_race=c(0,max(tmp2[tmp2[,1]==8,2],0)),Other=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==5,2],0)),
                 Prefer_Not_to_answer=c(tmp1[tmp1[,1]==6,2],max(tmp2[tmp2[,1]==6,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#Your gender
wilcox.test(na.omit(beforedata$gender),na.omit(afterdata$gender))
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['gender']))*100)
tmp2=data.frame(prop.table(table(afterdata['gender']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Male=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Female=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Other=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)))
freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#Your income
wilcox.test(na.omit(beforedata$income),na.omit(afterdata$income))
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['income']))*100)
tmp2=data.frame(prop.table(table(afterdata['income']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),"income_20k_or_less"=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),"income20_40k"=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 "income40_60k"=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),"income60_80k"=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)),
                 "income_80k_or_more"=c(0,max(tmp2[tmp2[,1]==5,2],0)),"prefer_not_to_answer"=c(tmp1[tmp1[,1]==5,2],max(tmp2[tmp2[,1]==6,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))

#Your employment status
wilcox.test(na.omit(beforedata$employment),na.omit(afterdata$employment))
#obtain the frequency table
tmp1=data.frame(prop.table(table(beforedata['race']))*100)
tmp2=data.frame(prop.table(table(afterdata['race']))*100)
#plot frequency graph
freq<-data.frame(study_group = c("before","after"),Student=c(tmp1[tmp1[,1]==1,2],max(tmp2[tmp2[,1]==1,2],0)),Unemployed=c(tmp1[tmp1[,1]==2,2],max(tmp2[tmp2[,1]==2,2],0)),
                 Part_Time=c(tmp1[tmp1[,1]==3,2],max(tmp2[tmp2[,1]==3,2],0)),Full_Time=c(tmp1[tmp1[,1]==4,2],max(tmp2[tmp2[,1]==4,2],0)),
                 Retired=c(0,max(tmp2[tmp2[,1]==5,2],0)),Other=c(tmp1[tmp1[,1]==5,2]))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+geom_vline(aes(xintercept=1.5))+scale_x_discrete(limits=c('before','after'))


#What is your overall satisfaction with the Gainesville Autonomous Shuttle?
tmp2=data.frame(prop.table(table(afterdata['overall_satisfaction']))*100)
freq<-data.frame(study_group = c("after"),Strongly_disagree=c(max(tmp2[tmp2[,1]==1,2],0)),Moderately_disagree=c(max(tmp2[tmp2[,1]==2,2],0)),
                 Somewhat_disagree=c(max(tmp2[tmp2[,1]==3,2],0)),Neutral=c(max(tmp2[tmp2[,1]==4,2],0)), Somewhat_agree=c(max(tmp2[tmp2[,1]==5,2],0)), 
                 Moderately_agree=c(max(tmp2[tmp2[,1]==6,2],0)),Strongly_agree=c(max(tmp2[tmp2[,1]==7,2],0)))

freq<-melt(freq,id.vars="study_group",variable.name="choice",value.name="proportion")
gg=ggplot(freq,aes(study_group,proportion,fill=choice))+ geom_bar(width=0.8,stat="identity",position="dodge")
gg+scale_y_continuous(limits=c(0,40))
# Separate brick by riders, drivers, peds ----
afterrider_data <-  afterdata[which(afterdata$freq_ride>1),]
afterdriver_data <-  afterdata[which(afterdata$freq_drive>1),]
afterped_data <-  afterdata[which(afterdata$freq_ped>1),]

#Demographics and Behavior Results
#18-30, 31-50, and 51+
afterdata[which((afterdata$age==1)|(afterdata$age==2)),'age_group']=1
afterdata[which((afterdata$age== 3)|(afterdata$age== 4)|(afterdata$age== 5)|(afterdata$age== 6)),'age_group']=2
afterdata[which(afterdata$age>6),'age_group']=3

beforedata[which((beforedata$age==1)|(beforedata$age==2)),'age_group']=1
beforedata[which((beforedata$age== 3)|(beforedata$age== 4)|(beforedata$age== 5)|(beforedata$age== 6)),'age_group']=2
beforedata[which(beforedata$age>6),'age_group']=3


Aage_data <- afterdata%>% drop_na(age)
Bage_data <- beforedata%>% drop_na(age)

tmp1=data.frame(prop.table(table(Aage_data['age_group']))*100)
tmp2=data.frame(prop.table(table(Bage_data['age_group']))*100)

Aage_group1 = Aage_data[which(Aage_data$age_group==1),]
Aage_group2 = Aage_data[which(Aage_data$age_group==2),]
Aage_group3 = Aage_data[which(Aage_data$age_group==3),]

Bage_group1 = Bage_data[which(Bage_data$age_group==1),]
Bage_group2 = Bage_data[which(Bage_data$age_group==2),]
Bage_group3 = Bage_data[which(Bage_data$age_group==3),]

describe(Aage_group1['intent'])
describe(Aage_group2['intent'])
describe(Aage_group3['intent'])
describe(Bage_group1['intent_ride'])
describe(Bage_group2['intent_ride'])
describe(Bage_group3['intent_ride'])

#after age comfort rider
wilcox.test(na.omit(Aage_group1$comfort_ride),na.omit(Aage_group2$comfort_ride),exact = FALSE)
wilcox.test(na.omit(Aage_group2$comfort_ride),na.omit(Aage_group3$comfort_ride),exact = FALSE)
wilcox.test(na.omit(Aage_group1$comfort_ride),na.omit(Aage_group3$comfort_ride),exact = FALSE)
describe(na.omit(Aage_group1$comfort_ride))
describe(na.omit(Aage_group2$comfort_ride))
describe(na.omit(Aage_group3$comfort_ride))

#before age comfort rider
describe(na.omit(Bage_group1$comfort_ride))
describe(na.omit(Bage_group2$comfort_ride))
describe(na.omit(Bage_group3$comfort_ride))

#after age comfort driver
wilcox.test(na.omit(Aage_group1$comfort_drive),na.omit(Aage_group2$comfort_drive),exact = FALSE)
wilcox.test(na.omit(Aage_group2$comfort_drive),na.omit(Aage_group3$comfort_drive),exact = FALSE)
wilcox.test(na.omit(Aage_group1$comfort_drive),na.omit(Aage_group3$comfort_drive),exact = FALSE)
describe(na.omit(Aage_group1$comfort_drive))
describe(na.omit(Aage_group2$comfort_drive))
describe(na.omit(Aage_group3$comfort_drive))

#before age comfort drive
describe(na.omit(Bage_group1$comfort_drive))
describe(na.omit(Bage_group2$comfort_drive))
describe(na.omit(Bage_group3$comfort_drive))

#after age comfort ped
wilcox.test(na.omit(Aage_group1$comfort_ped),na.omit(Aage_group2$comfort_ped))
wilcox.test(na.omit(Aage_group2$comfort_ped),na.omit(Aage_group3$comfort_ped),exact = FALSE)
wilcox.test(na.omit(Aage_group1$comfort_ped),na.omit(Aage_group3$comfort_ped),exact = FALSE)
describe(na.omit(Aage_group1$comfort_ped))
describe(na.omit(Aage_group2$comfort_ped))
describe(na.omit(Aage_group3$comfort_ped))

#before age comfort ped
describe(na.omit(Bage_group1$comfort_ped))
describe(na.omit(Bage_group2$comfort_ped))
describe(na.omit(Bage_group3$comfort_ped))

#after age intention rider
wilcox.test(Aage_group1[which(Aage_group1$freq_ride>1),]$intent,Aage_group2[which(Aage_group2$freq_ride>1),]$intent)
wilcox.test(na.omit(Aage_group2[which(Aage_group2$freq_ride>1),]$intent),na.omit(Aage_group3[which(Aage_group3$freq_ride>1),]$intent))
wilcox.test(Aage_group1[which(Aage_group1$freq_ride>1),]$intent,Aage_group3[which(Aage_group3$freq_ride>1),]$intent)
describe(na.omit(Aage_group1[which(Aage_group1$freq_ride>1),]$intent))
describe(na.omit(Aage_group2[which(Aage_group2$freq_ride>1),]$intent))
describe(na.omit(Aage_group3[which(Aage_group3$freq_ride>1),]$intent))

#after age intention driver
wilcox.test(Aage_group1[which(Aage_group1$freq_drive>1),]$intent,Aage_group2[which(Aage_group2$freq_drive>1),]$intent)
wilcox.test(na.omit(Aage_group2[which(Aage_group2$freq_drive>1),]$intent),na.omit(Aage_group3[which(Aage_group3$freq_drive>1),]$intent))
wilcox.test(Aage_group1[which(Aage_group1$freq_drive>1),]$intent,Aage_group3[which(Aage_group3$freq_drive>1),]$intent)
describe(na.omit(Aage_group1[which(Aage_group1$freq_drive>1),]$intent))
describe(na.omit(Aage_group2[which(Aage_group2$freq_drive>1),]$intent))
describe(na.omit(Aage_group3[which(Aage_group3$freq_drive>1),]$intent))

#after age intention ped
wilcox.test(Aage_group1[which(Aage_group1$freq_ped>1),]$intent,Aage_group2[which(Aage_group2$freq_ped>1),]$intent)
wilcox.test(na.omit(Aage_group2[which(Aage_group2$freq_ped>1),]$intent),na.omit(Aage_group3[which(Aage_group3$freq_ped>1),]$intent))
wilcox.test(Aage_group1[which(Aage_group1$freq_ped>1),]$intent,Aage_group3[which(Aage_group3$freq_ped>1),]$intent)
describe(na.omit(Aage_group1[which(Aage_group1$freq_ped>1),]$intent))
describe(na.omit(Aage_group2[which(Aage_group2$freq_ped>1),]$intent))
describe(na.omit(Aage_group3[which(Aage_group3$freq_ped>1),]$intent))

#Before age intention
wilcox.test(na.omit(Bage_data[which(Bage_data$age_group==1),]$intent),na.omit(Bage_data[which(Bage_data$age_group==2),]$intent))
wilcox.test(na.omit(Bage_data[which(Bage_data$age_group==1),]$intent),na.omit(Bage_data[which(Bage_data$age_group==3),]$intent))
wilcox.test(na.omit(Bage_data[which(Bage_data$age_group==2),]$intent),na.omit(Bage_data[which(Bage_data$age_group==3),]$intent))
describe(na.omit(Bage_data[which(Bage_data$age_group==1),]$intent))
describe(na.omit(Bage_data[which(Bage_data$age_group==2),]$intent))
describe(na.omit(Bage_data[which(Bage_data$age_group==3),]$intent))

#Before and after age_group1 comfort
wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==1),]$comfort_ride),na.omit(Bage_data[which(Bage_data$age_group==1),]$comfort_ride))
describe(na.omit(Aage_data[which(Aage_data$age_group==1),]$comfort_ride))
describe(na.omit(Bage_data[which(Bage_data$age_group==1),]$comfort_ride))

wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==1),]$comfort_drive),na.omit(Bage_data[which(Bage_data$age_group==1),]$comfort_drive))
describe(na.omit(Aage_data[which(Aage_data$age_group==1),]$comfort_drive))
describe(na.omit(Bage_data[which(Bage_data$age_group==1),]$comfort_drive))

wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==1),]$comfort_ped),na.omit(Bage_data[which(Bage_data$age_group==1),]$comfort_ped))
describe(na.omit(Aage_data[which(Aage_data$age_group==1),]$comfort_ped))
describe(na.omit(Bage_data[which(Bage_data$age_group==1),]$comfort_ped))

#Before and after age_group2
wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==2),]$comfort_ride),na.omit(Bage_data[which(Bage_data$age_group==2),]$comfort_ride))
describe(na.omit(Aage_data[which(Aage_data$age_group==2),]$comfort_ride))
describe(na.omit(Bage_data[which(Bage_data$age_group==2),]$comfort_ride))

wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==2),]$comfort_drive),na.omit(Bage_data[which(Bage_data$age_group==2),]$comfort_drive))
describe(na.omit(Aage_data[which(Aage_data$age_group==2),]$comfort_drive))
describe(na.omit(Bage_data[which(Bage_data$age_group==2),]$comfort_drive))

wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==2),]$comfort_ped),na.omit(Bage_data[which(Bage_data$age_group==2),]$comfort_ped))
describe(na.omit(Aage_data[which(Aage_data$age_group==2),]$comfort_ped))
describe(na.omit(Bage_data[which(Bage_data$age_group==2),]$comfort_ped))

#Before and after age_group3
wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==3),]$comfort_ride),na.omit(Bage_data[which(Bage_data$age_group==3),]$comfort_ride))
describe(na.omit(Aage_data[which(Aage_data$age_group==3),]$comfort_ride))
describe(na.omit(Bage_data[which(Bage_data$age_group==3),]$comfort_ride))

wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==3),]$comfort_drive),na.omit(Bage_data[which(Bage_data$age_group==3),]$comfort_drive))
describe(na.omit(Aage_data[which(Aage_data$age_group==3),]$comfort_drive))
describe(na.omit(Bage_data[which(Bage_data$age_group==3),]$comfort_drive))

wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==3),]$comfort_ped),na.omit(Bage_data[which(Bage_data$age_group==3),]$comfort_ped))
describe(na.omit(Aage_data[which(Aage_data$age_group==3),]$comfort_ped))
describe(na.omit(Bage_data[which(Bage_data$age_group==3),]$comfort_ped))

#Before and after age_group1 intention 
wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==1),]$intent),Bage_data[which(Bage_data$age_group==1),]$intent_ride)
describe(na.omit(Aage_data[which(Aage_data$age_group==1),]$intent))
describe(na.omit(Bage_data[which(Bage_data$age_group==1),]$intent_ride))

#Before and after age_group2 intention
wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==2),]$intent),Bage_data[which(Bage_data$age_group==2),]$intent_ride)
describe(na.omit(Aage_data[which(Aage_data$age_group==2),]$intent))
describe(na.omit(Bage_data[which(Bage_data$age_group==2),]$intent_ride))

#Before and after age_group3 intention
wilcox.test(na.omit(Aage_data[which(Aage_data$age_group==3),]$intent), Bage_data[which(Bage_data$age_group==3),]$intent_ride)
describe(na.omit(Aage_data[which(Aage_data$age_group==3),]$intent))
describe(na.omit( Bage_data[which(Bage_data$age_group==3),]$intent_ride))

#clear data for age
rm(Aage_data,Bage_data,Aage_group1,Aage_group2,Aage_group3,Bage_group1,Bage_group2,Bage_group3)

#gender
Agender_data <- afterdata%>% drop_na(gender)
Bgender_data <- beforedata%>% drop_na(gender)

Atmp1 = Agender_data[which(Agender_data$gender==1),]
Atmp2 = Agender_data[which(Agender_data$gender==2),]
Btmp1 = Bgender_data[which(Bgender_data$gender==1),]
Btmp2 = Bgender_data[which(Bgender_data$gender==2),]

describe(Atmp1['intent'])
describe(Atmp2['intent'])
describe(Btmp1['intent_ride'])
describe(Btmp2['intent_ride'])


#after gender comfort rider
wilcox.test(na.omit(Atmp1$comfort_ride),na.omit(Atmp2$comfort_ride),exact = FALSE)
describe(na.omit(Atmp1$comfort_ride))
describe(na.omit(Atmp2$comfort_ride))

#before gender comfort rider
wilcox.test(na.omit(Btmp1$comfort_ride),na.omit(Btmp2$comfort_ride),exact = FALSE)
describe(na.omit(Btmp1$comfort_ride))
describe(na.omit(Btmp2$comfort_ride))

#after gender comfort driver
wilcox.test(na.omit(Atmp1$comfort_drive),na.omit(Atmp2$comfort_drive),exact = FALSE)
describe(na.omit(Atmp1$comfort_drive))
describe(na.omit(Atmp2$comfort_drive))

#before gender comfort driver
wilcox.test(na.omit(Btmp1$comfort_drive),na.omit(Btmp2$comfort_drive),exact = FALSE)
describe(na.omit(Btmp1$comfort_drive))
describe(na.omit(Btmp2$comfort_drive))

#after gender comfort ped
wilcox.test(na.omit(Atmp1$comfort_ped),na.omit(Atmp2$comfort_ped),exact = FALSE)
describe(na.omit(Atmp1$comfort_ped))
describe(na.omit(Atmp2$comfort_ped))

#before gender comfort ped
wilcox.test(na.omit(Btmp1$comfort_ped),na.omit(Btmp2$comfort_ped),exact = FALSE)
describe(na.omit(Btmp1$comfort_ped))
describe(na.omit(Btmp2$comfort_ped))

#after gender intent rider
wilcox.test(na.omit(Atmp1[which(Atmp1$freq_ride>1),]$intent),na.omit(Atmp2[which(tmp2$freq_ride>1),]$intent),exact = FALSE)
describe(na.omit(Atmp1[which(Atmp1$freq_ride>1),]$intent))
describe(na.omit(Atmp2[which(Atmp2$freq_ride>1),]$intent))

#after gender intent driver
wilcox.test(na.omit(Atmp1[which(tmp1$freq_drive>1),]$intent),na.omit(Atmp2[which(tmp2$freq_drive>1),]$intent),exact = FALSE)
describe(na.omit(Atmp1[which(tmp1$freq_drive>1),]$intent))
describe(na.omit(Atmp2[which(tmp2$freq_drive>1),]$intent))

#after gender intent ped
tmp1 = Agender_data[which(Agender_data$gender==1),]
tmp2 = Agender_data[which(Agender_data$gender==2),]
wilcox.test(tmp1[which(tmp1$freq_ped>1),]$intent,tmp2[which(tmp2$freq_ped>1),]$intent,exact = FALSE)
describe(tmp1[which(tmp1$freq_ped>1),]$intent)
describe(tmp2[which(tmp2$freq_ped>1),]$intent)

wilcox.test(Agender_data[which(Agender_data$gender==1),]$intent,Agender_data[which(Agender_data$gender==2),]$intent)
wilcox.test(Bgender_data[which(Bgender_data$gender==1),]$intent,Bgender_data[which(Bgender_data$gender==2),]$intent)
describe(Agender_data[which(Agender_data$gender==1),]$intent)
describe(Agender_data[which(Agender_data$gender==2),]$intent)
describe(Bgender_data[which(Bgender_data$gender==1),]$intent)
describe(Bgender_data[which(Bgender_data$gender==2),]$intent)
#Before and after intention
#male
wilcox.test(na.omit(Agender_data[which(Agender_data$gender==1),]$intent),Bgender_data[which(Bgender_data$gender==1),]$intent_ride)
describe(na.omit(Agender_data[which(Agender_data$gender==1),]$intent))#male
describe(na.omit(Bgender_data[which(Bgender_data$gender==1),]$intent_ride))

#female
Atmp = Agender_data[which(Agender_data$gender==2),]
Btmp = Bgender_data[which(Bgender_data$gender==2),]
wilcox.test(na.omit(Atmp$intent),Btmp$intent_ride)
describe(na.omit(Atmp$intent))#female
describe(na.omit(Btmp$intent_ride))

#Before and after comfort
#male
wilcox.test(na.omit(Agender_data[which(Agender_data$gender==1),]$comfort_ride),Bgender_data[which(Bgender_data$gender==1),]$comfort_ride)
wilcox.test(na.omit(Agender_data[which(Agender_data$gender==1),]$comfort_drive),Bgender_data[which(Bgender_data$gender==1),]$comfort_drive)
wilcox.test(na.omit(Agender_data[which(Agender_data$gender==1),]$comfort_ped),Bgender_data[which(Bgender_data$gender==1),]$comfort_ped)
describe(na.omit(Agender_data[which(Agender_data$gender==1),]$comfort_ride))#female
describe(na.omit(Agender_data[which(Agender_data$gender==1),]$comfort_drive))
describe(na.omit(Agender_data[which(Agender_data$gender==1),]$comfort_ped))

#female 
wilcox.test(na.omit(Agender_data[which(Agender_data$gender==2),]$comfort_ride),Bgender_data[which(Bgender_data$gender==2),]$comfort_ride)
wilcox.test(na.omit(Agender_data[which(Agender_data$gender==2),]$comfort_drive),Bgender_data[which(Bgender_data$gender==2),]$comfort_drive)
wilcox.test(na.omit(Agender_data[which(Agender_data$gender==2),]$comfort_ped),Bgender_data[which(Bgender_data$gender==2),]$comfort_ped)
describe(na.omit(Agender_data[which(Agender_data$gender==2),]$comfort_ride))#female
describe(na.omit(Agender_data[which(Agender_data$gender==2),]$comfort_drive))
describe(na.omit(Agender_data[which(Agender_data$gender==2),]$comfort_ped))

#Race
#Caucasian1, African_American2,Hispanic3, Asian4,Other5
Arace_data <- afterdata%>% drop_na(race)
Brace_data <- beforedata%>% drop_na(race)

#after race comfort rider
tmp1 = Arace_data[which(Arace_data$race==1),]
tmp2 = Arace_data[which(Arace_data$race==2),]#too few data
tmp3 = Arace_data[which(Arace_data$race==3),]
tmp4 = Arace_data[which(Arace_data$race==4),]
tmp5 = Arace_data[which(Arace_data$race==5),]
wilcox.test(na.omit(tmp1$comfort_ride),na.omit(tmp3$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_ride),na.omit(tmp4$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_ride),na.omit(tmp5$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_ride),na.omit(tmp4$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_ride),na.omit(tmp5$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp4$comfort_ride),na.omit(tmp5$comfort_ride),exact = FALSE)

#after race comfort drive
wilcox.test(na.omit(tmp1$comfort_drive),na.omit(tmp3$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_drive),na.omit(tmp4$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_drive),na.omit(tmp5$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_drive),na.omit(tmp4$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_drive),na.omit(tmp5$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp4$comfort_drive),na.omit(tmp5$comfort_drive),exact = FALSE)

#after race comfort ped
wilcox.test(na.omit(tmp1$comfort_ped),na.omit(tmp3$comfort_ped),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_ped),na.omit(tmp4$comfort_ped),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_ped),na.omit(tmp5$comfort_ped),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_ped),na.omit(tmp4$comfort_ped),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_ped),na.omit(tmp5$comfort_ped),exact = FALSE)
wilcox.test(na.omit(tmp4$comfort_ped),na.omit(tmp5$comfort_ped),exact = FALSE)

#Before and after race-comfort
#Caucasian
wilcox.test(na.omit(Arace_data[which(Arace_data$race==1),]$comfort_ride),Brace_data[which(Brace_data$race==1),]$comfort_ride)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==1),]$comfort_drive),Brace_data[which(Brace_data$race==1),]$comfort_drive)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==1),]$comfort_ped),Brace_data[which(Brace_data$race==1),]$comfort_ped)
describe(Arace_data[which(Arace_data$race==1),]$comfort_ride)
describe(Arace_data[which(Arace_data$race==1),]$comfort_drive)
describe(Arace_data[which(Arace_data$race==1),]$comfort_ped)
describe(Brace_data[which(Brace_data$race==1),]$comfort_ride)
describe(Brace_data[which(Brace_data$race==1),]$comfort_drive)
describe(Brace_data[which(Brace_data$race==1),]$comfort_ped)

#African American dataset is so small

#Hispanic
wilcox.test(na.omit(Arace_data[which(Arace_data$race==3),]$comfort_ride),Brace_data[which(Brace_data$race==3),]$comfort_ride)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==3),]$comfort_drive),Brace_data[which(Brace_data$race==3),]$comfort_drive)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==3),]$comfort_ped),na.omit(Brace_data[which(Brace_data$race==3),]$comfort_ped))
describe(Arace_data[which(Arace_data$race==3),]$comfort_ride)
describe(Arace_data[which(Arace_data$race==3),]$comfort_drive)
describe(Arace_data[which(Arace_data$race==3),]$comfort_ped)
describe(Brace_data[which(Brace_data$race==3),]$comfort_ride)
describe(Brace_data[which(Brace_data$race==3),]$comfort_drive)
describe(Brace_data[which(Brace_data$race==3),]$comfort_ped)

#Asian
wilcox.test(na.omit(Arace_data[which(Arace_data$race==4),]$comfort_ride),Brace_data[which(Brace_data$race==4),]$comfort_ride)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==4),]$comfort_drive),Brace_data[which(Brace_data$race==4),]$comfort_drive)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==4),]$comfort_ped),Brace_data[which(Brace_data$race==4),]$comfort_ped)
describe(Arace_data[which(Arace_data$race==4),]$comfort_ride)
describe(Arace_data[which(Arace_data$race==4),]$comfort_drive)
describe(Arace_data[which(Arace_data$race==4),]$comfort_ped)
describe(Brace_data[which(Brace_data$race==4),]$comfort_ride)
describe(Brace_data[which(Brace_data$race==4),]$comfort_drive)
describe(Brace_data[which(Brace_data$race==4),]$comfort_ped)

#Other (dataset is so small)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==5),]$comfort_ride),Brace_data[which(Brace_data$race==5),]$comfort_ride)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==5),]$comfort_drive),Brace_data[which(Brace_data$race==5),]$comfort_drive)
wilcox.test(na.omit(Arace_data[which(Arace_data$race==5),]$comfort_ped),Brace_data[which(Brace_data$race==5),]$comfort_ped)
describe(Arace_data[which(Arace_data$race==5),]$comfort_ride)
describe(Arace_data[which(Arace_data$race==5),]$comfort_drive)
describe(Arace_data[which(Arace_data$race==5),]$comfort_ped)
describe(Brace_data[which(Brace_data$race==5),]$comfort_ride)
describe(Brace_data[which(Brace_data$race==5),]$comfort_drive)
describe(Brace_data[which(Brace_data$race==5),]$comfort_ped)

#race intention
#afte race intention
wilcox.test(Arace_data[which(Arace_data$race==1),]$intent,Arace_data[which(Arace_data$race==2),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==1),]$intent,Arace_data[which(Arace_data$race==3),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==1),]$intent,Arace_data[which(Arace_data$race==4),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==1),]$intent,Arace_data[which(Arace_data$race==5),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==2),]$intent,Arace_data[which(Arace_data$race==3),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==3),]$intent,Arace_data[which(Arace_data$race==4),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==4),]$intent,Arace_data[which(Arace_data$race==5),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==3),]$intent,Arace_data[which(Arace_data$race==4),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==3),]$intent,Arace_data[which(Arace_data$race==5),]$intent)
wilcox.test(Arace_data[which(Arace_data$race==4),]$intent,Arace_data[which(Arace_data$race==5),]$intent)

#before race intention
wilcox.test(Brace_data[which(Brace_data$race==1),]$intent,Brace_data[which(Brace_data$race==2),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==1),]$intent,Brace_data[which(Brace_data$race==3),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==1),]$intent,Brace_data[which(Brace_data$race==4),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==1),]$intent,Brace_data[which(Brace_data$race==5),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==2),]$intent,Brace_data[which(Brace_data$race==3),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==3),]$intent,Brace_data[which(Brace_data$race==4),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==4),]$intent,Brace_data[which(Brace_data$race==5),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==3),]$intent,Brace_data[which(Brace_data$race==4),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==3),]$intent,Brace_data[which(Brace_data$race==5),]$intent)
wilcox.test(Brace_data[which(Brace_data$race==4),]$intent_ride,Brace_data[which(Brace_data$race==5),]$intent_ride)

#after before intention
wilcox.test(Arace_data[which(Arace_data$race==1),]$intent,Brace_data[which(Brace_data$race==1),]$intent_ride)
wilcox.test(Arace_data[which(Arace_data$race==2),]$intent,Brace_data[which(Brace_data$race==2),]$intent_ride)
wilcox.test(Arace_data[which(Arace_data$race==3),]$intent,Brace_data[which(Brace_data$race==3),]$intent_ride)
wilcox.test(Arace_data[which(Arace_data$race==4),]$intent,Brace_data[which(Brace_data$race==4),]$intent_ride)
wilcox.test(Arace_data[which(Arace_data$race==5),]$intent,Brace_data[which(Brace_data$race==5),]$intent_ride)
describe(Arace_data[which(Arace_data$race==1),]$intent)
describe(Brace_data[which(Brace_data$race==1),]$intent_ride)
describe(Arace_data[which(Arace_data$race==2),]$intent)
describe(Brace_data[which(Brace_data$race==2),]$intent_ride)
describe(Arace_data[which(Arace_data$race==3),]$intent)
describe(Brace_data[which(Brace_data$race==3),]$intent_ride)
describe(Arace_data[which(Arace_data$race==4),]$intent)
describe(Brace_data[which(Brace_data$race==4),]$intent_ride)
describe(Arace_data[which(Arace_data$race==5),]$intent)
describe(Brace_data[which(Brace_data$race==5),]$intent_ride)


#Education
#No formal education1, High School2, Undergraduate Degree3, Graduate Degree or more4 
Aedu_data <- afterdata%>% drop_na(education)
Bedu_data <- beforedata%>% drop_na(education)

#after edu comfort rider
tmp1 = Aedu_data[which(Aedu_data$education==1),]
tmp2 = Aedu_data[which(Aedu_data$education==2),]
tmp3 = Aedu_data[which(Aedu_data$education==3),]
tmp4 = Aedu_data[which(Aedu_data$education==4),]
wilcox.test(na.omit(tmp1$comfort_ride),na.omit(tmp2$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_ride),na.omit(tmp3$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_ride),na.omit(tmp4$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp2$comfort_ride),na.omit(tmp3$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp2$comfort_ride),na.omit(tmp4$comfort_ride),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_ride),na.omit(tmp4$comfort_ride),exact = FALSE)

#after edu comfort drive
wilcox.test(na.omit(tmp1$comfort_drive),na.omit(tmp2$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_drive),na.omit(tmp3$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp1$comfort_drive),na.omit(tmp4$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp2$comfort_drive),na.omit(tmp3$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp2$comfort_drive),na.omit(tmp4$comfort_drive),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_drive),na.omit(tmp4$comfort_drive),exact = FALSE)

#after edu comfort ped
wilcox.test(na.omit(tmp2$comfort_ped),na.omit(tmp3$comfort_ped),exact = FALSE)
wilcox.test(na.omit(tmp2$comfort_ped),na.omit(tmp4$comfort_ped),exact = FALSE)
wilcox.test(na.omit(tmp3$comfort_ped),na.omit(tmp4$comfort_ped),exact = FALSE)

#Before and after education-comfort
#High School2
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==2),]$comfort_ride),Bedu_data[which(Bedu_data$education==2),]$comfort_ride)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==2),]$comfort_drive),Bedu_data[which(Bedu_data$education==2),]$comfort_drive)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==2),]$comfort_ped),Bedu_data[which(Bedu_data$education==2),]$comfort_ped)
describe(Aedu_data[which(Aedu_data$education==2),]$comfort_ride)
describe(Aedu_data[which(Aedu_data$education==2),]$comfort_drive)
describe(Aedu_data[which(Aedu_data$education==2),]$comfort_ped)
describe(Bedu_data[which(Bedu_data$education==2),]$comfort_ride)
describe(Bedu_data[which(Bedu_data$education==2),]$comfort_drive)
describe(Bedu_data[which(Bedu_data$education==2),]$comfort_ped)

#Undergraduate Degree3
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==3),]$comfort_ride),Bedu_data[which(Bedu_data$education==3),]$comfort_ride)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==3),]$comfort_drive),Bedu_data[which(Bedu_data$education==3),]$comfort_drive)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==3),]$comfort_ped),Bedu_data[which(Bedu_data$education==3),]$comfort_ped)
describe(Aedu_data[which(Aedu_data$education==3),]$comfort_ride)
describe(Aedu_data[which(Aedu_data$education==3),]$comfort_drive)
describe(Aedu_data[which(Aedu_data$education==3),]$comfort_ped)
describe(Bedu_data[which(Bedu_data$education==3),]$comfort_ride)
describe(Bedu_data[which(Bedu_data$education==3),]$comfort_drive)
describe(Bedu_data[which(Bedu_data$education==3),]$comfort_ped)

#Graduate Degree or more4
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==4),]$comfort_ride),Bedu_data[which(Bedu_data$education==4),]$comfort_ride)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==4),]$comfort_drive),Bedu_data[which(Bedu_data$education==4),]$comfort_drive)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==4),]$comfort_ped),Bedu_data[which(Bedu_data$education==4),]$comfort_ped)
describe(Aedu_data[which(Aedu_data$education==4),]$comfort_ride)
describe(Aedu_data[which(Aedu_data$education==4),]$comfort_drive)
describe(Aedu_data[which(Aedu_data$education==4),]$comfort_ped)
describe(Bedu_data[which(Bedu_data$education==4),]$comfort_ride)
describe(Bedu_data[which(Bedu_data$education==4),]$comfort_drive)
describe(Bedu_data[which(Bedu_data$education==4),]$comfort_ped)

#Education-intention
#After intention
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==1),]$intent),Aedu_data[which(Aedu_data$education==2),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==1),]$intent),Aedu_data[which(Aedu_data$education==3),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==1),]$intent),Aedu_data[which(Aedu_data$education==4),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==2),]$intent),Aedu_data[which(Aedu_data$education==3),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==2),]$intent),Aedu_data[which(Aedu_data$education==4),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==3),]$intent),Aedu_data[which(Aedu_data$education==4),]$intent)

#Before and after education intention
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==1),]$intent),Bedu_data[which(Bedu_data$education==1),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==2),]$intent),Bedu_data[which(Bedu_data$education==2),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==3),]$intent),Bedu_data[which(Bedu_data$education==3),]$intent)
wilcox.test(na.omit(Aedu_data[which(Aedu_data$education==4),]$intent),Bedu_data[which(Bedu_data$education==4),]$intent)
describe(Aedu_data[which(Aedu_data$education==1),]$intent)
describe(Aedu_data[which(Aedu_data$education==2),]$intent)
describe(Aedu_data[which(Aedu_data$education==3),]$intent)
describe(Aedu_data[which(Aedu_data$education==4),]$intent)
describe(Bedu_data[which(Bedu_data$education==1),]$intent)
describe(Bedu_data[which(Bedu_data$education==2),]$intent)
describe(Bedu_data[which(Bedu_data$education==3),]$intent)
describe(Bedu_data[which(Bedu_data$education==4),]$intent)

rm(Aedu_data,Bedu_data,tmp1,tmp2,tmp3,tmp4)

#Income
#1:<20k, 2:20k to 40k, 3:40k to 60k, 4:60k to 80k, 5:>80k 
Ainc_data <- afterdata%>% drop_na(income)
Binc_data <- beforedata%>% drop_na(income)

Ainc1 <-Ainc_data[which(Ainc_data$income==1),]
Ainc2 <-Ainc_data[which(Ainc_data$income==2),]
Ainc3 <-Ainc_data[which(Ainc_data$income==3),]
Ainc4 <-Ainc_data[which(Ainc_data$income==4),]
Ainc5 <-Ainc_data[which(Ainc_data$income==5),]
Binc1 <-Binc_data[which(Binc_data$income==1),]
Binc2 <-Binc_data[which(Binc_data$income==2),]
Binc3 <-Binc_data[which(Binc_data$income==3),]
Binc4 <-Binc_data[which(Binc_data$income==4),]
Binc5 <-Binc_data[which(Binc_data$income==5),]

describe(Ainc1['intent'])
describe(Ainc2['intent'])
describe(Ainc3['intent'])
describe(Ainc4['intent'])
describe(Ainc5['intent'])
describe(Binc1['intent_ride'])
describe(Binc2['intent_ride'])
describe(Binc3['intent_ride'])
describe(Binc4['intent_ride'])
describe(Binc5['intent_ride'])

#After income comfort rider
wilcox.test(na.omit(Ainc1$comfort_ride),Ainc2$comfort_ride)
wilcox.test(na.omit(Ainc1$comfort_ride),Ainc3$comfort_ride)
wilcox.test(na.omit(Ainc1$comfort_ride),Ainc4$comfort_ride)
wilcox.test(na.omit(Ainc1$comfort_ride),Ainc5$comfort_ride)
wilcox.test(na.omit(Ainc2$comfort_ride),Ainc3$comfort_ride)
wilcox.test(na.omit(Ainc2$comfort_ride),Ainc4$comfort_ride)
wilcox.test(na.omit(Ainc2$comfort_ride),Ainc5$comfort_ride)
wilcox.test(na.omit(Ainc3$comfort_ride),Ainc4$comfort_ride)
wilcox.test(na.omit(Ainc3$comfort_ride),Ainc5$comfort_ride)
wilcox.test(na.omit(Ainc4$comfort_ride),Ainc5$comfort_ride)
describe(Ainc1$comfort_ride)
describe(Ainc2$comfort_ride)
describe(Ainc3$comfort_ride)
describe(Ainc4$comfort_ride)
describe(Ainc5$comfort_ride)

#After income comfort driver
wilcox.test(na.omit(Ainc1$comfort_drive),Ainc2$comfort_drive)
wilcox.test(na.omit(Ainc1$comfort_drive),Ainc3$comfort_drive)
wilcox.test(na.omit(Ainc1$comfort_drive),Ainc4$comfort_drive)
wilcox.test(na.omit(Ainc1$comfort_drive),Ainc5$comfort_drive)
wilcox.test(na.omit(Ainc2$comfort_drive),Ainc3$comfort_drive)
wilcox.test(na.omit(Ainc2$comfort_drive),Ainc4$comfort_drive)
wilcox.test(na.omit(Ainc2$comfort_drive),Ainc5$comfort_drive)
wilcox.test(na.omit(Ainc3$comfort_drive),Ainc4$comfort_drive)
wilcox.test(na.omit(Ainc3$comfort_drive),Ainc5$comfort_drive)
wilcox.test(na.omit(Ainc4$comfort_drive),Ainc5$comfort_drive)
describe(Ainc1$comfort_drive)
describe(Ainc2$comfort_drive)
describe(Ainc3$comfort_drive)
describe(Ainc4$comfort_drive)
describe(Ainc5$comfort_drive)

#After income comfort ped
wilcox.test(na.omit(Ainc1$comfort_ped),Ainc2$comfort_ped)
wilcox.test(na.omit(Ainc1$comfort_ped),Ainc3$comfort_ped)
wilcox.test(na.omit(Ainc1$comfort_ped),Ainc4$comfort_ped)
wilcox.test(na.omit(Ainc1$comfort_ped),Ainc5$comfort_ped)
wilcox.test(na.omit(Ainc2$comfort_ped),Ainc3$comfort_ped)
wilcox.test(na.omit(Ainc2$comfort_ped),Ainc4$comfort_ped)
wilcox.test(na.omit(Ainc2$comfort_ped),Ainc5$comfort_ped)
wilcox.test(na.omit(Ainc3$comfort_ped),Ainc4$comfort_ped)
wilcox.test(na.omit(Ainc3$comfort_ped),Ainc5$comfort_ped)
wilcox.test(na.omit(Ainc4$comfort_ped),Ainc5$comfort_ped)
describe(Ainc1$comfort_ped)
describe(Ainc2$comfort_ped)
describe(Ainc3$comfort_ped)
describe(Ainc4$comfort_ped)
describe(Ainc5$comfort_ped)

#Before and after comfort
#1:<20k
wilcox.test(na.omit(Ainc1$comfort_ride),Binc1$comfort_ride)
wilcox.test(na.omit(Ainc1$comfort_drive),Binc1$comfort_drive)
wilcox.test(na.omit(Ainc1$comfort_ped),Binc1$comfort_ped)
describe(Ainc1$comfort_ride)
describe(Ainc1$comfort_drive)
describe(Ainc1$comfort_ped)
describe(Binc1$comfort_ride)
describe(Binc1$comfort_drive)
describe(Binc1$comfort_ped)

#2:20k-40k
wilcox.test(na.omit(Ainc2$comfort_ride),Binc2$comfort_ride)
wilcox.test(na.omit(Ainc2$comfort_drive),Binc2$comfort_drive)
wilcox.test(na.omit(Ainc2$comfort_ped),Binc2$comfort_ped)
describe(Ainc2$comfort_ride)
describe(Ainc2$comfort_drive)
describe(Ainc2$comfort_ped)
describe(Binc2$comfort_ride)
describe(Binc2$comfort_drive)
describe(Binc2$comfort_ped)

#3:40-60k
wilcox.test(na.omit(Ainc3$comfort_ride),Binc3$comfort_ride)
wilcox.test(na.omit(Ainc3$comfort_drive),Binc3$comfort_drive)
wilcox.test(na.omit(Ainc3$comfort_ped),Binc3$comfort_ped)
describe(Ainc3$comfort_ride)
describe(Ainc3$comfort_drive)
describe(Ainc3$comfort_ped)
describe(Binc3$comfort_ride)
describe(Binc3$comfort_drive)
describe(Binc3$comfort_ped)

#4:60k to 80k
wilcox.test(na.omit(Ainc4$comfort_ride),Binc4$comfort_ride)
wilcox.test(na.omit(Ainc4$comfort_drive),Binc4$comfort_drive)
wilcox.test(na.omit(Ainc4$comfort_ped),Binc4$comfort_ped)
describe(Ainc4$comfort_ride)
describe(Ainc4$comfort_drive)
describe(Ainc4$comfort_ped)
describe(Binc4$comfort_ride)
describe(Binc4$comfort_drive)
describe(Binc4$comfort_ped)

#4:80k+
wilcox.test(na.omit(Ainc5$comfort_ride),Binc5$comfort_ride)
wilcox.test(na.omit(Ainc5$comfort_drive),Binc5$comfort_drive)
wilcox.test(na.omit(Ainc5$comfort_ped),Binc5$comfort_ped)
describe(Ainc5$comfort_ride)
describe(Ainc5$comfort_drive)
describe(Ainc5$comfort_ped)
describe(Binc5$comfort_ride)
describe(Binc5$comfort_drive)
describe(Binc5$comfort_ped)

#After Income intention
wilcox.test(Ainc1$intent,Ainc2$intent)
wilcox.test(Ainc1$intent,Ainc3$intent)
wilcox.test(Ainc1$intent,Ainc4$intent)
wilcox.test(Ainc1$intent,Ainc5$intent)
wilcox.test(Ainc2$intent,Ainc3$intent)
wilcox.test(Ainc2$intent,Ainc4$intent)
wilcox.test(Ainc2$intent,Ainc5$intent)
wilcox.test(Ainc3$intent,Ainc4$intent)
wilcox.test(Ainc3$intent,Ainc5$intent)
wilcox.test(Ainc4$intent,Ainc5$intent)

#Before and after Income intention
wilcox.test(Ainc1$intent,Binc1$intent)
wilcox.test(Ainc2$intent,Binc2$intent)
wilcox.test(Ainc3$intent,Binc3$intent)
wilcox.test(Ainc4$intent,Binc4$intent)
wilcox.test(Ainc5$intent,Binc5$intent)

rm(Ainc_data,Binc_data,Ainc1,Ainc2,Ainc3,Ainc4,Ainc5,Binc1,Binc2,Binc3,Binc4,Binc5)

#Employment
#1Student, 2Unemployed, 3Part-Time, 4Full-Time, 5Retired
Aemp_data <- afterdata%>% drop_na(employment)
Bemp_data <- beforedata%>% drop_na(employment)

Aemp1 <-Aemp_data[which(Aemp_data$income==1),]
Aemp2 <-Aemp_data[which(Aemp_data$income==2),]
Aemp3 <-Aemp_data[which(Aemp_data$income==3),]
Aemp4 <-Aemp_data[which(Aemp_data$income==4),]
Aemp5 <-Aemp_data[which(Bemp_data$income==5),]
Bemp1 <-Bemp_data[which(Bemp_data$income==1),]
Bemp2 <-Bemp_data[which(Bemp_data$income==2),]
Bemp3 <-Bemp_data[which(Bemp_data$income==3),]
Bemp4 <-Bemp_data[which(Bemp_data$income==4),]
Bemp5 <-Bemp_data[which(Bemp_data$income==5),]

#Employment rider comfort
wilcox.test(Aemp1$comfort_ride,Aemp2$comfort_ride)
wilcox.test(Aemp1$comfort_ride,Aemp3$comfort_ride)
wilcox.test(Aemp1$comfort_ride,Aemp4$comfort_ride)
wilcox.test(Aemp1$comfort_ride,Aemp4$comfort_ride)
wilcox.test(Aemp1$comfort_ride,Aemp5$comfort_ride)
wilcox.test(Aemp2$comfort_ride,Aemp3$comfort_ride)
wilcox.test(Aemp2$comfort_ride,Aemp4$comfort_ride)
wilcox.test(Aemp2$comfort_ride,Aemp5$comfort_ride)
wilcox.test(Aemp3$comfort_ride,Aemp4$comfort_ride)
wilcox.test(Aemp3$comfort_ride,Aemp5$comfort_ride)
wilcox.test(Aemp4$comfort_ride,Aemp5$comfort_ride)

#Employment drive comfort
wilcox.test(Aemp1$comfort_drive,Aemp2$comfort_drive)
wilcox.test(Aemp1$comfort_drive,Aemp3$comfort_drive)
wilcox.test(Aemp1$comfort_drive,Aemp4$comfort_drive)
wilcox.test(Aemp1$comfort_drive,Aemp4$comfort_drive)
wilcox.test(Aemp1$comfort_drive,Aemp5$comfort_drive)
wilcox.test(Aemp2$comfort_drive,Aemp3$comfort_drive)
wilcox.test(Aemp2$comfort_drive,Aemp4$comfort_drive)
wilcox.test(Aemp2$comfort_drive,Aemp5$comfort_drive)
wilcox.test(Aemp3$comfort_drive,Aemp4$comfort_drive)
wilcox.test(Aemp3$comfort_drive,Aemp5$comfort_drive)
wilcox.test(Aemp4$comfort_drive,Aemp5$comfort_drive)

#Employment ped comfort
wilcox.test(Aemp1$comfort_ped,Aemp2$comfort_ped)
wilcox.test(Aemp1$comfort_ped,Aemp3$comfort_ped)
wilcox.test(Aemp1$comfort_ped,Aemp4$comfort_ped)
wilcox.test(Aemp1$comfort_ped,Aemp4$comfort_ped)
wilcox.test(Aemp1$comfort_ped,Aemp5$comfort_ped)
wilcox.test(Aemp2$comfort_ped,Aemp3$comfort_ped)
wilcox.test(Aemp2$comfort_ped,Aemp4$comfort_ped)
wilcox.test(Aemp2$comfort_ped,Aemp5$comfort_ped)
wilcox.test(Aemp3$comfort_ped,Aemp4$comfort_ped)
wilcox.test(Aemp3$comfort_ped,Aemp5$comfort_ped)
wilcox.test(Aemp4$comfort_ped,Aemp5$comfort_ped)

#Before and after comfort
#1Student
wilcox.test(Aemp1$comfort_ride,Bemp1$comfort_ride)
wilcox.test(Aemp1$comfort_drive,Bemp1$comfort_drive)
wilcox.test(Aemp1$comfort_ped,Bemp1$comfort_ped)
describe(Aemp1$comfort_ride)
describe(Aemp1$comfort_drive)
describe(Aemp1$comfort_ped)
describe(Bemp1$comfort_ride)
describe(Bemp1$comfort_drive)
describe(Bemp1$comfort_ped)

#2Unemployed
wilcox.test(Aemp2$comfort_ride,Bemp2$comfort_ride)
wilcox.test(Aemp2$comfort_drive,Bemp2$comfort_drive)
wilcox.test(Aemp2$comfort_ped,Bemp2$comfort_ped)
describe(Aemp2$comfort_ride)
describe(Aemp2$comfort_drive)
describe(Aemp2$comfort_ped)
describe(Bemp2$comfort_ride)
describe(Bemp2$comfort_drive)
describe(Bemp2$comfort_ped)

#3Part-Time
wilcox.test(Aemp3$comfort_ride,Bemp3$comfort_ride)
wilcox.test(Aemp3$comfort_drive,Bemp3$comfort_drive)
wilcox.test(Aemp3$comfort_ped,Bemp3$comfort_ped)
describe(Aemp3$comfort_ride)
describe(Aemp3$comfort_drive)
describe(Aemp3$comfort_ped)
describe(Bemp3$comfort_ride)
describe(Bemp3$comfort_drive)
describe(Bemp3$comfort_ped)

#4Full-Time
wilcox.test(Aemp4$comfort_ride,Bemp4$comfort_ride)
wilcox.test(Aemp4$comfort_drive,Bemp4$comfort_drive)
wilcox.test(Aemp4$comfort_ped,Bemp4$comfort_ped)
describe(Aemp4$comfort_ride)
describe(Aemp4$comfort_drive)
describe(Aemp4$comfort_ped)
describe(Bemp4$comfort_ride)
describe(Bemp4$comfort_drive)
describe(Bemp4$comfort_ped)

#5Retired
wilcox.test(Aemp5$comfort_ride,Bemp5$comfort_ride)
wilcox.test(Aemp5$comfort_drive,Bemp5$comfort_drive)
wilcox.test(Aemp5$comfort_ped,Bemp5$comfort_ped)
describe(Aemp5$comfort_ride)
describe(Aemp5$comfort_drive)
describe(Aemp5$comfort_ped)
describe(Bemp5$comfort_ride)
describe(Bemp5$comfort_drive)
describe(Bemp5$comfort_ped)

#after employment intention
wilcox.test(Aemp1$intent,Aemp2$intent)
wilcox.test(Aemp1$intent,Aemp3$intent)
wilcox.test(Aemp1$intent,Aemp4$intent)
wilcox.test(Aemp1$intent,Aemp5$intent)
wilcox.test(Aemp2$intent,Aemp3$intent)
wilcox.test(Aemp2$intent,Aemp4$intent)
wilcox.test(Aemp2$intent,Aemp5$intent)
wilcox.test(Aemp3$intent,Aemp4$intent)
wilcox.test(Aemp3$intent,Aemp5$intent)
wilcox.test(Aemp4$intent,Aemp5$intent)

#before employment intention
wilcox.test(Bemp1$intent,Bemp2$intent)
wilcox.test(Bemp1$intent,Bemp3$intent)
wilcox.test(Bemp1$intent,Bemp4$intent)
wilcox.test(Bemp1$intent,Bemp5$intent)
wilcox.test(Bemp2$intent,Bemp3$intent)
wilcox.test(Bemp2$intent,Bemp4$intent)
wilcox.test(Bemp2$intent,Bemp5$intent)
wilcox.test(Bemp3$intent,Bemp4$intent)
wilcox.test(Bemp3$intent,Bemp5$intent)
wilcox.test(Bemp4$intent,Bemp5$intent)


#after and before intention
wilcox.test(Aemp1$intent,Bemp1$intent)
wilcox.test(Aemp2$intent,Bemp2$intent)
wilcox.test(Aemp3$intent,Bemp3$intent)
wilcox.test(Aemp4$intent,Bemp4$intent)
wilcox.test(Aemp5$intent,Bemp5$intent)
describe(Aemp1$intent)
describe(Aemp2$intent)
describe(Aemp3$intent)
describe(Aemp4$intent)
describe(Aemp5$intent)
describe(Bemp1$intent)
describe(Bemp2$intent)
describe(Bemp3$intent)
describe(Bemp4$intent)
describe(Bemp5$intent)

attitude  =~ comfort_ride+comfort_ride2
PU =~ smooth_ride+reliable_ride
intention   =~ intent_ride
PEofU =~ easy_ride
trust =~ control_ride+anxious_ride+cyber_ride
tech =~ apps+uber
# regressions
intention ~ attitude+PU
attitude ~ PU+trust
PU~PEofU

beforerider_data = beforedata
beforedriver_data = beforedata
beforeped_data = beforedata
afterrider_data <-  afterdata[which(afterdata$freq_ride>1),]
afterdriver_data <-  afterdata[which(afterdata$freq_drive>1),]
afterped_data <-  afterdata[which(afterdata$freq_ped>1),]

beforerider_data <- beforerider_data%>% drop_na(comfort_ride,comfort_ride2,smooth_ride,
                                                reliable_ride,crashvehicle_ride,intent_ride, easy_ride,
                                                control_ride,anxious_ride,cyber_ride)
beforedriver_data <- beforedriver_data%>% drop_na(comfort_drive,comfort_drive2,slow_drive,acc_drive,
                                                  comfort_change_drive, intent_ride,signal_drive,
                                                  adjacent_drive,follow_drive)

beforeped_data <- beforeped_data%>% drop_na(comfort_ped,comfort_ped2,crash_ped,cross_ped,
                                            signal_ped,adjacent_ped,intent_ride,easy_ped)

afterrider_data <- afterrider_data%>% drop_na(c(comfort_ride,relax_ride,satisfy_ride,
                                                smooth_ride, reliable_ride,crashped_ride,
                                                easy_ride,control_ride,signal_ride,
                                                anxious_ride,intent,recommend))

afterdriver_data <- afterdriver_data%>% drop_na(c(comfort_drive,relax_drive,satisfy_drive,
                                                  slow_drive,acc_drive,comfort_change_drive,
                                                  easy_drive,signal_drive,adjacent_drive,follow_drive,
                                                  intent,recommend))

afterped_data <- afterped_data%>% drop_na(c(comfort_ped,relax_ped,satisfy_ped,
                                            crash_ped,cross_ped,easy_ped,signal_ped,
                                            intent,recommend,attent_ped,adjacent_ped))

afterrider_data['attent_ride']= 8-afterrider_data['attent_ride']
afterrider_data['anxious_ride']= 8-afterrider_data['anxious_ride']
afterdriver_data['signal_drive']= 8-afterdriver_data['signal_drive']
afterdriver_data['acc_drive']= 8-afterdriver_data['acc_drive']
afterdriver_data['follow_drive']= 8-afterdriver_data['follow_drive']
afterdriver_data['adjacent_drive']= 8-afterdriver_data['adjacent_drive']
afterdriver_data['comfort_change_drive']= 8-afterdriver_data['comfort_change_drive']
afterdriver_data['slow_drive']= 8-afterdriver_data['slow_drive']
afterdriver_data['attent_drive']= 8-afterdriver_data['attent_drive']
afterped_data['cross_ped']= 8-afterped_data['cross_ped']
afterped_data['attent_ped']= 8-afterped_data['attent_ped']
afterped_data['signal_ped']= 8-afterped_data['signal_ped']
afterped_data['adjacent_ped']= 8-afterped_data['adjacent_ped']


beforerider_data['anxious_ride'] = 8-beforerider_data['anxious_ride']
beforerider_data['cyber_ride'] = 8-beforerider_data['cyber_ride']
beforedriver_data['signal_drive']= 8-beforedriver_data['signal_drive']
beforedriver_data['acc_drive']= 8-beforedriver_data['acc_drive']
beforedriver_data['adjacent_drive']= 8-beforedriver_data['adjacent_drive']
beforedriver_data['comfort_change_drive']= 8-beforedriver_data['comfort_change_drive']
beforedriver_data['slow_drive']= 8-beforedriver_data['slow_drive']
beforedriver_data['follow_drive']= 8-beforedriver_data['follow_drive']
beforeped_data['cross_ped']= 8-beforeped_data['cross_ped']
beforeped_data['adjacent_ped']= 8-beforeped_data['adjacent_ped']
beforeped_data['signal_ped']= 8-beforeped_data['signal_ped']


write.csv(beforerider_data,file="Data/beforerider.csv",quote=F,row.names = F)
write.csv(beforedriver_data,file="Data/beforedriver.csv",quote=F,row.names = F)
write.csv(beforeped_data,file="Data/beforeped.csv",quote=F,row.names = F)
write.csv(afterrider_data,file="Data/afterrider.csv",quote=F,row.names = F)
write.csv(afterdriver_data,file="Data/afterdriver.csv",quote=F,row.names = F)
write.csv(afterped_data,file="Data/afterped.csv",quote=F,row.names = F)

describe(afterdriver_data['intent'])
describe(beforedriver_data['intent_ride'])
describe(afterrider_data['intent'])
describe(afterped_data['intent'])

describe(afterdriver_data['comfort_drive'])
describe(afterrider_data['comfort_ride'])
describe(afterped_data['comfort_ped'])


describe(afterdriver_data['follow_drive'])
describe(beforedriver_data['follow_drive'])

t.test(afterdriver_data['easy_drive'],beforedriver_data['easy_drive'])

describe(afterdriver_data['acc_drive'])
describe(beforedriver_data['acc_drive'])


describe(beforedata['age'])
describe(afterdata['age'])

describe(beforedata['gender'])
describe(afterdata['gender'])

describe(beforedata['income'])
describe(afterdata['income'])

describe(beforedata['comfort_ride'])
describe(afterdata['comfort_ride'])

describe(beforedata['crashvehicle_ride'])
describe(afterdata['crashvehicle_ride'])

describe(beforedata['smooth_ride'])
describe(afterdata['smooth_ride'])

describe(beforedata['easy_ride'])
describe(afterdata['easy_ride'])

describe(beforedata['anxious_ride'])
describe(afterdata['anxious_ride'])
describe(afterrider_data['anxious_ride'])

describe(beforedata['intent_ride'])
describe(afterdata['intent'])

describe(beforedata['slow_drive'])
describe(afterdata['slow_drive'])

describe(beforedata['comfort_drive'])
describe(afterdata['comfort_drive'])

describe(beforedata['signal_drive'])
describe(afterdata['signal_drive'])

describe(beforedata['adjacent_drive'])
describe(afterdata['adjacent_drive'])

describe(beforedata['comfort_ped'])
describe(afterdata['comfort_ped'])

describe(beforedata['easy_ped'])
describe(afterdata['easy_ped'])

describe(beforedata['adjacent_ped'])
describe(afterdata['adjacent_ped'])

describe(beforedata['crash_ped'])
describe(afterdata['crash_ped'])

describe(beforedata['cross_ped'])
describe(afterdata['cross_ped'])
