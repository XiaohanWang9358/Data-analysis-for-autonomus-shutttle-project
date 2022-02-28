library(tidyverse)
library(psych)
library(rela)
library(MASS)
library("lavaan")
library(GPArotation)
library(polycor)
#Run Data analysis.R first



# Separate brick by riders, drivers, peds ----
afterrider_data <-  afterdata[which(afterdata$freq_ride>1),]
afterdriver_data <-  afterdata[which(afterdata$freq_drive>1),]
afterped_data <-  afterdata[which(afterdata$freq_ped>1),]

beforerider_data <-  beforedata
beforedriver_data <-  beforedata
beforeped_data <-  beforedata
# TAM model ----
#reliability(cov(afterrider_data))

#take the mean of each factor
Atam_rider= as.data.frame(matrix(nrow=nrow(afterrider_data),ncol=0))
Atam_driver= as.data.frame(matrix(nrow=nrow(afterdriver_data),ncol=0))
Atam_ped= as.data.frame(matrix(nrow=nrow(afterped_data),ncol=0))

#Attitudes of Riders
Atam_rider$attitude=apply(afterrider_data[,c("comfort_ride","relax_ride","satisfy_ride")],1,mean,na.rm=T)

Atam_driver$attitude=apply(afterdriver_data[,c("comfort_drive","relax_drive","satisfy_drive")],1,mean,na.rm=T)
Atam_ped$attitude=apply(afterped_data[,c("comfort_ped","relax_ped","satisfy_ped")],1,mean,na.rm=T)

#Perceived Usefulness (PU) of Riders 
Atam_rider$PU=apply(afterrider_data[,c('smooth_ride',"reliable_ride","crashvehicle_ride","crashped_ride")],1,mean,na.rm=T)
Atam_driver$PU=apply(afterdriver_data[,c("crash_drive",'comfort_change_drive',"smooth_drive","slow_drive")],1,mean,na.rm=T)
Atam_ped$PU=apply(afterped_data[,c("crash_ped","cross_ped")],1,mean,na.rm=T)

#Perceived Ease of Use (PEofU) of Riders
Atam_rider$PEofU=apply(afterrider_data[,c("easy_ride","signal_ride")],1,mean,na.rm=T)
Atam_driver$PEofU=apply(afterdriver_data[,c("easy_drive","signal_drive")],1,mean,na.rm=T)
Atam_ped$PEofU=apply(afterped_data[,c("easy_ped","signal_ped")],1,mean,na.rm=T)

#Trust/Confidence of Riders
Atam_rider$trust=apply(afterrider_data[,c("control_ride","attent_ride")],1,mean,na.rm=T)
#Atam_driver$trust=apply(afterdriver_data[,c(16,19,20)],1,mean,na.rm=T)
#Atam_ped$trust=apply(afterped_data[,c("attent_ride")],1,mean,na.rm=T)
Atam_driver$intention=afterdriver_data[,"attent_drive"]
Atam_ped$intention=afterped_data[,"attent_ped"]

#Intention to use of Riders
Atam_rider$intention=apply(afterrider_data[,c("recommend","intent")],1,mean,na.rm=T)
Atam_driver$intention=apply(afterdriver_data[,c("recommend","intent")],1,mean,na.rm=T)
Atam_ped$intention=apply(afterped_data[,c("recommend","intent")],1,mean,na.rm=T)
#Atam_rider$intention=afterrider_data[,54]
#Atam_driver$intention=afterdriver_data[,48]
#Atam_ped$intention=afterped_data[,c(48)]

#Demographic of Riders
Atam_rider$age=afterrider_data$age
Atam_rider$income=afterrider_data$income
Atam_rider$race=afterrider_data$race
Atam_rider$edcation=afterrider_data$education
Atam_rider$employment=afterrider_data$employment

Atam_driver$age=afterdriver_data$age
Atam_driver$income=afterdriver_data$income
Atam_driver$race=afterdriver_data$race
Atam_driver$edcation=afterdriver_data$education
Atam_driver$employment=afterdriver_data$employment

Atam_ped$age=afterped_data$age
Atam_ped$income=afterped_data$income
Atam_ped$race=afterped_data$race
Atam_ped$edcation=afterped_data$education
Atam_ped$employment=afterped_data$employment

#Technical Comfort
Atam_rider$tech=apply(afterrider_data[,c("phone","apps","uber","AV_terms")],1,mean,na.rm=T)
Atam_rider$tech=apply(afterrider_data[,c("phone","apps","uber","AV_terms")],1,mean,na.rm=T)
Atam_rider$tech=apply(afterrider_data[,c("phone","apps","uber","AV_terms")],1,mean,na.rm=T)

#Travel Behavior
#Atam_rider$vehicle=afterrider_data$vehicle
#Atam_rider$RTS=afterrider_data$RTS
#Atam_rider$walk_bike=afterrider_data$walk_bike

#use Atam_rider to build TAM model for rider
library(tidyr) #QUERY: This is included in tidyverse
#drop rows if intention, or PU or PEofU or attitude is NAN
Atam_rider <- Atam_rider%>% drop_na(intention)
Atam_rider <- Atam_rider%>% drop_na(PU)
Atam_rider <- Atam_rider%>% drop_na(PEofU)
Atam_rider <- Atam_rider%>% drop_na(attitude)

Atam_driver <- Atam_driver%>% drop_na(intention)
Atam_driver <- Atam_driver%>% drop_na(PU)
Atam_driver <- Atam_driver%>% drop_na(PEofU)
Atam_driver <- Atam_driver%>% drop_na(attitude)

Atam_ped <- Atam_ped%>% drop_na(intention)
Atam_ped <- Atam_ped%>% drop_na(PU)
Atam_ped <- Atam_ped%>% drop_na(PEofU)
Atam_ped <- Atam_ped%>% drop_na(attitude)

# Assumptions -----

## Scatterplot to check linearity
pairs(Atam_rider[1:5]) #select different columns if you need more dvs/ivs

## Correlation plot
Atam_rider.cor = cor(Atam_rider[1:5]) #Change column numbers if you want to see other variables
corrplot::corrplot(Atam_rider.cor, 'number')

## Scatterplot to check linearity
pairs(Atam_driver[1:5]) #select different columns if you need more dvs/ivs

## Correlation plot
Atam_driver.cor = cor(Atam_driver[1:5]) #Change column numbers if you want to see other variables
corrplot::corrplot(Atam_driver.cor, 'number')

## Scatterplot to check linearity
pairs(Atam_ped[1:5]) #select different columns if you need more dvs/ivs

## Correlation plot
Atam_ped.cor = cor(Atam_ped[1:5]) #Change column numbers if you want to see other variables
corrplot::corrplot(Atam_ped.cor, 'number')

#################TAM model#################
library(mediation)

#rider_data<-rider_data[,c("Q1","Q2","Q3_1","Q3_2","Q3_3","Q3_4","Q8","Q18_1","Q18_2", "Q18_3","Q23_1", "Q23_2", "Q23_3", "Q23_4", "Q23_5", "Q23_6", "Q23_7", "Q23_8", "Q23_9","Q37", "Q39", "Q40", "Q41", "Q42", "Q43","Q33", "Q38", "Q45")]
#ped_data<-ped_data[,c("Q1","Q2","Q3_1","Q3_2","Q3_3","Q3_4","Q8","Q29_1",	"Q29_2","Q29_3", "Q30_1","Q30_2","Q30_3","Q30_4","Q30_5","Q30_6" ,"Q37", "Q39", "Q40", "Q41", "Q42", "Q43","Q33", "Q38", "Q45")]
#driver_data<-driver_data[,c("Q1","Q2","Q3_1","Q3_2","Q3_3","Q3_4","Q8","Q27_1","Q27_2","Q27_3","Q28_1","Q28_2","Q28_3","Q28_4",	"Q28_5","Q28_6","Q28_7","Q28_8","Q28_9","Q28_10","Q37", "Q39", "Q40", "Q41", "Q42", "Q43","Q33", "Q38", "Q45")]

#rider_data<- rider_data[-which(is.na(rider_data$start_)), ]

library(umx)

# Model1: model with mediation effect without covariates ----
#mediator is attitude
#after rider
lm1 <- lm(attitude ~ PU, data=Atam_rider)
lm2 <- lm(intention~ PU+PEofU+attitude, data=Atam_rider)
contcont <- mediation::mediate(lm1, lm2, sims=100, treat="PU",mediator="attitude") #psych and mediation conflict, need to add "mediation::
summary(contcont)
plot(contcont)
summary(lm1)
summary(lm2)

#after driver
lm1 <- lm(attitude ~ PU, data=Atam_driver)
lm2 <- lm(intention~ PU+PEofU+attitude, data=Atam_driver)
contcont <- mediation::mediate(lm1, lm2, sims=100, treat="PU",mediator="attitude") #psych and mediation conflict, need to add "mediation::
summary(contcont)
plot(contcont)
summary(lm1)

#after ped
lm1 <- lm(attitude ~ PU, data=Atam_ped)
lm2 <- lm(intention~ PU+PEofU+attitude, data=Atam_ped)
contcont <- mediation::mediate(lm1,lm2, sims=100, treat="PU",mediator="attitude") #psych and mediation conflict, need to add "mediation::
summary(contcont)
plot(contcont)
summary(lm1)


##Nifty package to check assumptions
summary(gvlma::gvlma(lm1))
plot(gvlma::gvlma(lm1))

## Durbin Watson (autocorrelations)
car::durbinWatsonTest(lm1) #Okay

# Extract R2 by variable
rsq::rsq.partial(lm1, adj = T)
rsq::rsq.partial(lm1, adj = F)
## Extract VIF
car::vif(lm2)
#Extract beta coefficients: #https://www.dataanalytics.org.uk/beta-coefficients-from-linear-models/
lm1.12 = lm.beta::lm.beta(lm1)
print(lm1.12, digits = 3)


# Model2: model with mediation effect with Moderator ----
#attitude is the mediator and age the the Moderator
#QUERY: Another option for creating interaction variables (https://recipes.tidymodels.org/reference/step_interact.html)
# Center variables -----
Atam_rider$PU.c <- scale(Atam_rider$PU, center = TRUE, scale = FALSE)[,] #QUERY - simpler method to center: 
#Atam_rider$PU.c = PU - mean(PU)
Atam_rider$age.c<- scale(Atam_rider$age, center = TRUE, scale = FALSE)[,]
Atam_rider$race.c<- scale(Atam_rider$race, center = TRUE, scale = FALSE)[,]

# Create interaction variables ----
Atam_rider$PU.age.i = Atam_rider$PU.c*Atam_rider$age.c
Atam_rider$PU.race.i = Atam_rider$PU.c*Atam_rider$race.c

lm1<-lm(attitude ~ PU.c*age.c, data = Atam_rider) 
lm2<-lm(intention ~ PU.c*age.c+PEofU+attitude, data = Atam_rider) 
low.w <- mean(Atam_rider$age.c) - sd(Atam_rider$age.c)

Mod.Med.Low.w <- mediation::mediate(lm1, lm2,    
                                    covariates = list(age.c = low.w), 
                                    boot = TRUE, boot.ci.type = "bca", sims = 10, 
                                    treat = c("PU.c","PEofU"), mediator = "attitude")
summary(Mod.Med.Low.w)
summary(lm1)

library(lavaan)

sem_model = '
  attitude ~ PU+PEofU
  intention ~ PU+PEofU+attitude
 
  # direct effect
  direct := c
 
  # indirect effect
  indirect := a*b
 
  # total effect
  total := c + (a*b)
'

model_sem = sem(sem_model, data=jobs, se='boot', bootstrap=500)
summary(model_sem, rsq=T)  # compare with ACME in mediation

library(psych)
library(corrplot)
library("psych")
library(car)

#After rider
#CFA
afterrider_data <-  afterdata[which(afterdata$freq_ride>1),]
summary(afterrider_data)
afterrider_data <- afterrider_data%>% drop_na(comfort_ride,relax_ride,satisfy_ride,AV_terms)
cfa.model <- '     attitude  =~ comfort_ride+relax_ride+satisfy_ride
    PU =~ smooth_ride+reliable_ride+crashped_ride
    intention   =~ intent+recommend
    PEofU =~ signal_ride+easy_ride
    trust =~ anxious_ride
            '
fit <- cfa(cfa.model, data=afterrider_data,std.lv=T)
summary(fit,fit.measures=TRUE)
lavInspect(fit, "cov.lv")

model.equal <- '
  # measurement model
    attitude  =~ comfort_ride+relax_ride+satisfy_ride
    PU =~ smooth_ride+reliable_ride+crash_ped_ride
    intention   =~ intent+recommend
    PEofU =~ signal_ride+eay_ride
    trust =~ anxious_ride
  # regressions
    intention ~ attitude+PU+trust
    attitude ~ PU+PEofU+trust
    PU~PEofU
    
'
fit.equal <- sem(model.equal, data=afterrider_data,std.lv=T)
summary(fit.equal)


#After driver

summary(afterdriver_data)
afterdriver_data <- afterdriver_data%>% drop_na(comfort_drive,relax_drive,satisfy_drive,intent)
cfa.model <- '    attitude  =~comfort_drive+relax_drive+satisfy_drive
    PU =~ slow_drive+smooth_drive
    intention   =~ recommend+intent
    PEofU =~ easy_drive+signal_drive
    trust =~ adjacent_drive+follow_drive+slow_drive
    tech =~apps+phone
            '
fit <- cfa(cfa.model, data=afterdriver_data,std.lv=T)
summary(fit,fit.measures=TRUE)

model.equal <- '
    attitude  =~comfort_drive+relax_drive+satisfy_drive
    PU =~ crash_drive+smooth_drive
    intention   =~ recommend+intent
    PEofU =~ easy_drive+signal_drive
    trust =~ adjacent_drive+follow_drive+slow_drive
    tech =~apps+phone
  # regressions
    intention ~ attitude
    attitude ~ PU+PEofU+tech
    PU ~PEofU
    
'
fit.equal <- sem(model.equal, data=afterdriver_data,std.lv=T)
summary(fit.equal)

#After ped
#CFA
summary(afterped_data)
afterped_data <- afterped_data%>% drop_na(comfort_ped,relax_ped,satisfy_ped,intent)
cfa.model <- '       # measurement model
    attitude  =~ comfort_ped+relax_ped+satisfy_ped
    PU =~ crash_ped+cross_ped
    intention   =~ recommend+intent
    PEofU =~ easy_ped
    trust =~ attent_ped
            '
fit <- cfa(cfa.model, data=afterped_data,std.lv=T)
summary(fit,fit.measures=TRUE)

model.equal <- '
  # measurement model
    attitude  =~ comfort_ped+relax_ped+satisfy_ped
    PU =~ crash_ped+cross_ped
    intention   =~ recommend+intent
    PEofU =~ easy_ped
    trust =~ attent_ped
  # regressions
    intention ~ attitude+trust+PU
    attitude ~ PU+PEofU+trust
'
fit.equal <- sem(model.equal, data=afterped_data,std.lv=T)
summary(fit.equal)

#Before Rider
summary(beforerider_data)
efa.beforerider = beforerider_data[c('comfort_ride','comfort_ride2','smooth_ride','reliable_ride','intent_ride',
                                   'easy_ride','control_ride','anxious_ride','cyber_ride','apps','uber')]

efa.beforerider%>% drop_na()
cor.data <- cor(efa.beforerider)
corrplot(cor.data, method="number")

KMO(r=cor(efa.beforerider))
cortest.bartlett(efa.beforerider)

beforerider_data <- beforerider_data%>% drop_na(age,education,race,gender,employment,income,apps,uber,control_ride,anxious_ride,
                                                 crashped_ride,comfort_ride2,cyber_ride,smooth_ride,reliable_ride,comfort_ride,intent_ride,easy_ride,education)

cfa.model <- '         attitude  =~ comfort_ride+comfort_ride2
    PU =~ smooth_ride+reliable_ride
    intention   =~ intent_ride
    PEofU =~ easy_ride
    trust =~ control_ride+anxious_ride+cyber_ride
            '
fit <- cfa(cfa.model, data=beforerider_data,std.lv=T)
summary(fit,fit.measures=TRUE)

model <- '
  # measurement model
    attitude  =~ comfort_ride+comfort_ride2
    PU =~ smooth_ride+reliable_ride+crashvehicle_ride
    intention   =~ intent_ride
    PEofU =~ easy_ride
    trust =~ anxious_ride+cyber_ride
  # regressions
    intention ~ attitude+PU+trust
    attitude ~ PU+trust+PEofU
    PU~PEofU
'
fit <- sem(model, data=beforerider_data,std.lv=T)
summary(fit,fit.measures = TRUE)
inspect(fit, 'r2')

#Before driver
efa.beforedrive = beforedriver_data[c('comfort_drive','comfort_drive2','slow_drive','acc_drive','comfort_change_drive',
                                     'intent_ride','signal_drive','adjacent_drive','follow_drive','apps','uber','AV_terms')]

efa.beforedrive%>% drop_na()
cor.data <- cor(efa.beforedrive)
corrplot(cor.data, method="number")

KMO(r=cor(efa.beforedrive))
cortest.bartlett(efa.beforedrive)

summary(beforedriver_data)
beforedriver_data <- beforedriver_data%>% drop_na(comfort_drive,comfort_drive2,slow_drive,crash_drive,
                                                  acc_drive,AV_terms,apps,uber,follow_drive,intent_ride)

cfa.model <- '     attitude  =~ comfort_drive+comfort_drive2
    PU =~ slow_drive+acc_drive+comfort_change_drive
    intention   =~ intent_ride
    PEofU =~ signal_drive
    trust =~adjacent_drive+follow_drive
    tech =~ apps+uber+AV_terms
            '
fit <- cfa(cfa.model, data=beforedriver_data,std.lv=T)
summary(fit,fit.measures=TRUE)

model <- '
  # measurement model
    attitude  =~ comfort_drive+comfort_drive2
    PU =~ slow_drive+acc_drive+comfort_change_drive
    intention   =~ intent_ride
    PEofU =~ signal_drive
    trust =~adjacent_drive+follow_drive
    tech =~ apps+uber+AV_terms
  # regressions
    intention ~ attitude+trust
    attitude ~ PU+PEofU
    PU~PEofU
'
fit <- sem(model, data=beforedriver_data,std.lv=T)
summary(fit,fit.measures = TRUE)

#Before ped
summary(beforeped_data)
efa.beforeped = beforeped_data[c('comfort_ped','comfort_ped2','crash_ped','cross_ped',
                                     'intent_ride','easy_ped','adjacent_ped','apps','uber','AV_terms')]

efa.beforeped%>% drop_na()
cor.data <- cor(efa.beforeped)
corrplot(cor.data, method="number")

KMO(r=cor(efa.beforeped))
cortest.bartlett(efa.beforeped)

beforeped_data <- beforeped_data%>% drop_na(education,comfort_ped,comfort_ped2,crash_ped,cross_ped,
                                            easy_ped,signal_ped,apps,uber,intent_ride)
cfa.model <- '     attitude  =~ comfort_ped+comfort_ped2
    PU =~crash_ped+cross_ped
    intention   =~ intent_ride
    PEofU =~ easy_ped
    #trust =~ adjacent_ped
    tech =~ apps+uber+AV_terms
            '
fit <- cfa(cfa.model, data=beforeped_data,std.lv=T)
summary(fit,fit.measures=TRUE)

model.equal <- '
  # measurement model
    attitude  =~ comfort_ped+comfort_ped2
    PU =~crash_ped+cross_ped
    intention   =~ intent_ride
    PEofU =~ easy_ped
    #trust =~ adjacent_ped
    tech =~ apps+uber+AV_terms
  # regressions
    intention ~ attitude+tech
    attitude ~ PU
    PU~PEofU
'
fit.equal <- sem(model.equal, data=beforeped_data,std.lv=T)
summary(fit.equal)


model.equal <- '
  # measurement model
    attitude  =~comfort_drive+relax_drive+satisfy_drive
    PU =~ crash_drive+comfort_change_drive+smooth_drive+slow_drive
    intention   =~ recommend+intent
    PEofU =~ easy_drive+signal_drive
    trust =~ attent_drive
    tech =~ AV_terms

  # regressions
    intention ~ attitude+PU+PEofU
    attitude ~ PU+PEofU

'
fit.equal <- sem(model.equal, data=afterdriver_data)
summary(fit.equal)

model.equal <- '
  # measurement model
    attitude  =~ comfort_ped+relax_ped+satisfy_ped
    PU =~ crash_ped+cross_ped
    intention   =~ recommend+intent
    PEofU =~ easy_ped+signal_ped
    trust =~ attent_ped

  # regressions
    intention ~ attitude+PU+PEofU
    attitude ~ PU

'
fit.equal <- sem(model.equal, data=afterped_data,std.lv=T)
summary(fit.equal)


#EFA analysis
#before driver EFA
Bdriver_efa = beforedriver_data[,c('comfort_drive','comfort_drive2','slow_drive',
                                   'acc_drive','comfort_change_drive','signal_drive','adjacent_drive','follow_drive')]
Bdriver_efa <- Bdriver_efa%>% drop_na()
EFA_model <- fa(Bdriver_efa, nfactors = 4)
print(EFA_model)
EFA_model$loadings
KMO(r=cor(Bdriver_efa))
cortest.bartlett(Bdriver_efa)

sdt_sub_correl <- hetcor(Bdriver_efa)
str(sdt_sub_correl)
sdt_polychoric <- sdt_sub_correl$correlations
cortest.bartlett(sdt_polychoric)
KMO(sdt_polychoric)
scree(sdt_polychoric)
fa.parallel(sdt_polychoric, n.obs = 100, fa = "fa")
EFA_model = fa(sdt_polychoric, fm = "ml", nfactors = 4)
EFA_model$loadings

#After driver EFA
#Adriver_efa = afterdriver_data[,c('comfort_drive','satisfy_drive','relax_drive','slow_drive','smooth_drive',
#                                   'acc_drive','comfort_change_drive','easy_drive','signal_drive','adjacent_drive','follow_drive')]
Adriver_efa = afterdriver_data[,c(
                                  'acc_drive','comfort_change_drive','easy_drive','signal_drive')]
Adriver_efa <- Adriver_efa%>% drop_na()
EFA_model <- fa(Adriver_efa, nfactors = 2)
print(EFA_model)
EFA_model$loadings

sdt_sub_correl <- hetcor(Adriver_efa)
str(sdt_sub_correl)
sdt_polychoric <- sdt_sub_correl$correlations
cortest.bartlett(sdt_polychoric)
KMO(sdt_polychoric)
EFA_model = fa(sdt_polychoric, fm = "ml", nfactors = 2)
EFA_model$loadings

#Before ped EFA
Bped_efa = beforeped_data[,c('comfort_ped','comfort_ped2','crash_ped',
                                   'cross_ped','easy_ped','signal_ped','adjacent_ped')]
Bped_efa <- Bped_efa%>% drop_na()
EFA_model <- fa(Bped_efa, nfactors = 4)
print(EFA_model)
EFA_model$loadings

sdt_sub_correl <- hetcor(Bped_efa)
str(sdt_sub_correl)
sdt_polychoric <- sdt_sub_correl$correlations
cortest.bartlett(sdt_polychoric)
KMO(sdt_polychoric)
EFA_model = fa(sdt_polychoric, fm = "ml", nfactors = 4)
EFA_model$loadings

#After ped EFA
Aped_efa = afterped_data[,c('comfort_ped','relax_ped','satisfy_ped','crash_ped',
                            'cross_ped','easy_ped','attent_ped')]
Aped_efa <- Aped_efa%>% drop_na()
EFA_model <- fa(Aped_efa, nfactors = 4)
print(EFA_model)
EFA_model$loadings

sdt_sub_correl <- hetcor(Aped_efa)
str(sdt_sub_correl)
sdt_polychoric <- sdt_sub_correl$correlations
cortest.bartlett(sdt_polychoric)
KMO(sdt_polychoric)
EFA_model = fa(sdt_polychoric, fm = "ml", nfactors = 4)
EFA_model$loadings
