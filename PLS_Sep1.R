#install.packages("seminr")
#install.packages('semPlot')
library(seminr)

plsbefore_rider= read.csv("Data/beforerider.csv",header=TRUE,na.strings = "") 
plsbefore_drive= read.csv("Data/beforedriver.csv",header=TRUE,na.strings = "") 
plsbefore_ped = read.csv("Data/beforeped.csv",header=TRUE,na.strings = "") 
plsafter_rider= read.csv("Data/afterrider.csv",header=TRUE,na.strings = "") 
plsafter_drive= read.csv("Data/afterdriver.csv",header=TRUE,na.strings = "") 
plsafter_ped = read.csv("Data/afterped.csv",header=TRUE,na.strings = "") 

#beforerider ----
names(plsbefore_rider)[names(plsbefore_rider)=='comfort_ride']<-"ATTITUDE1"
names(plsbefore_rider)[names(plsbefore_rider)=='comfort_ride2']<-"ATTITUDE2"
names(plsbefore_rider)[names(plsbefore_rider)=='smooth_ride']<-"PU1"
names(plsbefore_rider)[names(plsbefore_rider)=='reliable_ride']<-"PU2"
names(plsbefore_rider)[names(plsbefore_rider)=='crashvehicle_ride']<-"PU3"
names(plsbefore_rider)[names(plsbefore_rider)=='easy_ride']<-"PEofU1"
names(plsbefore_rider)[names(plsbefore_rider)=='control_ride']<-"PEofU2"
names(plsbefore_rider)[names(plsbefore_rider)=='anxious_ride']<-"TRUST1"
names(plsbefore_rider)[names(plsbefore_rider)=='cyber_ride']<-"TRUST2"
names(plsbefore_rider)[names(plsbefore_rider)=='intent_ride']<-"INTENT"

plsbefore_rider = subset(plsbefore_rider, select = c("ATTITUDE1", "ATTITUDE2", "PU1", "PU2","PU3","PEofU1","PEofU2","TRUST1","TRUST2","INTENT"))

measurements.br <- constructs(
  composite("Attitude",        multi_items("ATTITUDE", 1:2)),
  composite("PEofU",  multi_items("PEofU",1:2)),
  composite("PU",        multi_items("PU", 1:3)),
  composite("Trust", multi_items("TRUST", 1:2)),
  composite("Intention", single_item("INTENT")),
  interaction_term(iv = "PU", moderator = "Attitude"),
  interaction_term(iv = "Trust", moderator = "Attitude")
)

structure.br <- relationships(
  paths(from = c("PU", "Trust"),  to = c("Attitude","Intention")),
  paths(from = "PEofU", to = c("Attitude","PU")),
  paths(from = "Attitude",  to = c("Intention"))
)

mobi_pls.br <- estimate_pls(
  data = plsbefore_rider,
  measurement_model = measurements.br,
  #measurement_model = as.reflective(measurements),
  structural_model = structure.br
  #inner_weights = path_weighting
)

summary(mobi_pls.br)
plot(mobi_pls.br, title = "PLS-SEM for riders (before)")

# BOOTSTRAP model
mobi_boot.br <- bootstrap_model(mobi_pls.br, nboot = 1000, cores = 2) #Note: Changed to 1000 and 2
summary(mobi_boot.br)
plot(mobi_boot.br, title = "Bootstrapped Model for riders (before)") #NOTE: Renamed
#Bootstrapped before rider model is fit

#### QUERY: Stopped here


# mobi_cfa <- estimate_cfa(
#   data = plsbefore_rider,
#   measurement_model = as.reflective(measurements)
# )
# 
# 
# mobi_cbsem <- estimate_cbsem(
#   data = plsbefore_rider,
#   measurement_model = as.reflective(measurements),
#   structural_model = structure
# )
# 
# summary(mobi_cbsem)
# 
# res <-  dot_graph(structure)
# DiagrammeR::grViz(res)

#beforedriver ----
names(plsbefore_drive)[names(plsbefore_drive)=='comfort_drive']<-"ATTITUDE1"
names(plsbefore_drive)[names(plsbefore_drive)=='comfort_drive2']<-"ATTITUDE2"
names(plsbefore_drive)[names(plsbefore_drive)=='slow_drive']<-"PU1"
names(plsbefore_drive)[names(plsbefore_drive)=='acc_drive']<-"PU2"
names(plsbefore_drive)[names(plsbefore_drive)=='comfort_change_drive']<-"PU3"
names(plsbefore_drive)[names(plsbefore_drive)=='signal_drive']<-"PEofU1"
names(plsbefore_drive)[names(plsbefore_drive)=='adjacent_drive']<-"TRUST1"
names(plsbefore_drive)[names(plsbefore_drive)=='follow_drive']<-"TRUST2"
names(plsbefore_drive)[names(plsbefore_drive)=='intent_ride']<-"INTENT"

plsbefore_drive = subset(plsbefore_drive, select = c("ATTITUDE1", "ATTITUDE2", "PU1", "PU2","PU3","PEofU1","TRUST1","TRUST2","INTENT"))

#NOTE: Renamed to make sure there isn't an error if something doesn't run properly
# Consider doing throughout the models. I used bd to symbolize before driver but you can use whatever code/abbreviations that you prefer
.bd <- constructs(
  composite("Attitude",        multi_items("ATTITUDE", 1:2)),
  composite("PEofU",  single_item("PEofU1")),
  composite("PU",        multi_items("PU", 1:3)),
  composite("Trust", multi_items("TRUST", 1:2)),
  #reflective("Intention", single_item("INTENT")) #Changed to reflective - doesn't change anything because we have 1 DV
  composite("Intention", single_item("INTENT"))
  )

structure.bd <- relationships(
  paths(from = "PU",  to = c("Attitude",'Intention')),
  paths(from = "PEofU", to = c("Attitude",'PU')),
  paths(from = "Trust", to = c("Attitude",'Intention')),
  paths(from = "Attitude",  to = c('Intention'))
)

mobi_pls.bd <- estimate_pls(
  data = plsbefore_drive,
  measurement_model = measurements.bd,
  #measurement_model = as.reflective(measurements.bd),
  structural_model = structure.bd
)

summary(mobi_pls.bd)
plot(mobi_pls.bd, title = "PLS-SEM for drivers (before)") #NOTE: Renamed to help in readability and organization

# NOTE: Added bootstrap - consider doing throughout
boot_estimates.bd <- bootstrap_model(mobi_pls.bd, nboot = 1000, cores = 2)
summary(boot_estimates.bd)
plot(boot_estimates.bd, title = "Bootstrapped Model for drivers (before)") #NOTE: Renamed

# NOTE: I commented  out the cfas throughout the code
# mobi_cfa <- estimate_cfa(
#   data = plsbefore_drive,
#   measurement_model = as.reflective(measurements)
# )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
# 
# mobi_cbsem <- estimate_cbsem(
#   data = plsbefore_drive,
#   measurement_model = as.reflective(measurements),
#   structural_model = structure
# )

#beforeped ----
names(plsbefore_ped)[names(plsbefore_ped)=='comfort_ped']<-"ATTITUDE1"
names(plsbefore_ped)[names(plsbefore_ped)=='comfort_ped2']<-"ATTITUDE2"
names(plsbefore_ped)[names(plsbefore_ped)=='crash_ped']<-"PU1"
names(plsbefore_ped)[names(plsbefore_ped)=='cross_ped']<-"PU2"
names(plsbefore_ped)[names(plsbefore_ped)=='easy_ped']<-"PEofU1"
names(plsbefore_ped)[names(plsbefore_ped)=='signal_ped']<-"PEofU2"
names(plsbefore_ped)[names(plsbefore_ped)=='adjacent_ped']<-"TRUST1"
names(plsbefore_ped)[names(plsbefore_ped)=='intent_ride']<-"INTENT"

plsbefore_ped = subset(plsbefore_ped, select = c("ATTITUDE1", "ATTITUDE2", "PU1", "PU2","PEofU1","PEofU2","TRUST1","INTENT"))

measurements.bp <- constructs(
  composite("Attitude",        multi_items("ATTITUDE", 1:2)),
  composite("PEofU",  multi_items("PEofU",1:2)),
  composite("PU",        multi_items("PU", 1:2)),
  composite("Trust", single_item("TRUST1")),
  composite("Intention", single_item("INTENT")),
  interaction_term(iv = "PU", moderator = "Attitude"),
  interaction_term(iv = "Trust", moderator = "Attitude")
)

structure.bp <- relationships(
  paths(from = "PU",  to = c("Attitude",'Intention')),
  paths(from = "PEofU", to = c("Attitude",'PU')),
  paths(from = "Trust", to = c("Attitude",'Intention')),
  paths(from = "Attitude",  to = c('Intention'))
)

mobi_pls.bp <- estimate_pls(
  data = plsbefore_ped,
  measurement_model = measurements.bp,
  #measurement_model = as.reflective(measurements.bp),
  structural_model = structure.bp
)

summary(mobi_pls.bp)
plot(mobi_pls.bp, title = "PLS-SEM")

# BOOTSTRAP model
mobi_boot.bp <- bootstrap_model(mobi_pls.bp, nboot = 1000, cores = 2) #Note: Changed to 1000 and 2
summary(mobi_boot.bp)
plot(mobi_boot.bp, title = "Bootstrapped Model for pedestrians and cyclists (before)") #NOTE: Renamed

#Boostraped before rider model is parlt fit. It is reasonable.

#afterrider ----
names(plsafter_rider)[names(plsafter_rider)=='comfort_ride']<-"ATTITUDE1"
names(plsafter_rider)[names(plsafter_rider)=='relax_ride']<-"ATTITUDE2"
names(plsafter_rider)[names(plsafter_rider)=='satisfy_ride']<-"ATTITUDE3"
names(plsafter_rider)[names(plsafter_rider)=='smooth_ride']<-"PU1"
names(plsafter_rider)[names(plsafter_rider)=='reliable_ride']<-"PU2"
names(plsafter_rider)[names(plsafter_rider)=='crashped_ride']<-"PU3"
names(plsafter_rider)[names(plsafter_rider)=='easy_ride']<-"PEofU1"
names(plsafter_rider)[names(plsafter_rider)=='control_ride']<-"PEofU2"
names(plsafter_rider)[names(plsafter_rider)=='signal_ride']<-"PEofU3"
names(plsafter_rider)[names(plsafter_rider)=='anxious_ride']<-"TRUST1"
names(plsafter_rider)[names(plsafter_rider)=='intent']<-"INTENT1"
names(plsafter_rider)[names(plsafter_rider)=='recommend']<-"INTENT2"

plsafter_rider = subset(plsafter_rider, select = c("ATTITUDE1", "ATTITUDE2","ATTITUDE3", "PU1", "PU2","PU3","PEofU1","PEofU2","PEofU3","TRUST1","INTENT1","INTENT2"))

measurements.ar <- constructs(
  composite("Attitude",        multi_items("ATTITUDE", 1:3)),
  composite("PEofU",  multi_items("PEofU",1:3)),
  composite("PU",        multi_items("PU", 1:3)),
  composite("Trust", single_item("TRUST1")),
  composite("Intention", multi_items("INTENT",1:2)),
  interaction_term(iv = "PU", moderator = "Attitude"),
  interaction_term(iv = "Trust", moderator = "Attitude")
)

structure.ar <- relationships(
  paths(from = c("PU", "Trust"),  to = c("Attitude","Intention")),
  paths(from = "PEofU", to = c("Attitude","PU")),
  paths(from = "Attitude",  to = c("Intention"))
)

mobi_pls.ar <- estimate_pls(
  data = plsafter_rider,
  measurement_model = measurements.ar,
  #measurement_model = as.reflective(measurements.ar),
  structural_model = structure.ar
  #inner_weights = path_weighting
)

summary(mobi_pls.ar)
plot(mobi_pls.ar, title = "PLS-SEM for riders (after)")

#QUERY:I use mobi_boot here, may be this is better
#QUERY: I only know the T tsat. from the mobi_boot result. I want to know the significance level
#of each path.Does the line width is realted to the significance? I am not so sure about that.
#NOTE: I also am unsure but it seems that you need to bootstrap to get sig values
#NOTE: Changed this to after rider rather than the mobi_boot which was recoded above

# BOOTSTRAP model
mobi_boot.ar <- bootstrap_model(mobi_pls.ar, nboot = 300, cores = 2) #Note: Changed to 1000 and 2
summary(mobi_boot.ar)
plot(mobi_boot.ar, title = "Bootstrapped Model for riders (after)") #NOTE: Renamed

#Bootstrapped after rider model is fit

# NOTE: Commented out. Highlighted section and hit ctrl + shift + c to comment out the entire section
# mobi_cfa <- estimate_cfa(
#   data = plsafter_rider,
#   measurement_model = as.reflective(measurements)
# )
# 
# 
# mobi_cbsem <- estimate_cbsem(
#   data = plsafter_rider,
#   measurement_model = as.reflective(measurements),
#   structural_model = structure
# )
# 
# summary(mobi_cbsem)

res <-  dot_graph(structure)
DiagrammeR::grViz(res)

#afterdriver
names(plsafter_drive)[names(plsafter_drive)=='comfort_drive']<-"ATTITUDE1"
names(plsafter_drive)[names(plsafter_drive)=='relax_drive']<-"ATTITUDE2"
names(plsafter_drive)[names(plsafter_drive)=='satisfy_drive']<-"ATTITUDE3"
names(plsafter_drive)[names(plsafter_drive)=='slow_drive']<-"PU3" #NOTE: Changed to PU3
names(plsafter_drive)[names(plsafter_drive)=='acc_drive']<-"PU1" #NOTE: This may be the issue? PU1 is repeated
names(plsafter_drive)[names(plsafter_drive)=='comfort_change_drive']<-"PU2"
names(plsafter_drive)[names(plsafter_drive)=='signal_drive']<-"PEofU1"
names(plsafter_drive)[names(plsafter_drive)=='easy_drive']<-"PEofU2"
names(plsafter_drive)[names(plsafter_drive)=='adjacent_drive']<-"TRUST1"
names(plsafter_drive)[names(plsafter_drive)=='follow_drive']<-"TRUST2"
names(plsafter_drive)[names(plsafter_drive)=='intent']<-"INTENT1"
names(plsafter_drive)[names(plsafter_drive)=='recommend']<-"INTENT2"
#QUERY:In my previoud model, PU=slow_drive, acc_drive,comfort_change_drive. But this construct
#can not run now. So I cut off "slow_drive"

plsafter_drive = subset(plsafter_drive, select = c("ATTITUDE1", "ATTITUDE2","ATTITUDE3", "PU1", "PU2", "PU3", "PEofU1","PEofU2","TRUST1","TRUST2","INTENT1","INTENT2"))
#NOTE: Added "PU3"

measurements.ad <- constructs(
  composite("Attitude",        multi_items("ATTITUDE", 1:3)),
  composite("PEofU",  multi_items("PEofU",1:2)),
  composite("PU",        multi_items("PU", 1:3)), #NOTE: Change from 1:2 to 1:3
  composite("Trust", multi_items("TRUST", 1:2)),
  composite("Intention", multi_items("INTENT",1:2)),
  interaction_term(iv = "PU", moderator = "Attitude"),
  interaction_term(iv = "Trust", moderator = "Attitude")
)

structure.ad <- relationships(
  paths(from = "PU",  to = c("Attitude",'Intention')),
  paths(from = "PEofU", to = c("Attitude",'PU')),
  paths(from = "Trust", to = c("Attitude",'Intention')),
  paths(from = "Attitude",  to = c('Intention'))
)

mobi_pls.ad <- estimate_pls(
  data = plsafter_drive,
  #measurement_model = as.reflective(measurements.ad),
  measurement_model = measurements.ad,
  structural_model = structure.ad
)

summary(mobi_pls.ad)
plot(mobi_pls.ad, title = "PLS-SEM")

# BOOTSTRAP model
mobi_boot.ad <- bootstrap_model(mobi_pls.ad, nboot = 300, cores = 2) #Note: Changed to 1000 and 2
summary(mobi_boot.ad)
plot(mobi_boot.ad, title = "Bootstrapped Model for drivers (after)") #NOTE: Renamed


# mobi_cfa <- estimate_cfa(
#   data = plsafter_drive,
#   measurement_model = as.reflective(measurements)
# )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
# 
# mobi_cbsem <- estimate_cbsem(
#   data = plsafter_drive,
#   measurement_model = as.reflective(measurements),
#   structural_model = structure
# )

#afterped
names(plsafter_ped)[names(plsafter_ped)=='comfort_ped']<-"ATTITUDE1"
names(plsafter_ped)[names(plsafter_ped)=='relax_ped']<-"ATTITUDE2"
names(plsafter_ped)[names(plsafter_ped)=='satisfy_ped']<-"ATTITUDE3"
names(plsafter_ped)[names(plsafter_ped)=='crash_ped']<-"PU1"
names(plsafter_ped)[names(plsafter_ped)=='cross_ped']<-"PU2"
names(plsafter_ped)[names(plsafter_ped)=='easy_ped']<-"PEofU1"
names(plsafter_ped)[names(plsafter_ped)=='attent_ped']<-"TRUST1"
names(plsafter_ped)[names(plsafter_ped)=='adjacent_ped']<-"TRUST2"
names(plsafter_ped)[names(plsafter_ped)=='intent']<-"INTENT1"
names(plsafter_ped)[names(plsafter_ped)=='recommend']<-"INTENT2"

plsafter_ped = subset(plsafter_ped, select = c("ATTITUDE1", "ATTITUDE2",  "ATTITUDE3", "PU1", "PU2","PEofU1","TRUST1","TRUST2","INTENT1","INTENT2"))

measurements.ap <- constructs(
  composite("Attitude",        multi_items("ATTITUDE", 1:3)),
  composite("PEofU",  single_item("PEofU1")),
  composite("PU",        multi_items("PU", 1:2)),
  composite("Trust", multi_items("TRUST",1:2)),
  composite("Intention", multi_items("INTENT",1:2)),
  interaction_term(iv = "PU", moderator = "Attitude"),
  interaction_term(iv = "Trust", moderator = "Attitude")
)

structure.ap <- relationships(
  paths(from = "PU",  to = c("Attitude",'Intention')),
  paths(from = "PEofU", to = c("Attitude",'PU')),
  paths(from = "Trust", to = c("Attitude",'Intention')),
  paths(from = "Attitude",  to = c('Intention'))
)

mobi_pls.ap <- estimate_pls(
  data = plsafter_ped,
  measurement_model = measurements.ap,
  #measurement_model = as.reflective(measurements.ap),
  structural_model = structure.ap
)
summary(mobi_pls.ap)
plot(mobi_pls.ap, title = "PLS-SEM")

# mobi_cfa <- estimate_cfa(
#   data = plsafter_drive,
#   measurement_model = as.reflective(measurements)
# )                  
# 
# summary(mobi_cfa)

mobi_boot.ap <- bootstrap_model(mobi_pls.ap, nboot = 300, cores = 2)
summary(mobi_boot.ap)
plot(mobi_boot.ap, title = "Bootstrapped Model for pedestrians and cyclists (after")

#Bootstrapped after ped model is partly fit. It is reasonable
