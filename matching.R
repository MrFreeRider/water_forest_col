rm(list = ls())

library(ggplot2)
library(extrafont)
library(RColorBrewer)

library(csvread)  #Import CSV files
library(reshape2)
library(dplyr)
library(tidyverse)

library(plotly)
library(Matching) #Matching process
library(rgenoud) #Genetic Optimization
library(knitr) #Rmarkdown Dynamic Report
library(xtable) #Export latex tables
library(cobalt) #Covariate balance and plots
library(glm2) #glm packages for 3.6 version
library(MatchIt)
library(gridExtra) #groups of grapha

setwd('/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water')

detach(dt_1km)

data_baseline <- read.csv('data_baseline.csv',header=TRUE)
names(data_baseline)
attach(data_baseline)
#detach(data_baseline)

#******* 1 kilometer
#The covariates we want to match on (at 1km) without dependent variable at baseline
dt_1km <-  tibble(pa, d_se, d_coke, d_cap, d_min,
                 rain_pre, temp_pre, slope1km, eleva1km, n_cap,
                 area_mp, y_pc, arm_att,vc_pre, ftreat1)

dt_1km <- na.omit(dt_1km)  #omit na's

names(dt_1km)

detach(data_baseline)
attach(dt_1km)
names(dt_1km)
covar1km <- cbind(pa,d_se,d_coke,d_cap,d_min,rain_pre,temp_pre,slope1km,
                    eleva1km, area_mp, vc_pre)

#********************************************
## Nearest Neighbor PSM
#********************************************
require(MatchIt) 

m.out.nn.1km <- matchit(ftreat1~covar1km, method = "nearest"
                             ,data = dt_1km, ratio = 1, replace=TRUE)
#### Checking the sample size after matching
m.out.nn.1km
### final match data saved (here distance is the pscore)
nn.1km <- match.data(m.out.nn.1km)
head(nn.1km)
pair.nn.1km <- m.out.nn.1km$match.matrix
setwd('/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching')

## Saving results
write.csv(nn.1km, 
          file="nn.1km.cvs")
write.csv(pair.nn.1km, 
          file="pair.nn.1km.cvs")


## Balance
bal.tab(m.out.nn.1km, un=TRUE, m.threshold = .1, v.threshold = 2)
bal.plot(m.out.nn.1km, var.name = "pa")



##Variable names for plot pourpose
varnames <- data.frame(old=c("pa","d_se","d_coke","d_cap", "d_min","rain_pre",
                             "temp_pre","slope1km",  "eleva1km",
                             "area_mp",  "vc_pre",
                             "ftreat1", "distance"),
                       new=c("Protected area", "D. to Strategic Ecosystems", "D. to Coke crops",
                             "D. to Capital city", "D. to mining", "Rainfall", "Temperature",
                             "Slope", "Elevation", "Municipal Area", "Cost <= 2014",
                             "Treated 1km", "Propensity Score"))


#covariate balance
############################################
jpeg("nn.1km.covbal.jpg")
love.plot(m.out.nn.1km, stat = "mean.diff", treshold=0.1, 
          var.order = "unadjusted", var.names = varnames, abs = TRUE,
          line = TRUE, limits = c(0,1))
dev.off()

#Visual inspection
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$ftreat1 <- as.factor(dta$ftreat1)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = ftreat1)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support) 
}

jpeg("nn.1km.visual_insp.jpg")
grid.arrange(
  fn_bal(nn.1km, "vc_pre") + 
    theme(legend.position = "none")+
    ylab("Cost"),
  fn_bal(nn.1km, "pa") + 
    theme(legend.position = "none")+
    ylab("Prot. Area"),
  fn_bal(nn.1km, "d_se") + 
    theme(legend.position = "none")+
    ylab("D. ES"),
  fn_bal(nn.1km, "d_coke") + 
    theme(legend.position = "none")+
    ylab("D. Coke"),
  fn_bal(nn.1km, "d_cap") + 
    theme(legend.position = "none")+
    ylab("D. Capital"),
  fn_bal(nn.1km, "d_min") + 
    theme(legend.position = "none")+
    ylab("D. Mining"),
  fn_bal(nn.1km, "rain_pre") +
    theme(legend.position = "none")+
    ylab("Rainfall"),
  fn_bal(nn.1km, "temp_pre") + 
    theme(legend.position = "none")+
    ylab("Temperature"),
  fn_bal(nn.1km, "slope1km")+ 
    theme(legend.position = "none")+
    ylab("Slope"),
  fn_bal(nn.1km, "eleva1km")+ 
    theme(legend.position = "none")+
    ylab("Elevation"),
  fn_bal(nn.1km, "area_mp") +
    ylab("Area km2"),
  nrow = 6, widths = c(0.9, 0.9)) 
dev.off()
#Histograms
jpeg("hist.nn.1km.jpg")
plot(m.out.nn.1km, type = "hist") #check matched treated vs matched control
dev.off()
############################################

#********************************************
## Caliper matching (perform)
###########################################

m.out.test <- matchit(ftreat1~covar1km, method = "nearest",
                      data = dt_1km, ratio = 1)
test_data <- match.data(m.out.test) 
ps.sd = sd(test_data$distance)

# matching is performed below using propensity scores given the covariates mentioned below
# caliper = 0.25 times sd of propensity scores (optimal)
m.out.caliper.1km = matchit(ftreat1~covar1km,method="nearest", 
                            data=dt_1km, caliper = 0.2*ps.sd, replace=TRUE)
# check the sample sizes (below)
m.out.caliper.1km
# Final matched data saved as final_data
caliper.1km = match.data(m.out.caliper.1km)
head(caliper.1km)
pair.caliper.1km <- m.out.caliper.1km$match.matrix

## Saving results
write.csv(caliper.1km, 
          file="caliper.1km.cvs")
write.csv(pair.caliper.1km, 
          file="pair.caliper.1km.cvs")

## Balance
bal.tab(m.out.caliper.1km, un=TRUE, m.threshold = .1, v.threshold = 2)
bal.plot(m.out.caliper.1km, var.name = "vc_pre")

#Covariate balance
############################################
jpeg("caliper.1km.covbal.jpg")
love.plot(m.out.caliper.1km, stat = "mean.diff", treshold=0.1, 
          var.order = "unadjusted", var.names = varnames, abs = TRUE,
          line = TRUE, limits = c(0,1))
dev.off()

jpeg("caliper.1km.visual_insp.jpg")
grid.arrange(
  fn_bal(caliper.1km, "vc_pre") + 
    theme(legend.position = "none")+
    ylab("Cost"),
  fn_bal(caliper.1km, "pa") + 
    theme(legend.position = "none")+
    ylab("Prot. Area"),
  fn_bal(caliper.1km, "d_se") + 
    theme(legend.position = "none")+
    ylab("D. ES"),
  fn_bal(caliper.1km, "d_coke") + 
    theme(legend.position = "none")+
    ylab("D. Coke"),
  fn_bal(caliper.1km, "d_cap") + 
    theme(legend.position = "none")+
    ylab("D. Capital"),
  fn_bal(caliper.1km, "d_min") + 
    theme(legend.position = "none")+
    ylab("D. Mining"),
  fn_bal(caliper.1km, "rain_pre") +
    theme(legend.position = "none")+
    ylab("Rainfall"),
  fn_bal(caliper.1km, "temp_pre") + 
    theme(legend.position = "none")+
    ylab("Temperature"),
  fn_bal(caliper.1km, "slope1km")+ 
    theme(legend.position = "none")+
    ylab("Slope"),
  fn_bal(caliper.1km, "eleva1km")+ 
    theme(legend.position = "none")+
    ylab("Elevation"),
  fn_bal(caliper.1km, "area_mp") +
    ylab("Area km2"),
  nrow = 6, widths = c(0.9, 0.9)) 
dev.off()

jpeg("hist.caliper.1km.jpg")
plot(m.out.caliper.1km, type = "hist")
dev.off()
############################################

############################################
#********************************************
## Mahalonobis Multivariate Matching
############################################

require(MatchIt)
m.out.maha.1km <- matchit(ftreat1~covar1km, method = "nearest",
                             distance="mahalanobis",data = dt_1km, 
                            ratio = 1)
#### Checking the sample size after matching
m.out.maha.1km
### final match data saved as mahalanobis_1km (here distance is the pscore)
maha.1km <- match.data(m.out.maha.1km)
head(m.out.maha.1km)
pair.maha.1km <- m.out.maha.1km$match.matrix
setwd('/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/gen_matching')

write.csv(maha.1km, 
          file="nearest.1km.cvs")
write.csv(pair.maha.1km, 
          file="pair.nearest.1km.cvs")



jpeg("maha.1km.covbal.jpg")
love.plot(m.out.maha.1km, stat = "mean.diff", treshold=0.1, 
          var.order = "unadjusted", var.names = varnames, abs = TRUE,
          line = TRUE, limits = c(0,1))
dev.off()


jpeg("all.covbal.jpg")
love.plot(ftreat1~covar1km,data=dt_1km,
          weights = data.frame(Matched=get.w(m.out.nn.1km),
                               Caliper=get.w(m.out.caliper.1km),
                               Mahalanobis=get.w(m.out.maha.1km)),
          method = c("matching", "matching", "matching"),
          var.order = "unadjusted",
          abs = TRUE)
dev.off()

#********************************************
## Matching initial example
#********************************************
# We now estimate our first propensity score model
# 1-1 matching with replacement


glm1km <-  glm(ftreat1 ~ covar1km, family = binomial, data = dt_1km)

glm1 <-  glm(ftreat1 ~ pa+d_se+d_coke+d_cap+d_min+rain_pre+temp_pre+slope1km+
               eleva1km+vc_pre, family = binomial, data = dt_1km)
summary(glm1)

summary(glm1km)
## Match 1 kilometer
m1km <- Match( Tr = ftreat1, X = glm1km$fitted)
summary(m1km)

mb <- MatchBalance(ftreat1 ~ covar1km, match.out = m.out.maha.1km, 
             nboots = 1000, data = dt_1km)


library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

xtable(glm1km)

#********************************************
## Genetic Matching
#********************************************

BalanceMat1km <- cbind(pa, d_ser, I(d_ser^2), d_coke,I(d_coke^2), d_capital,
                  I(d_capital^2), d_min, I(d_min^2), rain_bsl, I(rain_bsl^2),
                  temp_bsl, I(temp_bsl^2), slope1km, I(slope1km^2),eleva1km,
                  I(eleva1km^2), n_cap, ubn_bsl,I(ubn_bsl^2), area_mp, I(area_mp^2),
                  pump, sup_intk, org_loc, org_pub)

#  finds optimal balance using multivariate matching where a genetic search 
# algorithm determines the weight each covariate is given

gen1 <- GenMatch(forest_treat1, covar1km, BalanceMatrix = BalanceMat1km,
                 pop.size = 100)

mgen1 <- Match( Tr=forest_treat1, X = covar1km, Weight.matrix = gen1)

mgen1$index.control


MatchBalance(forest_treat1 ~ covar1, match.out = mgen1, 
             nboots = 1000, data = dt_1km)




require(ggplot2)
ggplot(data_baseline, aes(x = psc1, fill = forest_treat1)) + 
  geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.02)




