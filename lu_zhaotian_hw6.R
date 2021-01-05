#########################
#   Load DATA           #
#   Explore             #
#########################
rm(list=ls())
#Mobile apps data

gmapps <- read.csv("GlobalMobileAppsExercise.csv", sep=",", header=T); # data is loaded to a frame named gmapps

#The attach() function in R can be used to make objects within dataframes accessible in R with fewer keystrokes
attach(gmapps) 

#Features
names(gmapps)

#The str function shows an outline of the structure of its argument
str(gmapps)


#Create summary
summary(gmapps)
#check dummy info
levels(factor(gmapps$in_app_purchase))
levels(factor(gmapps$app_type))
#create new variables
gmapps$lrank = log(gmapps$rank)
gmapps$lprice = log(1+gmapps$price)
gmapps$lfilesize = log(gmapps$filesize)
gmapps$lnum_screenshot=log(gmapps$num_screenshot)
gmapps$laverage_rating=log(1+gmapps$average_rating)
gmapps$lapp_age_current_version=log(1+gmapps$app_age_current_version)

gmapps$t_day=factor(gmapps$t_day)
gmapps$app_store=factor(gmapps$app_store)
gmapps$inapp_addummy = factor(inapp_addummy)
gmapps$region=factor(gmapps$region)
gmapps$inapp_purchasedummy=factor(gmapps$inapp_purchasedummy)
gmapps$category=factor(gmapps$category)
gmapps$app_type=factor(gmapps$app_type)
gmapps$device=factor(gmapps$device)
gmapps$developer=factor(gmapps$developer)
gmapps$categoryindex=factor(categoryindex)     
#build the model
model<-lm(lrank~lprice+lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
            +laverage_rating+inapp_addummy+region+inapp_purchasedummy+category+app_type+
            +app_store+device+developer,data=gmapps)
summary(model)

summary(model)$coefficients[1:30,]
exp(coef(model))
#Run a  split sample analysis by region

summary(gmapps$region)


r5US <- lm(lrank~lprice+lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
             +laverage_rating+inapp_addummy+inapp_purchasedummy+category+app_type+
             +app_store+device+developer, data=gmapps[rindex==2,])
summary(r5US)$coefficients[1:50,]
summary(r5US)
r5China <- lm(lrank~lprice+lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
                +laverage_rating+inapp_addummy+inapp_purchasedummy+category+app_type+
                +app_store+device+developer, data=gmapps[region=="CN",])
summary(r5China)$coefficients[1:30,]
summary(r5China)

#95% CI for the average_rating coeff
confint(r5US, 'laverage_rating', level=0.95)
confint(r5China, 'laverage_rating', level=0.95)


# two sample t-test to comapre coeffs --> is average rating sensitivity higher in china than US

diffsenregion <- summary(r5US)$coefficients['laverage_rating',1]-summary(r5China)$coefficients['laverage_rating',1]
denom <-sqrt(summary(r5US)$coefficients['laverage_rating',2]+summary(r5China)$coefficients['laverage_rating',2])
diffsenregion
denom
tval <-diffsenregion/denom
tval
(qt(0.05, 10574)) # t-critical

#Run a  split sample analysis by device specific

summary(gmapps$device)


rphone<- lm(lrank~lprice+lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
             +laverage_rating+inapp_addummy+inapp_purchasedummy+category+app_type+
             +app_store+developer+region, data=gmapps[device=="smart_phone",])
summary(rphone)$coefficients[1:30,]

rtablet<- lm(lrank~lprice+lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
               +laverage_rating+inapp_addummy+inapp_purchasedummy+category+app_type+
               +developer+region, data=gmapps[deviceindex==2,])
summary(rtablet)$coefficients[1:30,]


#95% CI for the average_rating coeff
confint(rphone, 'laverage_rating', level=0.95)
confint(rtablet, 'laverage_rating', level=0.95)


# two sample t-test to comapre coeffs --> is average rating sensitivity higher in tablet than phone

diffsendevice <- summary(rphone)$coefficients['laverage_rating',1]-summary(rtablet)$coefficients['laverage_rating',1]
denomdevice <-sqrt(summary(rphone)$coefficients['laverage_rating',2]+summary(rtablet)$coefficients['laverage_rating',2])
diffsendevice
denomdevice
tvaldevice <-diffsendevice/denomdevice
tvaldevice
(qt(0.05, 10445)) # t-critical

#Run a  split sample analysis by platform specific

summary(gmapps$app_store)


rapple<- lm(lrank~lprice+lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
              +laverage_rating+inapp_addummy+inapp_purchasedummy+category+app_type+
              +developer+region+device, data=gmapps[app_store=="Apple",])
summary(rapple)$coefficients[1:30,]

rgoogle<- lm(lrank~lprice+lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
               +laverage_rating+inapp_addummy+inapp_purchasedummy+category+app_type+
               +developer+region, data=gmapps[app_store=="Google Play",])
summary(rgoogle)$coefficients[1:30,]


#95% CI for the average_rating coeff
confint(rapple, 'laverage_rating', level=0.95)
confint(rgoogle, 'laverage_rating', level=0.95)


# two sample t-test to comapre coeffs --> is average rating sensitivity higher in google than apple

diffsenplatform <- summary(rapple)$coefficients['laverage_rating',1]-summary(rgoogle)$coefficients['laverage_rating',1]
denomplatform <-sqrt(summary(rapple)$coefficients['laverage_rating',2]+summary(rgoogle)$coefficients['laverage_rating',2])
diffsenplatform
denomplatform
tvalplatform <-diffsenplatform/denomplatform
tvalplatform
(qt(0.05,3543)) # t-critical

#Run a  split sample analysis by price

summary(gmapps$app_type)


rfree<- lm(lrank~lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
              +laverage_rating+inapp_addummy+inapp_purchasedummy+category+
              +developer+region+device, data=gmapps[app_type=="free",])
summary(rfree)$coefficients[1:30,]

rpaid<- lm(lrank~lfilesize+lnum_screenshot+t_day+lapp_age_current_version+
             +laverage_rating+inapp_addummy+inapp_purchasedummy+category+
             +developer+region+device, data=gmapps[app_type=="paid",])


#95% CI for the average_rating coeff
confint(rfree, 'laverage_rating', level=0.95)
confint(rpaid, 'laverage_rating', level=0.95)


# two sample t-test to comapre coeffs --> is average rating sensitivity higher in paid than free

diffsenprice <- summary(rfree)$coefficients['laverage_rating',1]-summary(rpaid)$coefficients['laverage_rating',1]
denomprice <-sqrt(summary(rfree)$coefficients['laverage_rating',2]+summary(rpaid)$coefficients['laverage_rating',2])
diffsenprice
denomprice
tvalprice <-diffsenprice/denomprice
tvalprice
(qt(0.05,3543)) # t-critical