#############################################
## KC HOMES
##
#############################################

############################################
###########################################
## FUNCTION:

#pre:
#### fn.sqft.living: living room space of house (numeric)
#### fn.grade: grade of house (numeric)
#### fn.yr.built: year built of house (numeric)
#### fn.lat: latitude of house (numeric)
#### fn.view: rating of view from house  (numeric)
#### data.loaded: boolean signaling whether houses dataset is already loaded in R enviornment. 
####     If False, function will load the data set. If true, loading step is skipped. Default is set to False.
####     Data set must be stored in kc.homes variable in order to use True flag.
#post: 
#### returns a numeric value
#usage: 
#### Enter values from a house who's price you wish to predict for each 'fn' variable.
#### If 'fn' values are not specified, they will be treated as zero.  
get_price <-function(fn.sqft.living=0,fn.grade=0,fn.yr.built=0,fn.lat=0,fn.view=0 ,data.loaded=F){
  if(data.loaded==F){
    kc.homes <- read.table("http://mkahn.webspace.wheatoncollege.edu/math251/datasets/Stat2Data/kc-house-data.txt",
                                            header=T,sep="\t",quote="\"")
    kc.homes$lat_bin <- ifelse(kc.homes$lat > 47.535, 1, 0) 
  }
  fn.model <-lm(log(price) ~ sqft.living + grade + yr.built + lat_bin + sqft.living*(view), data=kc.homes)
  fn.lat_bin = ifelse(fn.lat > 47.535, 1, 0)
  p.price <- exp(predict(fn.model, data.frame("sqft.living" = fn.sqft.living, "grade"=fn.grade,"yr.built"=fn.yr.built,
                                          "lat_bin"=fn.lat_bin,"view"=fn.view)))
  round(as.numeric(p.price), digits=2)
}

##for example: 
house_1 <- get_price(fn.sqft.living=	1680,fn.grade=8,fn.yr.built=1987,fn.lat=47.6168,fn.view=0,data.loaded=T)
##predeicts price of $499,316 -- actual price is $510,000




#########################################
##IMPORT AND CLEANING 
########################################
steelblue <-"#4682B433"
steelblue2 <-"#4682B466"
kc.homes <- read.table("http://mkahn.webspace.wheatoncollege.edu/math251/datasets/Stat2Data/kc-house-data.txt",
                                header=T,sep="\t",quote="\"")
kc.homes$lat_bin <- ifelse(kc.homes$lat > 47.535, 1, 0) 
attach(kc.homes)
#fix dates
kc.homes$date <- substr(kc.homes$date, 1,8)
kc.homes$date <- as.numeric(as.character(kc.homes$date))


##see all relationships
##sampled b/c w/ all entries it took forever
sample.homes.inds <- sample(1:nrow(kc.homes),size=200,replace=F)
sample.homes <- kc.homes[sample.homes.inds, ]
pairs(sample.homes)

#########################
#MODELING
##some tests 

train.lm.price <-lm(log(price) ~ sqft.living)
summary(train.lm.price)
plot(log(price)~sqft.living, data=kc.homes, pch=20, col = steelblue )
lines(lowess(sqft.living,price), lwd=2)
abline(train.lm.price$coefficients, lwd=3, col=rgb(0.25,0.25,.25,1/1))
hist(train.lm.price$residuals, border=F, col="steelblue",
     main="distribution of residuals", freq=F)

# train.lm.price <-lm(price ~ condition)
# summary(train.lm.price)
# plot(price~condition, data=kc.homes, pch=20, col = rgb(0.20,0.35,1,4/16) )
# lines(lowess(condition,price), lwd=2)
# abline(train.lm.price$coefficients, lwd=3, col=rgb(0.25,0.25,.25,1/1))
# hist(train.lm.price$residuals, border=F, col="steelblue",
#      main="distribution of residuals", freq=F)


#####################################################
### FINAL MODEL ####################################
### 
#######################
####################### 
#####################################################

#training -- some testing cross validation (only 2fold)
t.rows <- sample(1:nrow(kc.homes), size=0.5*nrow(kc.homes))
h.rows <- (1:nrow(kc.homes))[-sort(t.rows)]
homes.t <- kc.homes[sort(t.rows),]
homes.h <- kc.homes[sort(h.rows),]

###LOOK AT LOG RELATIONSHIP OF PRICE -- SEE WHICH RESPOND WELL // WHICH WORK POORLY

###################################
# the model
####################################
train.lm.price <-lm(log(price) ~ sqft.living + grade + yr.built + waterfront+ lat_bin + sqft.living*(view), data=kc.homes)
summary(train.lm.price)

#########################################
# cross validate analysis + visualiztions
#########################################
h.lm.homes.predictions <- exp(predict(train.lm.price, newdata=homes.h[,c("sqft.living","grade","yr.built", "lat", "view", "long","waterfront")]))
head(h.lm.homes.predictions) ##predicted prices dollar value
homes.res <- homes.h$price - h.lm.homes.predictions ##actual dollar differences (not log)
head(homes.res)
homes.res <- as.data.frame(homes.res)

homes.res$std.val <- homes.res$homes.res / sd(homes.res$homes.res)
#Sd of all res then div by the sd of all residuals -- (zscore type of thing) find outliers 
## find all more than 2 or 3 sd off -- track/look at these outliers (graohs etc)

##residual plot no CV
# par(mfrow=c(1,2))
hist(train.lm.price$residuals, border=F, col=steelblue2,add=F,
       main="distribution of model residuals", freq=F)
##error normally dist around 0
hist(homes.res$homes.res, border=F, freq=F, col="#ff000077", main="distribution of residuals 
  (holdout - predictions)", breaks = 50, xlim=c(-1000000,1000000))

######################################
### observe ind. relationships
### for above model
######################################



################
## LATITUDE
## SPLIT LAT INTO 0,1 AROUDN 47.53 BOUNDRY
##################

train.lm.price <-lm(log(price) ~ poly(lat,degree=3))
summary(train.lm.price)
plot(log(price) ~ lat, col=steelblue,
     main="log price ($) vs latitude (idk the units lol)")
lines(lowess(lat,log(price), f=1/8), lty=4, col="chartreuse")
#draw least squares line
x <- with(kc.homes, seq(min(lat), max(lat), length.out=2000))
y <- predict(train.lm.price, newdata = data.frame(lat = x))
lines(x, y, col = "darkblue", lwd=2)
##track confidence interval
## if choosing one specific price based on lat 95% conf between dotted lines
yci <-as.data.frame(predict(train.lm.price, newdata = data.frame(lat = x), interval = "predict"))
lines(x, yci$upr, col="darkgreen", lwd=2, lty=2);lines(x, yci$lwr, col="darkgreen", lwd=2, lty=2)##looks too parallel?
  #resid plot
hist(train.lm.price$residuals, border=F, freq=F, col=steelblue, main="distribution of residuals")

###########
##GRADE
###########
##log bcause apparent exponential relationship w/ grade
train.lm.price <-lm(log(price) ~ grade)
plot(log(price) ~ jitter(grade, factor=1/4), col=steelblue)
summary(train.lm.price)
abline(train.lm.price$coefficients, col="darkblue", lwd=2)
hist(train.lm.price$residuals, border=F, freq=F, col=steelblue, main="distribution of residuals")


######################
## LATITUDE // BINARY
######################
tst.mod <- lm(log(kc.homes$price) ~ kc.homes$lat_bin)
summary(tst.mod)
boxplot(log(price) ~ lat_bin)
# tst.mod <-lm(price ~ waterfront)
# summary(tst.mod)

###########################################
## OLD // DEPRECATED MODEL
###########################################
kc.homes$sqft.living.sq <- sqft.living*sqft.living
##big model
##add squared variables and create larger model
kc.homes$lat.sq <- lat*lat
kc.homes$grade.sq <-grade*grade
kc.homes$bedrooms.sq <-bedrooms*bedrooms
detach(kc.homes)
attach(kc.homes)##add new terms to attach

lm.training.price <-lm(log(price) ~ sqft.living + sqft.living15 + floors + 
                condition + yr.built + yr.renovated + waterfront + 
                zipcode + lat + long + grade + floors*zipcode + lat*long + bedrooms + sqft.living*sqft.living15 + sqft.lot15 + kc.homes$date)
summary(lm.training.price)

hist(lm.training.price$residuals, border=F, col="steelblue",
     main="distribution of residuals", freq=F)

###################################
#--------- 4 fold CROSS VALIDATION
# train.lm.price <-lm(log(price) ~ sqft.living + grade + yr.built + poly(lat, degree = 3) + sqft.living*(view), data=homes.t)
# ##lat^3 look at plot below for why
# summary(train.lm.price)
###################################

permute.rows <- sample(1:nrow(kc.homes), size=nrow(kc.homes), replace=FALSE)##shuffle numbers
subset1.rows <- permute.rows[1:5402] 
subset2.rows <- permute.rows[5402:10805]
subset3.rows <- permute.rows[10806:16209]
subset4.rows <- permute.rows[16210:21613]
subset.names <- c("subset1.rows","subset2.rows",
                  "subset3.rows","subset4.rows")
Se.compare <- NULL
for(i in 1:4){
  holdout.rows <- get(subset.names[i])
  training.rows <- c(get(subset.names[-i][1]),
                     get(subset.names[-i][2]),
                     get(subset.names[-i][3]))
  
  ## let fev.training be the rows of the original dataset for 3 subsets
  price.training <- kc.homes[sort(training.rows),]
  
  ## let fev.holdout be the selected rows of fourth subset
  price.holdout <- kc.homes[sort(holdout.rows),]
  
  ## fit the model we liked best above just to the training dataset
  train.lm.price <-lm(log(price) ~ sqft.living + grade + yr.built + lat_bin +  sqft.living*(view), data=price.training)
  summary(train.lm.price)
  
  ## get predictions from the above model for the holdout data
  holdout.lm.price.predictions <- predict(train.lm.price ,
                                        newdata=price.holdout[,c("sqft.living","grade","yr.built","lat_bin", "view")])
  
  ## compute observed - predicted = residuals for the holdout dataset
  
  ## USE LOG(HOLDOUTS) B/C MODEL RETURNS RSE IN TERMS OF LOG(PRICE) -- make the two comparable
  holdout.lm.price.resids <- log(price.holdout[,"price"]) - holdout.lm.price.predictions
  
  ## Compute Resid Se for test, compare to Resid Se from training
  n <- length(holdout.lm.price.resids)
  Se.test <- sd(holdout.lm.price.resids,na.rm=TRUE)*sqrt((n-1)/(n-5))
  Se.compare <- rbind(Se.compare,
                      c(summary(train.lm.price)$sigma , Se.test))
  
}
##both RSE in terms of log prices
Se.compare


#############################
## BOOTSTRAP of SQFT LIVING
############ change var names
##############################

n.fev <- dim(kc.homes)[1]
B <- 5000
fev.reg.bootstrap <- NULL
for(k in 1:B){
  resample.inds <- sample(1:n.fev,size=n.fev,replace=TRUE)
  resample.fevdata <- kc.homes[resample.inds,]
  resample.fev <- resample.fevdata$fev
  resample.smoke <- resample.fevdata$smoke
  resample.age <- resample.fevdata$age
  ### use resampled data to fit regression
  tmp.lm <- lm(log(price) ~ sqft.living, data=resample.fevdata)
  ### save "fourth" coefficient (b_3), from interaction
  tmp.coef <- coefficients(tmp.lm)[2]
  fev.reg.bootstrap <- c(fev.reg.bootstrap , tmp.coef)
}
hist(fev.reg.bootstrap ,
     xlab="5000 resampled (n=21613) bootstrap coefficients of sqft.living",
     freq=F,main="", col=steelblue2, border = F)
title(main=paste("Bootstrap distribution of coefficient of sqft.living, 
      log(price) = B0 + B1*SQFT.LIVING",sep="\n"),
      cex.main=.8)

quantile(fev.reg.bootstrap , probs=c(.025,.975))
mean(fev.reg.bootstrap)


# EXPORT COORDS for GIS plotting
# points <- kc.homes[,c(3,18,19)]
# points <- points[order(-price),]
# tst <- points[c(1:50),c(2,3)]
# 
# write.csv(points[c(1:500),c(2,3)], "points_max3.csv", row.names = F)
# write.csv(points[c(21113:21613),c(2,3)], "points_min3.csv", row.names = F)
# getwd()



################################
#------ standardize predictors
################################
# kc.homes.std <- read.table("http://mkahn.webspace.wheatoncollege.edu/math251/datasets/Stat2Data/kc-house-data.txt",
#                            header=T,sep="\t",quote="\"")
# kc.homes.std$lat_bin <- ifelse(kc.homes.std$lat > 47.535, 1, 0) 
# 
# for(i in names(kc.homes.std)[-c(1,2,3)]){
#   kc.homes.std[ , i ] <-
#     (kc.homes.std[ , i ] - mean(kc.homes.std[ , i ], na.rm = T))/sd(kc.homes.std[ , i ], na.rm = T)
# }
# train.lm.price <-lm(log(price) ~ sqft.living + grade + lat_bin, + view, data=kc.homes.std)
# summary(train.lm.price)
