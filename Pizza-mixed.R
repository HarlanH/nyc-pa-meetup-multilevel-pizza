# Pizza data analysis with mixed-effects modeling
# Harlan D. Harris, Jared Lander
# for NYC Predictive Analytics Meetup
# October 14th, 2010

library(stringr)
library(ggplot2)
library(nlme)
library(lme4)
library(boot) # probably not needed
library(arm)

# custom routines to cross-validate lm and lme (but not mer) regressions,
# using all of the data to train, but only a subset to test
source("cv_subset.R")

za.df <- read.csv("Fake Pizza Data.csv")[,c(1,2,3,5)]

# fix cost column
#za.df$CostPerSlice <- as.numeric(str_sub(za.df$CostPerSlice, 2))

# get the contrasts right for HeatSource
contrasts(za.df$HeatSource) <- contr.treatment(levels(za.df$HeatSource), 
	base=which(levels(za.df$HeatSource) == 'Gas'))

# some basic summaries and visualizations
summary(za.df)

# we'll use this later for cross-validation
inchinatown <- za.df$Neighborhood == 'Chinatown'

# center the scalars, and construct a function to uncenter them
mean.cps <- mean(za.df$CostPerSlice)
mean.rating <- mean(za.df$Rating)
za.df$CostPerSlice <- za.df$CostPerSlice - mean.cps
za.df$Rating <- za.df$Rating - mean.rating
center <- function(df) transform(df, CostPerSlice=CostPerSlice-mean.cps,
			Rating=Rating-mean.rating)
uncenter <- function(df) transform(df, CostPerSlice=CostPerSlice+mean.cps,
			Rating=Rating+mean.rating)

# some quick visualizations
#qplot(za.df$Rating, binwidth=.25)
#qplot(za.df$CostPerSlice, binwidth=.25)

# raw data, plus simple lm fits (Rating ~ CPS only) per neighborhood
ggplot(uncenter(za.df), aes(CostPerSlice, Rating, color=HeatSource)) + geom_jitter() +
	facet_wrap(~ Neighborhood) + 
	geom_smooth(aes(color=NULL), color='black', method='lm', se=FALSE, size=2)

# First we're going to fit a couple of versions of a standard linear model,
# intially main effects only, then with the two biggest predictors interacting.

# full (complete) pooling
lm.full.main <- glm(Rating ~ CostPerSlice + HeatSource, data=za.df)
display(lm.full.main, detail=TRUE) # from arm package
lm.full.int <- glm(Rating ~ CostPerSlice * HeatSource, data=za.df)
display(lm.full.int, detail=TRUE)

AIC(lm.full.main, lm.full.int) 
# interaction isn't provably better, but it might predict a bit better

# CV the simpler model, all data, then Chinatown only
set.seed(11102)
cvsub.glm(za.df, lm.full.main, K=10)$delta
set.seed(11102)
cvsub.glm(za.df, lm.full.main, K=10, cv.subset=inchinatown)$delta
# CV the interaction model, all data, then Chinatown only
set.seed(11102)
cvsub.glm(za.df, lm.full.int, K=10)$delta # overfitting
set.seed(11102)
cvsub.glm(za.df, lm.full.int, K=10, cv.subset=inchinatown)$delta # overfitting
# interaction model a bit better!

# add the ML predictions of the full-pooling better to the df for viz later
za.df$lm.full.pred <- predict(lm.full.int)

# Next: a no-pooling model, treating the levels of Neighborhood as
# factors.
lm.no <- glm(Rating ~ CostPerSlice + HeatSource + Neighborhood, 
		data=za.df)
#		contrasts=list(Neighborhood="contr.sum")) 
# contr.sum forces 

set.seed(11102)
cvsub.glm(za.df, lm.no, K=10)$delta
set.seed(11102)
cvsub.glm(za.df, lm.no, K=10, cv.subset=inchinatown)$delta #[[2]]

summary(lm.no)


lm.no.int <- glm(Rating ~ HeatSource + CostPerSlice * Neighborhood, 
		data=za.df,
		contrasts=list(Neighborhood="contr.sum"))

set.seed(11102)
cvsub.glm(za.df, lm.no.int, K=10)$delta
set.seed(11102)
cvsub.glm(za.df, lm.no.int, K=10, cv.subset=inchinatown)$delta #[[2]]

summary(lm.no.int)

za.df$lm.no.pred <- predict(lm.no.int)

# partial pooling, intercepts/neighborhood
lm.me.int <- lme(Rating ~ CostPerSlice + HeatSource, data=za.df,
		random = ~ 1 | Neighborhood)
AIC(lm.me.int)
lm.me.cost <- lme(Rating ~ 1 + CostPerSlice + HeatSource, data=za.df,
		random = ~ 1 + CostPerSlice | Neighborhood, 
		control=list(returnObject=TRUE, msMaxEval=1000)) #, opt="optim"
AIC(lm.full.main, lm.no, lm.no.int, lm.me.int, lm.me.cost)

za.df$lme.pred <- predict(lm.me.cost)

set.seed(11102)
cvsub.lme(za.df, lm.me.cost, K=10)$delta
set.seed(11102)
cvsub.lme(za.df, lm.me.cost, K=10, cv.subset = inchinatown)$delta #[[2]]

# lmer is great, but it's difficult to predict from it!
lm.me.cost2 <- lmer(Rating ~ HeatSource +CostPerSlice + (1+CostPerSlice | Neighborhood), 
	data=za.df)
display(lm.me.cost2, detail=TRUE)
#AIC(lm.me.cost2)

#ranef(lm.me.cost2)

#coef(lm.no.int)



# visualize partial pooling
base.plot <- ggplot(za.df, aes(CostPerSlice, Rating, color=HeatSource)) + geom_point() +
	facet_wrap(~ Neighborhood) + 
	geom_smooth(aes(color=NULL), color='darkgrey', method='lm', se=FALSE, size=1.5)

base.plot +
	geom_smooth(aes(y=lm.full.pred), method='lm', se=FALSE, size=1.5) +
	opts(title='Full Pooling')
	
base.plot +
	geom_smooth(aes(y=lm.no.pred), method='lm', se=FALSE, size=1.5) +
	opts(title='No Pooling')

base.plot +
	geom_smooth(aes(y=lme.pred), method='lm', se=FALSE, size=1.5) +
	opts(title='Partial Pooling (Hierarchical)')


# using sim() to simulate full distribution of predictions

num.sims <- 10000
lfi.sim <- sim(lm.full.int, num.sims)
new.za.cps <- 4.20

# have to manually construct the predictor matrix, which is a bit
# tedious. predict() lets you avoid that, but at the cost of less
# flexibility.
colnames(lfi.sim$coef)
X.new <- cbind(1, new.za.cps-mean.cps, 0, 1, 0, new.za.cps-mean.cps)

y.lfi.new <- array(NA, c(num.sims, nrow(X.new)))
for (s in 1:num.sims) {
  y.lfi.new[s, ] <- rnorm(nrow(X.new), 
			    X.new %*% lfi.sim$coef[s, ], 
			    lfi.sim$sigma[s])
}
y.est <- data.frame(rating=y.lfi.new + mean.rating, type='FullPooling')
ggplot(y.est, aes(rating, fill=type,color=type)) + geom_histogram(alpha=.5)

# do the same with no-pooling (neighborhood as factor)
lni.sim <- sim(lm.no.int, num.sims)
colnames(lni.sim$coef)
X.new <- cbind(1, 0, 1, new.za.cps-mean.cps, 1, 0, 0, 0, new.za.cps-mean.cps, 0, 0, 0)
y.lni.new <- array(NA, c(num.sims, nrow(X.new)))
for (s in 1:num.sims) {
  y.lni.new[s, ] <- rnorm(nrow(X.new), 
			    X.new %*% lni.sim$coef[s, ], 
			    lni.sim$sigma[s])
}
y.est <- rbind(y.est,
	data.frame(rating=y.lni.new + mean.rating, type='NoPooling'))

ggplot(y.est, aes(rating, fill=type,color=type)) + 
	geom_density(alpha=.5, position='identity',size=1) +
	geom_vline(xintercept=mean.rating)

# and again with partial pooling

# first existing neighborhood (same as before)
lmc.sim <- sim(lm.me.cost2, num.sims)
colnames(lmc.sim$fixef)
dimnames(lmc.sim$Neighborhood)
X.new.fixed <- cbind(1, 0, 1, new.za.cps-mean.cps)
X.new.rand <- cbind(1, new.za.cps-mean.cps)
y.lmc.new <- array(NA, c(num.sims, nrow(X.new.fixed)))
for (s in 1:num.sims) {
  y.lmc.new[s, ] <- rnorm(nrow(X.new), 
			    X.new.fixed %*% lmc.sim$fixef[s, ] +
			    X.new.rand %*% lmc.sim$Neighborhood[s , 'Chinatown', ], 
			    lmc.sim$sigma[s])
}
y.est <- rbind(y.est,
	data.frame(rating=y.lmc.new + mean.rating, type='PartialPooling'))

ggplot(y.est, aes(rating, fill=type,color=type)) + 
	geom_density(alpha=.5, position='identity',size=1) +
	geom_vline(xintercept=mean.rating)

# and now a new neighborhood
y.lmc.new2 <- array(NA, c(num.sims, nrow(X.new.fixed)))
for (s in 1:num.sims) {
  ranef.sample <- mvrnorm(nrow(X.new), c(0,0),  VarCorr(lm.me.cost2)$Neighborhood)
  y.lmc.new2[s, ] <- rnorm(nrow(X.new), 
			    X.new.fixed %*% lmc.sim$fixef[s, ] +
			    X.new.rand %*% ranef.sample, 
			    lmc.sim$sigma[s])
}
y.est <- rbind(y.est,
	data.frame(rating=y.lmc.new2 + mean.rating, type='PartialPooling(New)'))

ggplot(y.est, aes(rating, fill=type,color=type)) + 
	geom_density(alpha=.5, position='identity',size=1) +
	geom_vline(xintercept=mean.rating)

