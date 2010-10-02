# Pizza data analysis with mixed-effects modeling

library(stringr)
library(ggplot2)
library(nlme)
library(lme4)
library(boot)

za.df <- read.csv("Fake Pizza Data.csv")

# some basic summaries and visualizations
summary(za.df)

# fix cost column
za.df$CostPerSlice <- as.numeric(str_sub(za.df$CostPerSlice, 2))

# get the contrasts right for HeatSource
contrasts(za.df$HeatSource) <- contr.treatment(levels(za.df$HeatSource), 
	base=which(levels(za.df$HeatSource) == 'Gas'))

qplot(za.df$Rating)
qplot(za.df$CostPerSlice, binwidth=.25)

ggplot(za.df, aes(CostPerSlice, Rating, color=HeatSource)) + geom_point() +
	facet_wrap(~ Neighborhood) + 
	geom_smooth(aes(color=NULL), color='black', method='lm', se=FALSE, size=2)

# basics with linear model


# full pooling
lm.full.main <- glm(Rating ~ CostPerSlice + HeatSource + BrickOven, data=za.df)
lm.full.int <- glm(Rating ~ CostPerSlice * HeatSource + BrickOven + CostPerSlice:BrickOven, data=za.df)
AIC(lm.full.main, lm.full.int) 
summary(lm.full.main)
set.seed(11102)
cv.glm(za.df, lm.full.main, K=10)$delta[[2]]
set.seed(11102)
cv.glm(za.df, lm.full.int, K=10)$delta[[2]] # overfitting

za.df$lm.full.pred <- predict(lm.full.main)

# no pooling
lm.no <- glm(Rating ~ CostPerSlice + HeatSource + BrickOven + Neighborhood, 
		data=za.df,
		contrasts=list(Neighborhood="contr.sum"))

set.seed(11102)
cv.glm(za.df, lm.no, K=10)$delta[[2]]

summary(lm.no)


lm.no.int <- glm(Rating ~ HeatSource + BrickOven + CostPerSlice * Neighborhood, 
		data=za.df,
		contrasts=list(Neighborhood="contr.sum"))

set.seed(11102)
cv.glm(za.df, lm.no.int, K=10)$delta[[2]]

summary(lm.no.int)

za.df$lm.no.pred <- predict(lm.no.int)

# partial pooling, intercepts/neighborhood
lm.me.int <- lme(Rating ~ CostPerSlice + HeatSource + BrickOven, data=za.df,
		random = ~ 1 | Neighborhood)
AIC(lm.me.int)
lm.me.cost <- lme(Rating ~ 1+ HeatSource + BrickOven, data=za.df,
		random = ~ 1 + CostPerSlice | Neighborhood, 
		control=list(returnObject=TRUE, opt="optim"))
AIC(lm.full.main, lm.no, lm.no.int, lm.me.int, lm.me.cost)

za.df$lme.pred <- predict(lm.me.cost)

set.seed(11102)
cv.lme(za.df, lm.me.cost, K=10)$delta[[2]]

# lmer is great, but it's difficult to predict from it!
#lm.me.cost2 <- lmer(Rating ~ HeatSource + BrickOven + (1+CostPerSlice | Neighborhood), 
#	data=za.df)
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


