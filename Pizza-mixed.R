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

qplot(za.df$Rating)
qplot(za.df$CostPerSlice)

ggplot(za.df, aes(CostPerSlice, Rating)) + geom_point() +
	facet_wrap(~ Neighborhood) + 
	geom_smooth(method='lm', se=FALSE, size=2)

# basics with linear model

# get the contrasts right for HeatSource
contrasts(za.df$HeatSource) <- contr.treatment(levels(za.df$HeatSource), 
	base=which(levels(za.df$HeatSource) == 'Gas'))

# full pooling
lm.full.main <- glm(Rating ~ CostPerSlice + HeatSource + BrickOven, data=za.df)
lm.full.int <- glm(Rating ~ CostPerSlice * HeatSource + BrickOven + CostPerSlice:BrickOven, data=za.df)
AIC(lm.full.main, lm.full.int) # not actually better
cv.glm(za.df, lm.full.main, K=10)$delta
cv.glm(za.df, lm.full.int, K=10)$delta # overfitting

# no pooling
lm.no <- glm(Rating ~ CostPerSlice + HeatSource + BrickOven + Neighborhood, 
		data=za.df,
		contrasts=list(Neighborhood="contr.sum"))

cv.glm(za.df, lm.no, K=10)$delta 

lm.no.int <- glm(Rating ~ HeatSource + BrickOven + CostPerSlice * Neighborhood, 
		data=za.df,
		contrasts=list(Neighborhood="contr.sum"))
cv.glm(za.df, lm.no.int, K=10)$delta 

# partial pooling, intercepts/neighborhood
lm.me.int <- lme(Rating ~ CostPerSlice + HeatSource + BrickOven, data=za.df,
		random = ~ 1 | Neighborhood)
AIC(lm.me.int)
lm.me.cost <- lme(Rating ~ 1+ HeatSource + BrickOven, data=za.df,
		random = ~ 0 + CostPerSlice | Neighborhood, 
		control=list(returnObject=TRUE))
AIC(lm.full.main, lm.no, lm.me.int, lm.me.cost)

lm.me.cost2 <- lmer(Rating ~ HeatSource + BrickOven + (1+CostPerSlice | Neighborhood), 
	data=za.df)
AIC(lm.me.cost2)


jlza.df <- read.csv("Pizza Diverse.csv")

contrasts(jlza.df$Fuel) <- contr.treatment(levels(jlza.df$Fuel), 
	base=which(levels(jlza.df$Fuel) == 'Gas'))

ggplot(jlza.df, aes(Price.Level, Rating)) + geom_jitter() +
	facet_wrap(~ Neighborhood) + 
	geom_smooth(method='lm', se=FALSE, size=2)

# full pooling
lm.full.main <- glm(Rating ~ Price.Level + Fuel + PizzaName, data=jlza.df)
# no pooling
lm.no <- glm(Rating ~ Fuel + PizzaName + Price.Level + Price.Level:Neighborhood, 
		data=jlza.df,
		contrasts=list(Neighborhood="contr.sum"))

lm.me.cost2 <- lmer(Rating ~ Fuel + PizzaName + (0 + Price.Level | Neighborhood), 
	data=jlza.df)
summary(lm.me.cost2)


