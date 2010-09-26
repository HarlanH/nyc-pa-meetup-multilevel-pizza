# Pizza data analysis with mixed-effects modeling

library(stringr)
library(ggplot2)
library(nlme)
library(lme4)

za.df <- read.csv("Fake Pizza Data.csv")

# some basic summaries and visualizations
summary(za.df)

# fix cost column
za.df$CostPerSlice <- as.numeric(str_sub(za.df$CostPerSlice, 2))

qplot(za.df$Rating)
qplot(za.df$CostPerSlice)

ggplot(za.df, aes(CostPerSlice, Rating)) + geom_point() +
	facet_wrap(~ Neighborhood) + 
	geom_smooth(method='lm', se=FALSE)