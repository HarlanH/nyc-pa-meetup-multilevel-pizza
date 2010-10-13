plotCoef <- function(model,Main="",YLab="",XLab="",labelDirec=2,CEX=.8,LWD68=1,LWD95=0,vertColor="grey",vertLWD=1,vertType=2,Color="blue",Intercept=TRUE,Interaction="*",ShortenFactors=TRUE)
## This function plots the coefficients and confidence intervals for a regression (lm or glm)
## It is designed to replicate the coefficient plots found in Drs. Andrew Gelman and Jennifer Jill's "Data Analysis Using Regression and Multilevel/Hierarchical Models" (page 176, Figure 9.5 for example)
## This version requires the library ggplot2
## This version handles the margins much better so that the coefficient names don't get cut off on the left side
## A future version should allow the user to specifiy the coefficient names and order
## A future version might order the coefficients according to their sizes
## Written by Jared P. Lander, www.jaredlander.com
## Latest draft:  10/11/2010
{
	if(!"package:ggplot2" %in% search())
	# check if ggplot2 is loaded
	{
		if(!"ggplot2" %in% installed.packages())
		# see if it is even installed
		# if not, stop the function and ask the user to install it
		{
			cat("Please install ggplot2 before running this function\n")
			stop()
		}
		
		require(ggplot2)	# load the package
		cat("ggplot2 was loaded\n")
	}
	
	modelCoef <- model$coef		# get the coefficients
	
	## this section grabs the standard deviations, it must be done differently depending on the type of model (lm or glm)
	if(model$call[1] == "lm()")
	{
		modelSE <- sqrt(diag(summary(model)$cov))
	}
	else if(model$call[1] == "glm()")
	{
		modelSE <- sqrt(diag(summary(model)$cov.scaled))
	}
	
	modelCI <- data.frame(Low95=modelCoef - 2*modelSE,High95=modelCoef + 2*modelSE,Low68=modelCoef - modelSE,High68=modelCoef + modelSE,Coef=modelCoef) # build a data.frame of the confidence bounds and original coefficients
	modelCI$Name <- rownames(modelCI)	## grab the coefficient names into the data.frame
	
	if(ShortenFactors)
	# Often the variable name is tacked onto each of the levels of a factor which can be unslightly, this section strips off the name
	# this can be a problem when the levels are numeric such as Round1, Round2, Round3
	# a future version should be intelligent enough not to strip those kind of factors
	{
		varNames <- names(model$model)[-1]	# grabs the variable names
		varFrame <- data.frame(varNames)	# starts a data.frame with those names
		varFrame$factor <- ifelse(attr(model$terms,"dataClasses")[-1]=="factor",1,0)	# denote each variable as a factor or not
		modelNames <- names(modelCoef)		# get the coefficient names
		modelNames <- sub("factor\\([a-zA-Z0-9]*\\)","",modelNames)	# strip the word "factor" and the included variable from any factors, only works for the kind like "factor(...)"
		for(i in 1:nrow(varFrame))
		# go through and strip the remaining factors
		{
			if(varFrame[i,2] == 1)
			# if that variable is a factor
			{
				modelNames <- gsub(varFrame[i,1],"",modelNames)	# strip out the variable name
			}
		}
		modelCI$Name <- modelNames	# change the names of the coefficients to these new names
	}
	
	modelCI$Name <- gsub("\\*|\\:x",Interaction,modelCI$Name)
	
	if(Intercept == FALSE)
	{
		modelCI <- modelCI[-which(modelCI$Name=="Intercept"),]
	}
	
	modelMelt <- melt(modelCI,id="Name")	# melt the frame to make it easier to work with ggplot
	modelMelt95 <- modelMelt[modelMelt$variable=="Low95" | modelMelt$variable=="High95",]	# pull out the 95% CI
	modelMelt68 <- modelMelt[modelMelt$variable=="Low68" | modelMelt$variable=="High68",]	# pull out the 68% CI
	qplot(Coef,Name,data=modelCI,main=Main,xlab=XLab,ylab=YLab) + geom_vline(xintercept=0,colour=vertColor,linetype=vertType,lwd=vertLWD) + geom_line(aes(x=value),data=modelMelt95,colour=Color,lwd=LWD95) + geom_line(aes(x=value),data=modelMelt68,colour=Color,lwd=LWD68) + geom_point(colour=Color) # the actual plotting
}




plotCoefBase <- function(model,Main="",YLab="",XLab="",labelDirec=2,CEX=.8,LWD68=3,LWD95=1,Interaction="*",Color="blue",vertType=2,vertColor="grey",ShortenFactors=TRUE)
## This function plots the coefficients and confidence intervals for a regression (lm or glm)
## It is designed to replicate the coefficient plots found in Drs. Andrew Gelman and Jennifer Jill's "Data Analysis Using Regression and Multilevel/Hierarchical Models" (page 176, Figure 9.5 for example)
## This version uses base graphics so that the ggplot2 library is not required)
## A future version should allow the user to specifiy the coefficient names and order
## A future version might order the coefficients according to their sizes
## Written by Jared P. Lander, www.jaredlander.com
## Latest draft:  10/11/2010
{
	modelCoef <- model$coef		# get the coefficients
	numCoef <- length(modelCoef)	# find out how many coefficients there are in the model
	
	## this section grabs the standard deviations, it must be done differently depending on the type of model (lm or glm)
	if(model$call[1] == "lm()")
	{
		modelSE <- sqrt(diag(summary(model)$cov))
	}
	else if(model$call[1] == "glm()")
	{
		modelSE <- sqrt(diag(summary(model)$cov.scaled))
	}
	
	modelCI <- data.frame(Low95=modelCoef - 2*modelSE,High95=modelCoef + 2*modelSE,Low68=modelCoef - modelSE,High68=modelCoef + modelSE)	# build a data.frame of the confidence bounds and original coefficients
	
	plot(modelCoef,1:numCoef,type="n",col=Color,xlim=c(floor(min(modelCI$Low95)),ceiling(max(modelCI$High95))),yaxt="n",ylab=YLab,xlab=XLab,main=Main)	# plot the (blank) graph for the coefficients leaving room for the 95% CIs, with no vertical axis
	abline(v=0,lty=vertType,col=vertColor)	# add in a vertical line at 0 for a sense of scale

	for(i in 1:numCoef)
	# plot the CI lines for each coefficient
	{
		lines(c(modelCI$Low95[i],modelCI$High95[i]),c(i,i),col=Color,lwd=LWD95)
		lines(c(modelCI$Low68[i],modelCI$High68[i]),c(i,i),col=Color,lwd=LWD68)
	}
	
	points(modelCoef,1:numCoef,col=Color,pch=16,cex=CEX)	# put in the coefficient points on top
	
	modelNames <- names(modelCoef)	# get the names of the coefficients
	
	if(ShortenFactors)
	# Often the variable name is tacked onto each of the levels of a factor which can be unslightly, this section strips off the name
	# this can be a problem when the levels are numeric such as Round1, Round2, Round3
	# a future version should be intelligent enough not to strip those kind of factors
	{
		varNames <- names(model$model)[-1]	# grabs the variable names
		varFrame <- data.frame(varNames)	# starts a data.frame with those names
		varFrame$factor <- ifelse(attr(model$terms,"dataClasses")[-1]=="factor",1,0)	# denote each variable as a factor or not
		modelNames <- sub("factor\\([a-zA-Z0-9]*\\)","",modelNames)	# strip the word "factor" and the included variable from any factors, only works for the kind like "factor(...)"
		for(i in 1:nrow(varFrame))
		# go through and strip the remaining factors
		{
			if(varFrame[i,2] == 1)
			# if that variable is a factor
			{
				modelNames <- gsub(varFrame[i,1],"",modelNames)	# strip out the variable name
			}
		}
	}
	
	modelNames <- gsub("\\*|\\:x",Interaction,modelNames)
	
	axis(side=2,labels=modelNames,at=1:numCoef,las=labelDirec,cex.axis=CEX)		# plot the vertical axis with the names of the coefficients
}


