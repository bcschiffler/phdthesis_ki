
# Plotting script for testscores obtained in study 1 separately by symbol combination
# I convert the output PDF manually to png to include in the Markdown document

# Formulas from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

rm(list = ls())

library(R.matlab)
library(plyr)
library(reshape2)
library(lme4)
library(showtext)
library(extrafont)
# load fonts for extrafonts
extrafont::loadfonts(device="win")

library(ggplot2)
library(ggthemes)
font.add("Garamond",
         regular = "C:/Windows/Fonts/GARA.ttf",
         italic = "C:/Windows/Fonts/GARAIT.ttf",
         symbol = "C:/Windows/Fonts/GARABD.ttf")



# Control some plotting variables
axistextsize = 20
axistitlesize = 30 # titles on y- and x-axis
overalltextsize = 25 # e.g. legend
yaxistitlevjust = 0.3 # how far should the y-axis title be away from the y-axis?
xaxistitlevjust = -0.25 # same for x-axis


## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
        library(plyr)
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
                if (na.rm) sum(!is.na(x))
                else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                               c(N    = length2(xx[[col]], na.rm=na.rm),
                                 mean = mean   (xx[[col]], na.rm=na.rm),
                                 sd   = sd     (xx[[col]], na.rm=na.rm)
                               )
                       },
                       measurevar
        )
        
        # Rename the "mean" column    
        datac <- rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
        library(plyr)
        
        # Measure var on left, idvar + between vars on right of formula.
        data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                               .fun = function(xx, col, na.rm) {
                                       c(subjMean = mean(xx[,col], na.rm=na.rm))
                               },
                               measurevar,
                               na.rm
        )
        
        # Put the subject means with original data
        data <- merge(data, data.subjMean)
        
        # Get the normalized data in a new column
        measureNormedVar <- paste(measurevar, "_norm", sep="")
        data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
                mean(data[,measurevar], na.rm=na.rm)
        
        # Remove this subject mean column
        data$subjMean <- NULL
        
        return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
        
        # Ensure that the betweenvars and withinvars are factors
        factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                             FUN=is.factor, FUN.VALUE=logical(1))
        
        if (!all(factorvars)) {
                nonfactorvars <- names(factorvars)[!factorvars]
                message("Automatically converting the following non-factors to factors: ",
                        paste(nonfactorvars, collapse = ", "))
                data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
        }
        
        # Get the means from the un-normed data
        datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                           na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
        
        # Drop all the unused columns (these will be calculated with normed data)
        datac$sd <- NULL
        datac$se <- NULL
        datac$ci <- NULL
        
        # Norm each subject's data
        ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
        
        # This is the name of the new column
        measurevar_n <- paste(measurevar, "_norm", sep="")
        
        # Collapse the normed data - now we can treat between and within vars the same
        ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                            na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
        
        # Apply correction from Morey (2008) to the standard error and confidence interval
        #  Get the product of the number of conditions of within-S variables
        nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                        FUN.VALUE=numeric(1)))
        correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
        
        # Apply the correction factor
        ndatac$sd <- ndatac$sd * correctionFactor
        ndatac$se <- ndatac$se * correctionFactor
        ndatac$ci <- ndatac$ci * correctionFactor
        
        # Combine the un-normed means with the normed results
        merge(datac, ndatac)
}

#############################
# Plot number of corrects in testing phase depending on symbol pair
# (not in manuscript)
##########################

# Load in processed testdata
testdata <- readMat('../data/testingscores.mat')

# Load in study dataframe
qws <- read.table("../data/primes_testing_after_exclusion.csv", header=TRUE, 
                  sep=",")

df_test <- data.frame(AC=as.vector(testdata$AC),AD=as.vector(testdata$AD),AE=as.vector(testdata$AE),AF=as.vector(testdata$AF),
                      BC=as.vector(testdata$BC),BD=as.vector(testdata$BD),BE=as.vector(testdata$BE),BF=as.vector(testdata$BF),
                      subject=as.factor(1:37), prime=as.factor(as.numeric(qws$prime) - 1))

df_test_long <- melt(df_test, id.vars=c("subject", "prime"),variable.name="testpair")

# stats on difficulty of testpairs
df_test_long$difficulty <- NaN
df_test_long$difficulty[df_test_long$testpair=="AC" | df_test_long$testpair=="BD"] <- 10
df_test_long$difficulty[df_test_long$testpair=="AE" | df_test_long$testpair=="BF"] <- 20
df_test_long$difficulty[df_test_long$testpair=="AF" | df_test_long$testpair=="BE"] <- 40
df_test_long$difficulty[df_test_long$testpair=="AD" | df_test_long$testpair=="BC"] <- 50

df_test_long$binarydifficulty[df_test_long$testpair=="AC" | df_test_long$testpair=="BD" | df_test_long$testpair=="AE" | df_test_long$testpair=="BF"] <- 1
df_test_long$binarydifficulty[df_test_long$testpair=="AF" | df_test_long$testpair=="BE" | df_test_long$testpair=="AD" | df_test_long$testpair=="BC"] <- 2

mod_difficulty_test <- lmer(value ~ difficulty + prime + (1 | subject), data=df_test_long, REML=TRUE)
summary(mod_difficulty_test)

mod_diff_binary <- lmer(value ~ binarydifficulty + prime + (1 | subject), data=df_test_long, REML=TRUE)
summary(mod_diff_binary)

# Get summary of data
test_summarized_within <- summarySEwithin(df_test_long,measurevar="value",withinvars="testpair",idvar="subject")
# test_summarized <- summarySE(df_test_long,measurevar="value",groupvars="testpair")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(test_summarized_within, aes(x=testpair, y=value, fill=testpair)) + 
        geom_bar(position=position_dodge(), stat="identity",
                 colour="black",
                 size=.4) +
        geom_errorbar(aes(ymin=value-se, ymax=value+se),
                      size=.4,
                      width=.2,
                      position=position_dodge(.9)) +
        xlab("Pair") +
        ylab("Correct answers") +
        # scale_fill_manual(values=c("#CC6666", "#66CC99", "#CC6666", "#66CC99", "#66CC99", "#CC6666", "#66CC99", "#CC6666")) +
        scale_fill_manual(values=c("#FF9900", "#66CC99", "#FF9900", "#66CC99", "#66CC99", "#FF9900", "#66CC99", "#FF9900")) +
        # ggtitle("Average number of corrects in testing") +
        # scale_y_continuous(limits=c(1,4)) +
        coord_cartesian(ylim=c(0,4)) +
        theme_bw() +
        theme(text=element_text(family="Garamond", size=16)) +
        theme(axis.title.y=element_text(vjust=0.3)) +
        guides(fill=FALSE)

ggsave(file="../figures/raw/test_scores_by_pair_se.pdf", dpi=600, device=cairo_pdf)
ggsave(file="../figures/raw/test_scores_by_pair_se.svg", dpi=600, device=cairo_pdf)

dev.off()
