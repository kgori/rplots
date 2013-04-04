library(ggplot2)
library(RColorBrewer)
colours <- brewer.pal(8, "Set1")
colours[6] <- "#11dddd"

capitalise <- function(s) {
	first_letter <- toupper(substring(s, 1, 1))
	rest <- substring(s, 2)
	return (paste(first_letter, rest, sep=""))
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                           c( N    = length2(xx[,col], na.rm=na.rm),
                              mean = mean   (xx[,col], na.rm=na.rm),
                              sd   = sd     (xx[,col], na.rm=na.rm)
                              )
                          },
                    measurevar,
                    na.rm
             )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean"=measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}





# d <- subset(nni, Distance=='geo' & Method!="ward" & Method!="single" & Method!="average" & Method !="complete")
mysubset <- function(data, dataset, process, method, datatype, distance) {
  d <- subset(data, DATASET==dataset & PROCESS==process & METHOD==method & DATATYPE==datatype & DISTANCE==distance & CLUSTERING!="spectral_e7" & CLUSTERING!="spectral_n7")
  return(d)
}



myplot <- function(data, mvar, xlab="", ylab="", title="") {
  dfc <- summarySE(data, measurevar=mvar, groupvars=c("CLUSTERING", "LEVEL"))
  pd <- position_dodge(.1)
  l <- max(data$LEVEL)
  if (l == 50){
    breaks <- c(1,seq(5,l-1,5),l) 
  } else { 
    breaks <- 1:l
  }
  
  method <- dfc[[1]]
  xvec <- dfc[[2]]
  yvec <- dfc[[4]]
  se <- dfc[[5]]

  ymin <- yvec-se
  ymax <- yvec+se
  
  print(xvec)
  p <- ggplot(dfc, aes(x=xvec, y=yvec, colour=method, fill=method, group=method), environment=environment()) + 
    geom_line(position=pd, size=1.2, alpha=0.8) +
    geom_point(position=pd) +
    # geom_ribbon(aes(ymin=ymin, ymax=ymax, colour=NA, fill=method), alpha=0.05) + 
    scale_fill_hue() +
    scale_x_discrete(breaks=breaks) +
    labs(
      title=title,
      x=xlab,
      y=ylab
    ) +
    theme_bw()
  return(p)
}


