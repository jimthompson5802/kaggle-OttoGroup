###
#  high-level data summary
###

library(caret)
library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
library(caret)

data.dir <- "./data"

load(file=paste0(data.dir,"/train_calib_test.RData"))

# strip off ID field
eda.df <- data.frame(target=train.raw$target,train.raw[,2:(ncol(train.raw)-1)])

#augment features with number of features with non-zero counts and total counts
nz.count <- apply(eda.df[,2:ncol(eda.df)],1,function(row){sum(row>0)})
count.total <- apply(eda.df[,2:ncol(eda.df)],1,sum)
eda.df <- data.frame(eda.df,nz.count,count.total)

# calculate mean value of scaled features by Class
ll<-dlply(eda.df,.(target),function(df){apply(df[,2:(ncol(df))],2,mean)})
df <- t(do.call(rbind,ll))

# scale each feature based on range
pp <- preProcess(eda.df[,2:ncol(eda.df)],method="range")

# replace orginal features with scaled versions
eda.df[,2:ncol(eda.df)] <- predict(pp,eda.df[,2:ncol(eda.df)])

# calculate mean value of scaled features by Class
ll<-dlply(eda.df,.(target),function(df){apply(df[,2:(ncol(df))],2,mean)})
df2 <- t(do.call(rbind,ll))

data_long <- melt(df,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("target"),
                  # The source columns
                  measure.vars=setdiff(names(df),c("target")),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="feature",
                  value.name="mean.value"
)

names(data_long)[1:2] <- c("feature","target")

data_long_scale <- melt(df2,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("target"),
                  # The source columns
                  measure.vars=setdiff(names(df),c("target")),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="feature",
                  value.name="mean.value"
)

names(data_long_scale)[1:2] <- c("feature","target")

ggplot(data_long,aes(x=target, y=feature)) +
    geom_tile(aes(fill=mean.value), color="black") +
    ylab("Features") +
    xlab("Classes") +
    scale_fill_gradient(low = "green", high = "red") +
    ggtitle("Feature Mean Values") +
    theme(axis.title.x=element_blank(),
          axis.ticks=element_blank())

ggplot(data_long_scale,aes(x=target, y=feature)) +
    geom_tile(aes(fill=mean.value), color="black") +
    ylab("Features") +
    xlab("Classes") +
    scale_fill_gradient(low = "green", high = "red") +
    ggtitle("Mean Value of Scaled Features") +
    theme(axis.title.x=element_blank(),
          axis.ticks=element_blank())
