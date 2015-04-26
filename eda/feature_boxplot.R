###
# generate boxplot by feature
###

library(ggplot2)
library(tidyr)

DATA.DIR <- "./data"

load(paste0(DATA.DIR,"/train_calib_test.RData"))

predictor.vars <- setdiff(names(train.raw), c("id","target"))

# convert to long format 
df <- gather(train.raw, feature, value, feat_1:feat_93)

df$feature.id <- do.call(rbind,strsplit(as.character(df$feature),"_"))[,2]
df$class.id <- do.call(rbind,strsplit(df$target,"_"))[,2]

png("./eda/feature_boxplot.png",height=17, width=11, units="in",res=72)
qplot(class.id,value,data=df,geom="boxplot") + 
    facet_wrap(~feature,ncol=10,scales="free_y") +
    xlab("Class")
dev.off()
