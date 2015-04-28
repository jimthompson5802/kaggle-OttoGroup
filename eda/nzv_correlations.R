###
# initial status
###

library(caret)
library(corrplot)
library(tidyr)

data.dir <- "./data"

load(file=paste0(data.dir,"/train_calib_test.RData"))

predictors <- train.raw[setdiff(names(train.raw),c("id","target"))]

nz <- nearZeroVar(predictors, saveMetrics= TRUE)

nzidx <- nearZeroVar(predictors)

nz.vars <- names(predictors)[nzidx]

save(nz.vars,file=paste0(data.dir,"/near_zero_vars.RData"))

correlations <- cor(predictors)

png("./eda/corrplot.png",height=17, width=11, units="in", res=72)
corrplot(correlations, order="hclust")
dev.off()

coridx <- findCorrelation(correlations,cutoff=0.8)

corr.vars <- names(predictors)[coridx]

save(corr.vars, file=paste0(data.dir,"/correlated_vars.RData"))

#flatten the correlation matrrix and extract features with high correlation
df <- data.frame(feature.x=row.names(correlations),correlations,stringsAsFactors=FALSE)

df <- gather(df,feature.y,correlation,contains("feat_"))

subset(df,abs(correlation) >= 0.7 & abs(correlation) < 1.0)


