###
#  create VarImp plots
###

library(caret)
library(plyr)
library(randomForest)
library(gbm)
library(ggplot2)
library(grid)



# VarImp for rf2 model
load("./src/rf2_model/model_rf_all_data_ntree_4000.RData")
df<- varImp(mdl.fit)

# normalize importance to 100
df$var.imp <- 100*df$Overall/max(df$Overall)

df <- head(df[order(-df$var.imp),],20)
df$feature <- row.names(df)

p1 <- ggplot(df) +
    geom_bar(aes(x=reorder(feature,var.imp),y=var.imp), fill="blue", stat="identity") +
    coord_flip() +
    ylab("Relative Variable Importance") +
    xlab("Feature") +
    theme(axis.text=element_text(size=15))
png(filename="./presentations/varImp_rf2.png", width=11, height=8.5, units="in", res=600)
print(p1)
dev.off()


# function to extract from one vs all model the sub-model variable importance
extractVarImp <- function(cls,mdls) {
    df <- varImp(mdls[[cls]])$importance
    df$feature <- row.names(df)
    df$class <- cls
    df <- df[order(-df$Overall),]
    df <- head(df,20)
    px <- ggplot(df) + 
        geom_bar(aes(x=reorder(feature,Overall), y=Overall), stat="identity", fill="blue") +
        coord_flip() +
        ylab("Relative Variable Importance") +
        xlab("Feature") +
        ggtitle(paste(cls)) +
        theme(axis.text=element_text(size=15))
    px
}

# gbm2 varImp
load("./src/gbm2_model/model_gbm_one_vs_all_2015-05-08_22_59_43.RData")
ll <- lapply(paste0("Class_",1:9), extractVarImp,gbm.mdls)

png(filename="./presentations/varImp_gbm2.png", width=17*1.2, height=11*1.2, units="in", res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,5)))
print(ll[[1]], vp=viewport(layout.pos.row=1, layout.pos.col = 1))
print(ll[[2]], vp=viewport(layout.pos.row=1, layout.pos.col = 2))
print(ll[[3]], vp=viewport(layout.pos.row=1, layout.pos.col = 3))
print(ll[[4]], vp=viewport(layout.pos.row=1, layout.pos.col = 4))
print(ll[[5]], vp=viewport(layout.pos.row=1, layout.pos.col = 5))
print(ll[[6]], vp=viewport(layout.pos.row=2, layout.pos.col = 1))
print(ll[[7]], vp=viewport(layout.pos.row=2, layout.pos.col = 2))
print(ll[[8]], vp=viewport(layout.pos.row=2, layout.pos.col = 3))
print(ll[[9]], vp=viewport(layout.pos.row=2, layout.pos.col = 4))
dev.off()

# gbm4 varImp
load("./src/gbm4_model/model_gbm_one_vs_all_2015-05-14_12_00_38.RData")
ll <- lapply(paste0("Class_",1:9), extractVarImp,gbm.mdls)

png(filename="./presentations/varImp_gbm4.png", width=17*1.2, height=11*1.2, units="in", res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,5)))
print(ll[[1]], vp=viewport(layout.pos.row=1, layout.pos.col = 1))
print(ll[[2]], vp=viewport(layout.pos.row=1, layout.pos.col = 2))
print(ll[[3]], vp=viewport(layout.pos.row=1, layout.pos.col = 3))
print(ll[[4]], vp=viewport(layout.pos.row=1, layout.pos.col = 4))
print(ll[[5]], vp=viewport(layout.pos.row=1, layout.pos.col = 5))
print(ll[[6]], vp=viewport(layout.pos.row=2, layout.pos.col = 1))
print(ll[[7]], vp=viewport(layout.pos.row=2, layout.pos.col = 2))
print(ll[[8]], vp=viewport(layout.pos.row=2, layout.pos.col = 3))
print(ll[[9]], vp=viewport(layout.pos.row=2, layout.pos.col = 4))
dev.off()