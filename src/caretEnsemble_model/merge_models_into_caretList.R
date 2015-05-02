###
# merge models into caretList structure
###

library(caretEnsemble)

all.classes.test <- list()

for (cls in PRODUCT.CLASSES) {
    for (i in 1:length(other.mdls)) {
        # add models to respective class
        caret.list[[cls]][[other.mdls[[i]]$model.parm$method]] <- other.mdls[[i]]$models[[cls]]

    }
    all.classes.test[[cls]] <- caretEnsemble(caret.list[[cls]])
}

# accumulate the time for training
Reduce("+",lapply(other.mdls,function(x){x$time.data}))

