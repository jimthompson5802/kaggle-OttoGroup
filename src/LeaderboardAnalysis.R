###
# Analysis of Kaggle Leaderboard
###

library(lubridate)
library(ggplot2)

DATA.DIR <- "./data"
USER.ID <- "JMT5802"
# read kaggle leaderboard 
lb.df <- read.csv(unz(paste0(DATA.DIR,"/publicleaderboarddata.zip"),
                       "otto-group-product-classification-challenge_public_leaderboard.csv"),stringsAsFactors=FALSE)

# convert date/time to numeric representation
lb.df$SubmissionDate <- ymd_hms(lb.df$SubmissionDate)


ggplot() + 
    geom_point(data=lb.df[lb.df$TeamName != USER.ID,], aes(x=SubmissionDate, y=Score)) +
    geom_point(data=lb.df[lb.df$TeamName == USER.ID,],aes(x=SubmissionDate,y=Score,color="red"))
