###
# Analysis of Kaggle Leaderboard
###

library(lubridate)
library(ggplot2)
library(caTools)
library(plyr)
library(RCurl)

DATA.DIR <- "./data"
USER.ID <- "JMT5802"

# download the leaderboard data
# download.file("https://www.kaggle.com/c/4280/publicleaderboarddata.zip",
#               destfile=paste0(DATA.DIR,"/publicleaderboarddata.zip"),
#               mode="wb",
#               method="curl")
lb.df <- read.csv(unz(paste0(DATA.DIR,"/publicleaderboarddata.zip"),
                       "otto-group-product-classification-challenge_public_leaderboard.csv"),
                  stringsAsFactors=FALSE)

# convert date/time to numeric representation
lb.df$SubmissionDate <- ymd_hms(lb.df$SubmissionDate)

# determine number of teams by date/time
lb.df$number.teams <- sapply(1:nrow(lb.df),function(pos,df){length(unique(df[1:pos,"TeamName"]))},lb.df)
lb.df$leader.score <- sapply(1:nrow(lb.df),function(pos,df){min(df[1:pos,"Score"])},lb.df)
lb.df$report.date <- as.Date(lb.df[,"SubmissionDate"])

# get team standings by day
teamStandings <- function(this.date,lb.df) {
    df <- lb.df[lb.df$report.date <= this.date,]

    df <- ddply(df,.(TeamName),summarize,Score=min(Score))

    df <- df[order(df$Score),]
    team.rank <- 1:nrow(df)
    
    return(cbind(team.rank,df))

}

# user score and ranking for specified user as of specified date
userRanking <- function(this.date,lb.df) {
    standings <- teamStandings(this.date,lb.df)
    
    leader.score <- standings[1,"Score"]
    number.teams <- nrow(standings)
    team.rank <- standings[standings$TeamName == USER.ID,"team.rank"]
    team.score <- standings[standings$TeamName == USER.ID,"Score"]
    
    if (length(team.rank) > 0 ) {
        team.percentile <- 100*(1-team.rank/number.teams)
    } else {
        team.percentile <- NA
        team.rank <- NA
        team.score <- NA
    }
    
    return(data.frame(report.date=this.date,
                leader.score=leader.score, number.teams=number.teams,
                team.rank=team.rank,team.percentile=team.percentile,
                team.score=team.score,stringsAsFactors=FALSE))
}

# get team ranking for specified user by competition day
ll <- lapply(unique(lb.df$report.date),userRanking,lb.df)
ranking.df <- do.call(rbind,ll)


ggplot() + 
    geom_point(data=lb.df[lb.df$TeamName != USER.ID,], aes(x=SubmissionDate, y=Score),color="grey75") +
    geom_point(data=lb.df[lb.df$TeamName == USER.ID,],aes(x=SubmissionDate,y=Score),
                                                          pch=18,color="red",size=3) +
    scale_fill_manual("Team", values=c("red","grey75")) +
    ggtitle("Otto Group Scores") +
    theme()


ggplot(data=ranking.df) +
    geom_line(aes(x=report.date,y=leader.score),color="blue",size=1.25) +
    geom_point(aes(x=report.date, y=team.score), pch=18,color="red",size=3) +
    xlab("Submission Date") +
    ylab("Score") +
    ggtitle("Team Score")
