###
#  Add data for the final standings
#  this assumes that code in LeaderboardVisualizations.R has already run
###

final.date <- "2015-05-20"
post.date <- "2015-05-21"
leader.final.score <- 0.3802
team.final.score <- 0.459
team.final.position <- 925
team.post.score <- 0.42
team.post.position <- 600
total.teams <- 3590

team.final.percentile <- 100*(1-(team.final.position-1)/total.teams)
team.post.percentile <- 100*(1-(team.post.position-1)/total.teams)

leader.final.df <- data.frame(report.date=as.Date(final.date),score=leader.final.score)
team.final.df <- data.frame(report.date=as.Date(final.date),score=team.final.score,
                            percentile=team.final.percentile) 
team.post.df <- data.frame(report.date=as.Date(post.date),score=team.post.score,
                           percentile=team.post.percentile)




p3.final <- p3 + geom_point(data=leader.final.df,aes(x=report.date,y=score), 
                    color="dark blue") +
    geom_point(data=team.final.df,aes(x=report.date,y=score),color="dark orange") +
    geom_point(data=team.post.df,aes(x=report.date,y=score),color="red")

print(p3.final)


p4.final <- p4 + geom_bar(data=team.final.df, aes(x=report.date, y=percentile),
                          color="yellow", fill="yellow",stat="identity") +
    geom_bar(data=team.post.df, aes(x=report.date, y=percentile),
             color="red", fill="red",stat="identity") 
print(p4.final)

    
