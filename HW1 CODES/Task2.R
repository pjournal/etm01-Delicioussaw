install.packages('car')
library('car')
install.packages('data.table')
require(data.table)
install.packages('lubridate')
require(lubridate)
install.packages('ggplot2')
library(ggplot2)
#Read data, filter for 1x2 bet types and the bookmaker 1xBet
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')
odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')
filtered_odd=odds[betType=='1x2'& bookmaker=='1xBet']

#delete totalhandicap column
filtered_odd[,totalhandicap:=NULL]

#normalize time
filtered_odd[,timestamp:=as_datetime(date,tz='Turkey')]

#filter and order the data based on time; then filter and create new tables
#for 3 different odd types: 1, X and 2
filtered_odd=filtered_odd[order(matchId,oddtype,timestamp)]
filtered_odd1s=filtered_odd[oddtype=='odd1']
filtered_odd2s=filtered_odd[oddtype=='odd2']
filtered_oddXs=filtered_odd[oddtype=='oddX']

#delete oddtype, bookmaker, betType, date columns from all 3 tables since we filtered them already
filtered_odd1s[,c('oddtype','bookmaker','betType','date')]=list(NULL)
filtered_odd2s[,c('oddtype','bookmaker','betType','date')]=list(NULL)
filtered_oddXs[,c('oddtype','bookmaker','betType','date')]=list(NULL)

#find the difference between the first odd and the last odd and plot this data for all the matches
#We do that for all 3 odd types
odds1_initial=filtered_odd1s[,list(start_odd=odd[1]),
                          by=list(matchId)]

odds1_final=filtered_odd1s[,list(final_odd=odd[.N]),
                        by=list(matchId)]

odds1_diff=filtered_odd1s[,list(diff_odd=0),
                          by=list(matchId)]
odds1_diff$diff_odd=odds1_final$final_odd - odds1_initial$start_odd

x <- (1:nrow(odds1_diff))
y <- odds1_diff$diff_odd

scatterplot(y ~ x)

odds2_initial=filtered_odd2s[,list(start_odd=odd[1]),
                             by=list(matchId)]

odds2_final=filtered_odd2s[,list(final_odd=odd[.N]),
                           by=list(matchId)]

odds2_diff=filtered_odd2s[,list(diff_odd=0),
                          by=list(matchId)]
odds2_diff$diff_odd=odds2_final$final_odd - odds2_initial$start_odd

x2 <- (1:nrow(odds2_diff))
y2 <- odds2_diff$diff_odd

scatterplot(y2 ~ x2)

oddsX_initial=filtered_oddXs[,list(start_odd=odd[1]),
                             by=list(matchId)]

oddsX_final=filtered_oddXs[,list(final_odd=odd[.N]),
                           by=list(matchId)]

oddsX_diff=filtered_oddXs[,list(diff_odd=0),
                          by=list(matchId)]
oddsX_diff$diff_odd=oddsX_final$final_odd - oddsX_initial$start_odd

xX <- (1:nrow(oddsX_diff))
yX <- oddsX_diff$diff_odd

scatterplot(yX ~ xX)

install.packages("rmarkdown",repos="http://cran.rstudio.com/")
