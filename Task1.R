#HW1 - Task 1
install.packages('data.table')
require(data.table)
install.packages('lubridate')
require(lubridate)

install.packages('car')
library('car')
#read matches info
setwd("C:/Users/Mustafa Oguz Turkkan/Documents/GitHub/etm01-Delicioussaw")
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')
odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')

#Filter odds for over&under bet type and a total handicap of 2.5
ou_odds_all=odds[betType=='ou'& totalhandicap=='2.5']

#Change all the time values to meaningful values
ou_odds_all[,timestamp:=as_datetime(date,tz='Turkey')]

#Order all the odds
ou_odds_all=ou_odds_all[order(matchId,oddtype,timestamp)]

#Get the latest odds
latest_ou_odds=ou_odds_all[,list(last_odd=odd[.N]),list(matchId,oddtype,bookmaker)]

#Filter odds for 5 different bookmakers
latest_ou_odds_bet365=latest_ou_odds[bookmaker=='bet365']
latest_ou_odds_1xBet=latest_ou_odds[bookmaker=='1xBet']
latest_ou_odds_Betclic=latest_ou_odds[bookmaker=='Betclic']
latest_ou_odds_BetVictor=latest_ou_odds[bookmaker=='BetVictor']
latest_ou_odds_Betway=latest_ou_odds[bookmaker=='Betway']

#We adjust the matches data too for showing the total number of goals
matches[,c('score_home','score_away'):=tstrsplit(score,':')]
matches[,score_home:=as.numeric(score_home)]
matches[,score_away:=as.numeric(score_away)]
matches$sum_score=rowSums(cbind(matches$score_home,matches$score_away))

#Normalize data. Change the odd values to probabilities. We are taking
#the last odd into account here.
#### bet365 ####
wide_ou_odds=dcast(latest_ou_odds_bet365,matchId~oddtype,value.var='last_odd')
wide_ou_odds$result=rowSums(cbind(wide_ou_odds$over,wide_ou_odds$under))
wide_ou_odds$over_prob=wide_ou_odds$over/wide_ou_odds$result
wide_ou_odds$under_prob=wide_ou_odds$under/wide_ou_odds$result

ou_odds_results=merge(wide_ou_odds,matches[,list(matchId,sum_score)],by=c('matchId'),all.x=TRUE)
ou_odds_results[,score:=ifelse(sum_score>2.5,'over','under')]

ou_bucket=c(5:15)/15

ou_odds_results[,discrete_over:=cut(over_prob,ou_bucket)]
summary_ou_buckets_over=ou_odds_results[,list(overwins=sum(score=='over',na.rm=TRUE),.N),by=list(discrete_over)]#gerçekte over ile bitenler toplanir, value araliklarina göre pivot alinir

summary_ou_buckets_over[,ratio_over:=overwins/N]

summary_ou_buckets_over=summary_ou_buckets_over[order(discrete_over)]
summary_over=ou_odds_results[,list(avg_over_prob=mean(over_prob,na.rm=TRUE),.N),by=list(discrete_over)]

scatterplot(summary_over$avg_over_prob,summary_ou_buckets_over$ratio_over,
            xlab="Average probability of overs in a bin", ylab="Actual overs ratio in a bin",
            main="Bet365")
abline(coef=c(0,1))

#### 1xBet ####
wide_ou_odds=dcast(latest_ou_odds_1xBet,matchId~oddtype,value.var='last_odd')
wide_ou_odds$result=rowSums(cbind(wide_ou_odds$over,wide_ou_odds$under))
wide_ou_odds$over_prob=wide_ou_odds$over/wide_ou_odds$result
wide_ou_odds$under_prob=wide_ou_odds$under/wide_ou_odds$result

ou_odds_results=merge(wide_ou_odds,matches[,list(matchId,sum_score)],by=c('matchId'),all.x=TRUE)
ou_odds_results[,score:=ifelse(sum_score>2.5,'over','under')]

ou_odds_results[,discrete_over:=cut(over_prob,ou_bucket)]
summary_ou_buckets_over=ou_odds_results[,list(overwins=sum(score=='over',na.rm=TRUE),.N),by=list(discrete_over)]#gerçekte over ile bitenler toplanir, value araliklarina göre pivot alinir

summary_ou_buckets_over[,ratio_over:=overwins/N]

summary_ou_buckets_over=summary_ou_buckets_over[order(discrete_over)]
summary_over=ou_odds_results[,list(avg_over_prob=mean(over_prob,na.rm=TRUE),.N),by=list(discrete_over)]

scatterplot(summary_over$avg_over_prob,summary_ou_buckets_over$ratio_over,
            xlab="Average probability of overs in a bin", ylab="Actual overs ratio in a bin",
            main="1xBet")
abline(coef=c(0,1))

### Betclic ####
wide_ou_odds=dcast(latest_ou_odds_Betclic,matchId~oddtype,value.var='last_odd')
wide_ou_odds$result=rowSums(cbind(wide_ou_odds$over,wide_ou_odds$under))
wide_ou_odds$over_prob=wide_ou_odds$over/wide_ou_odds$result
wide_ou_odds$under_prob=wide_ou_odds$under/wide_ou_odds$result

ou_odds_results=merge(wide_ou_odds,matches[,list(matchId,sum_score)],by=c('matchId'),all.x=TRUE)
ou_odds_results[,score:=ifelse(sum_score>2.5,'over','under')]

ou_odds_results[,discrete_over:=cut(over_prob,ou_bucket)]
summary_ou_buckets_over=ou_odds_results[,list(overwins=sum(score=='over',na.rm=TRUE),.N),by=list(discrete_over)]#gerçekte over ile bitenler toplanir, value araliklarina göre pivot alinir

summary_ou_buckets_over[,ratio_over:=overwins/N]

summary_ou_buckets_over=summary_ou_buckets_over[order(discrete_over)]
summary_over=ou_odds_results[,list(avg_over_prob=mean(over_prob,na.rm=TRUE),.N),by=list(discrete_over)]

scatterplot(summary_over$avg_over_prob,summary_ou_buckets_over$ratio_over,
            xlab="Average probability of overs in a bin", ylab="Actual overs ratio in a bin",
            main="Betclic")
abline(coef=c(0,1))

### BetVictor ####
wide_ou_odds=dcast(latest_ou_odds_BetVictor,matchId~oddtype,value.var='last_odd')
wide_ou_odds$result=rowSums(cbind(wide_ou_odds$over,wide_ou_odds$under))
wide_ou_odds$over_prob=wide_ou_odds$over/wide_ou_odds$result
wide_ou_odds$under_prob=wide_ou_odds$under/wide_ou_odds$result

ou_odds_results=merge(wide_ou_odds,matches[,list(matchId,sum_score)],by=c('matchId'),all.x=TRUE)
ou_odds_results[,score:=ifelse(sum_score>2.5,'over','under')]

ou_odds_results[,discrete_over:=cut(over_prob,ou_bucket)]
summary_ou_buckets_over=ou_odds_results[,list(overwins=sum(score=='over',na.rm=TRUE),.N),by=list(discrete_over)]#gerçekte over ile bitenler toplanir, value araliklarina göre pivot alinir

summary_ou_buckets_over[,ratio_over:=overwins/N]

summary_ou_buckets_over=summary_ou_buckets_over[order(discrete_over)]
summary_over=ou_odds_results[,list(avg_over_prob=mean(over_prob,na.rm=TRUE),.N),by=list(discrete_over)]

scatterplot(summary_over$avg_over_prob,summary_ou_buckets_over$ratio_over,
            xlab="Average probability of overs in a bin", ylab="Actual overs ratio in a bin",
            main="BetVictor")
abline(coef=c(0,1))

### Betway ####
wide_ou_odds=dcast(latest_ou_odds_Betway,matchId~oddtype,value.var='last_odd')
wide_ou_odds$result=rowSums(cbind(wide_ou_odds$over,wide_ou_odds$under))
wide_ou_odds$over_prob=wide_ou_odds$over/wide_ou_odds$result
wide_ou_odds$under_prob=wide_ou_odds$under/wide_ou_odds$result

ou_odds_results=merge(wide_ou_odds,matches[,list(matchId,sum_score)],by=c('matchId'),all.x=TRUE)
ou_odds_results[,score:=ifelse(sum_score>2.5,'over','under')]

ou_odds_results[,discrete_over:=cut(over_prob,ou_bucket)]
summary_ou_buckets_over=ou_odds_results[,list(overwins=sum(score=='over',na.rm=TRUE),.N),by=list(discrete_over)]#gerçekte over ile bitenler toplanir, value araliklarina göre pivot alinir

summary_ou_buckets_over[,ratio_over:=overwins/N]

summary_ou_buckets_over=summary_ou_buckets_over[order(discrete_over)]
summary_over=ou_odds_results[,list(avg_over_prob=mean(over_prob,na.rm=TRUE),.N),by=list(discrete_over)]

scatterplot(summary_over$avg_over_prob,summary_ou_buckets_over$ratio_over,
            xlab="Average probability of overs in a bin", ylab="Actual overs ratio in a bin",
            main="Betway")
abline(coef=c(0,1))

#1-b - Selected bookmaker: Betway
annual=merge(wide_ou_odds,matches[,list(matchId,sum_score,date)],by=c('matchId'),all.x=TRUE)
annual[,score:=ifelse(sum_score>2.5,'over','under')]
annual[,timestamp:=as_datetime(date,tz='Turkey')]
annual[,discrete_over:=cut(over_prob,ou_bucket)]

annual=annual[sum_score<2.5,over_under:=0]
annual=annual[sum_score>2.5,over_under:=1]
annual_pivot=annual[,list(odd_avg=mean(over_prob,na.rm=T),real_avg=mean(over_under,na.rm = T),count=.N),by=list(year(timestamp),discrete_over)]
annual_pivot=annual_pivot[order(year)]
annual_pivot2=annual_pivot[discrete_over=='(0.4,0.467]']
x=annual_pivot2$year
y1=annual_pivot2$odd_avg
y2=annual_pivot2$real_avg

plot(x,y1, type="l", col="green", lwd=5, xlab="Years", ylab="Odd-Actual Averages", xlim=c(2015,2019), ylim=c(0.42,0.7))
lines(x, y2, col="red", lwd=2)
title("Betway Odds Over the Years")
legend(2,1,c("Odds","Actual"), lwd=c(5,2), col=c("green","red"))
