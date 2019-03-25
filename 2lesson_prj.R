rm(list=ls())#to clear environment variables
install.packages('car')
library('car')
install.packages('data.table')#pivot gibi hersey var
require(data.table)
getwd()
setwd('C:\\Users\\Mustafa Oguz Turkkan\\Documents\\GitHub\\etm01-Delicioussaw')
#read matches info
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')
str(matches)
#delete column type
matches[,type :=NULL]
matches$leagueId=NULL
#data kontrol check unique home team
unique(matches$home)
#fix data error
matches[home=='manchester-utd',home:='manchester united']
unique(matches$home)
matches[home=='manchester-united',home:='manchester united']
unique(matches$home)
#fix away error
matches[away %in% c('manchester-utd','manchester-united'),away:='manchester united']
unique(matches$away)
#manipulate score to break home score from away score ,text to column
matches[,c('score_home','score_away'):=tstrsplit(score,':')]

#change into numeric
matches[,c('score_home','score_away'):=as.numeric(tstrsplit(score,':'))]
matches[,score_home:=as.numeric(score_home)]
matches[,score_away:=as.numeric(score_away)]


#remove score column
matches[,score:=NULL]
#adding result column
matches[,result:=ifelse(score_home>score_away,'home',ifelse(score_home==score_away,'draw','away'))]
#tarih manipülasyonu epoch to date
install.packages('lubridate')
require(lubridate)
matches[,timestamp:=as_datetime(date,tz='Turkey')]

# read odds data
odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')

#filter 1x2 type odd,bookmaker pinnacle
filtered_odd=odds[betType=='1x2'& bookmaker=='Pinnacle']
filtered_odd[,totalhandicap:=NULL]
filtered_odd[,timestamp:=as_datetime(date,tz='Turkey')]
filtered_odd=filtered_odd[order(matchId,oddtype,timestamp)]
latest_odd=filtered_odd[,list(close_odd=odd[.N]),list(matchId,oddtype)]

wide_odds=dcast(latest_odd,matchId~oddtype,value.var='close_odd')
#olasýlýklara çevirmek, toplamý 1'den büyük toplamlarýný al-o sayýya böl normalize et
wide_odds[,home_prob:=1/odd1]

wide_odds[,draw_prob:=1/oddX]

wide_odds[,away_prob:=1/odd2]
# normalize prob - to do


# merge / join result and odds



odds_with_results=merge(wide_odds,matches[,list(matchId,result)],
                        
                        by=c('matchId'),all.x=TRUE)



bucket=c(0:20)/20
bucket2=c(0:20)/10

odds_with_results[,discrete_home:=cut(home_prob,bucket)]



summary_buckets_home=odds_with_results[,list(homewins=sum(result=='home',na.rm=TRUE)
                                        
                                        ,.N),by=list(discrete_home)]
odds_with_results[,discrete_away:=cut(away_prob,bucket)]
summary_buckets_away=odds_with_results[,list(awaywins=sum(result=='away',na.rm=TRUE)
                                        
                                        ,.N),by=list(discrete_away)]

summary_buckets[,ratio:=homewins/N]

#bookmakerýn tahmn iyiliði, çok yüksek oranla kazanýr denilenlerde zarar

#HW1
ou_odds_all=odds[betType=='ou'& totalhandicap=='2.5']
#odds part
ou_odds=odds[betType=='ou'& totalhandicap=='2.5' & bookmaker=='Betclic']
ou_odds[,timestamp:=as_datetime(date,tz='Turkey')]
ou_odds=ou_odds[order(matchId,oddtype,timestamp)]
latest_ou_odds=ou_odds[,list(last_odd=odd[.N]),list(matchId,oddtype)]#son tahmin baz alýnýr
wide_ou_odds=dcast(latest_ou_odds,matchId~oddtype,value.var='last_odd')
wide_ou_odds$result=rowSums(cbind(wide_ou_odds$over,wide_ou_odds$under))#normalize etmek için toplarýz
wide_ou_odds$over_prob=wide_ou_odds$over/wide_ou_odds$result
wide_ou_odds$under_prob=wide_ou_odds$under/wide_ou_odds$result

#matches part

matches$sum_score=rowSums(cbind(matches$score_home,matches$score_away))

ou_odds_results=merge(wide_ou_odds,matches[,list(matchId,sum_score)],by=c('matchId'),all.x=TRUE)
ou_odds_results[,score:=ifelse(sum_score>2.5,'over','under')]
  
ou_bucket=c(0:20)/20
#ou_odds_results[,discrete_under:=cut(under_prob,ou_bucket)]
ou_odds_results[,discrete_over:=cut(over_prob,ou_bucket)]
summary_ou_buckets_over=ou_odds_results[,list(overwins=sum(score=='over',na.rm=TRUE),.N),by=list(discrete_over)]
#summary_ou_buckets_under=ou_odds_results[,list(underwins=sum(score=='under',na.rm=TRUE),.N),by=list(discrete_under)]

summary_ou_buckets_over[,ratio_over:=overwins/N]
summary_ou_buckets_over=summary_ou_buckets_over[order(discrete_over)]

#summary_ou_buckets_under[,ratio_under:=underwins/N]
#a
summary_avg_ou_buckets_over=ou_odds_results[,list(overwins_mean=mean(score=='over_prob',na.rm=TRUE),overwins_sum=sum(score=='over',na.rm=TRUE),.N),by=list(discrete_over)]
summary_avg_ou_buckets_over[,ratio_over:=overwins_sum/N]

scatterplot(summary_avg_ou_buckets_over$overwins_mean,summary_avg_ou_buckets_over$ratio_over)

k <- 1              
for (i in 1:2) {
  for (j in 1:2) {
    par(mfg=c(i,j,2,2))
    abline(0,1,lwd=3,col=k)
    k <- k+1
  }
}

