
#install.packages('data.table')



require(data.table)



setwd('C:/Users/VYKM 2/Desktop/ETM58D')

setwd('C:\\Users\\VYKM 2\\Desktop\\ETM58D')



# read matches info

matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')



# what is in matches?

str(matches)



# filtering '0:0'

matches[score=='0:0']



# filtering '0:0' and home team tottenham

matches[score=='0:0' & home=='tottenham']



# tenth entry

matches[10]



# delete column type

matches[,type:=NULL]



# delete leagueId

matches$leagueId=NULL



# check unique home team

unique(matches$home)



# fix data error

matches[home=='manchester-utd',home:='manchester united']

matches[home=='manchester-united',home:='manchester united']



# check corrections

sort(unique(matches$home))



# fix away error

matches[away %in% c('manchester-utd','manchester-united') ,away:='manchester united']



# manipulate score to break home score from away score

matches[,c('score_home','score_away'):=tstrsplit(score,':')]



write.csv(matches,'deneme.csv',row.names=F)

matches=read.csv('deneme.csv')

#read as data.table

matches=fread('deneme.csv')



#install.packages('readxl')

require(readxl)

excel_matches=data.table(read_excel('deneme.xlsx',1))



a=fread('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data')





matches[,c('score_home','score_away'):=as.numeric(tstrsplit(score,':'))]

matches[,score_home:=as.numeric(score_home)]

matches[,score_away:=as.numeric(score_away)]



# remove score column

matches[,score:=NULL]



# adding result column

matches[,result:=ifelse(score_home>score_away,'home',
                        
                        ifelse(score_home==score_away,'draw','away'))]





#install.packages('lubridate')

require(lubridate)



matches[,timestamp:=as_datetime(date,tz='Turkey')]

matches[,date:=NULL]



# match count by score



match_count=matches[,list(count=length(matchId)),by=list(score_home,score_away)]



# order by count

match_count[order(-count)]



# order by score_home ascending and then score_away ascending

match_count[order(score_home,score_away)]



# average away goals by year

avg_goals=matches[,list(avg=mean(score_away,na.rm=T)),by=list(year(timestamp))]



# average away goals and match count by year

avg_goals=matches[,list(avg=mean(score_away,na.rm=T),count=.N),by=list(year(timestamp))]



# average away goals and match count by hour

avg_goals=matches[,list(avg=mean(score_away,na.rm=T),count=.N),by=list(hour(timestamp))]



# read odds data

odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')



# filter 1x2 type odd, bookmaker pinnacle

filtered_odd=odds[betType=='1x2' & bookmaker=='Pinnacle']

filtered_odd[,totalhandicap:=NULL]

filtered_odd[,timestamp:=as_datetime(date,tz='Turkey')]



filtered_odd=filtered_odd[order(matchId,oddtype,timestamp)]

latest_odds=filtered_odd[,list(close_odd=odd[.N]),list(matchId,oddtype)]



wide_odds=dcast(latest_odds,matchId~oddtype,value.var='close_odd')



wide_odds[,home_prob:=1/odd1]

wide_odds[,draw_prob:=1/oddX]

wide_odds[,away_prob:=1/odd2]



# normalize prob - to do



# merge / join result and odds



odds_with_results=merge(wide_odds,matches[,list(matchId,result)],
                        
                        by=c('matchId'),all.x=TRUE)



bucket=c(0:20)/20

odds_with_results[,discrete_home:=cut(home_prob,bucket)]



summary_buckets=odds_with_results[,list(homewins=sum(result=='home',na.rm=TRUE)
                                        
                                        ,.N),by=list(discrete_home)]

summary_buckets[,ratio:=homewins/N]