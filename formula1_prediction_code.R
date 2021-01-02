###Author: Anqi Chen, Zewen Shi 

pit=read.csv("C:\\...\\pitStops.csv")
qualifying=read.csv("C:\\...\\qualifying.csv")
races=read.csv("C:\\...\\races.csv")
results=read.csv("C:\\...\\results.csv")
status=read.csv("C:\\...\\status.csv")
drivers=read.csv("C:\\...\\drivers.csv")
status=read.csv("C:\\...\\status.csv")
constructors=read.csv("C:\\...\\constructors.csv")
circuits=read.csv("C:\\...\\circuits.csv")
cons_result=read.csv("C:\\...\\constructorResults.csv")
driver_standings=read.csv("C:\\...\\driverStandings.csv")
seasons=read.csv("C:\\...\\seasons.csv")
laptimes=read.csv("C:\\...\\lapTimes.csv")

library(dplyr)
attach(races)
###join results and races
df1 = left_join(results,races,by="raceId")
###filter out results from 2010 and onwards
df1 = subset(df1,year >= 2010)
####extract month from races and add a new column
df2 = subset(races,year >= 2010)
df2$month = format(as.Date(df2$date),"%m")

######### Calculate historical lapTime of each racer regarding to each race #########
###join dataset "laptimes" and "races" 
df3 = left_join(laptimes,races,by="raceId")
df3 = subset(df3,year >= 2009)
###drop unnecessary data
df3 = df3 %>% select(-time.x,-date,-time.y)
###change to seconds 
df3$milliseconds = (df3$milliseconds)/1000
###create a new table for the purpose of calculating historical data 
circuit_annual_speed1 = df3 %>% group_by(year=df3$year,driverId=df3$driverId,circuitId=df3$circuitId,raceId=df3$raceId) %>% summarize(avg=mean(milliseconds))
circuit_annual_speed2 = circuit_annual_speed1 %>% arrange(circuit_annual_speed1$driverId, circuit_annual_speed1$circuitId)

detach(races)
attach(circuit_annual_speed2)
count = 0
update = 0
for (i in 1:nrow(circuit_annual_speed2)){
  if (i==1){
    circuit_annual_speed2$up_to_date[i]=circuit_annual_speed2$avg[1]
  }else if (circuit_annual_speed2[i,3] == circuit_annual_speed2[i-1,3]){
    count = count + 1
    a = (update+(circuit_annual_speed2[i-1,5]))/count
    update = a*count
    circuit_annual_speed2$up_to_date[i]=a
  }else if (circuit_annual_speed2[i,3] != circuit_annual_speed2[i-1,3] & i != nrow(circuit_annual_speed2)){
    count = 0
    update = 0
    if (circuit_annual_speed2[i,3] != circuit_annual_speed2[i+1,3]){
      circuit_annual_speed2$up_to_date[i]=circuit_annual_speed2[i,5]
    }else if (circuit_annual_speed2[i,3] == circuit_annual_speed2[i+1,3]){
      circuit_annual_speed2$up_to_date[i]=circuit_annual_speed2[i,5]
    }
  }else if (i==nrow(circuit_annual_speed2)){
    circuit_annual_speed2$up_to_date[i]=circuit_annual_speed2$avg[i]
  }
}
###join with circuits to get the city name 
df4 = left_join(df2, circuits, by='circuitId')
###add temperature and max wind into df4 
Tempt_wind = read.csv("C:\\Users\\zewen\\Desktop\\MGSC661\\Final Project\\formula-1-race-data-19502017\\Tempt_wind.csv")
###set temperature to celsius
df4$avg_day_temperature = (5/9)*(Tempt_wind$temperature-32)
df4$max_day_wind_speed = Tempt_wind$wind.speed

################################# Create final tables ###################################
results_df4 = left_join(results, df4, by='raceId')
results_df4_races = left_join(results_df4, races, by='raceId')
###filter out results from 2010 and onwards
detach(circuit_annual_speed2)
attach(results_df4_races)
results_df4_races_2010_2017 = subset(results_df4_races,year.y >= 2010)
results_df4_races_2010_2017_clean = results_df4_races_2010_2017 %>% select(-time.x,-milliseconds,-date.x,-time.y,-url.x,-url.y
                                                                           ,-circuitRef,-name.y,-alt,-year.y,-round.y,-circuitId.y
                                                                           ,-name.x,-date.y,-time,-url)
detach(results_df4_races)
attach(results_df4_races_2010_2017_clean)
names(results_df4_races_2010_2017_clean)[names(results_df4_races_2010_2017_clean)=="year.x"] = "year"
names(results_df4_races_2010_2017_clean)[names(results_df4_races_2010_2017_clean)=="round.x"] = "round"
names(results_df4_races_2010_2017_clean)[names(results_df4_races_2010_2017_clean)=="circuitId.x"] = "circuitId"
###join with avg_speed for each curcuit for each driver 
final_result_draft1 = left_join(results_df4_races_2010_2017_clean, circuit_annual_speed2, by=c('driverId','raceId','circuitId'))
###clean constructors table
detach(results_df4_races_2010_2017_clean)
attach(constructors)
constructors = constructors%>% select(-url,-X)
###join with constructor 
final_table = left_join(final_result_draft1, constructors, by='constructorId')
###rename and rearrange final_table
detach(constructors)
attach(final_table)
final_table = final_table %>% select(-year.y,-avg,-constructorRef,-position,-fastestLap,-rank,-fastestLapTime,-fastestLapSpeed)
names(final_table)[names(final_table)=="year.x"] = "year"
names(final_table)[names(final_table)=="name.x"] = "circuit_name"
names(final_table)[names(final_table)=="name.y"] = "constructor_name"
col_order = c('resultId','raceId','driverId','circuitId','constructorId','year','month','number','grid','positionText','positionOrder'
              ,'statusId','points','laps','round','location','country','lat','lng','avg_day_temperature','max_day_wind_speed'
              ,'circuit_name','up_to_date','constructor_name','nationality')
final_table = final_table[, col_order]
###drop null value 
final_table = final_table[up_to_date != "NULL",]
###add target variable 
final_table$target = ifelse(final_table$positionOrder <= 10, 1, 0)
###change variable class
final_table$up_to_date = unlist(final_table$up_to_date)
final_table$target = as.factor(final_table$target)
final_table$month = as.integer(final_table$month)
final_table$location = as.character(final_table$location)
final_table$constructor_name = as.character(final_table$constructor_name)
final_table$location = as.factor(final_table$location)
final_table$constructor_name = as.factor(final_table$constructor_name)

################################ Plot distribution of the variables ########################################
library(ggplot2)
library(ggfortify)
###histogram of month 
ggplot(data=final_table,aes(x=month))+geom_histogram(aes(fill=..count..))+
  scale_fill_gradient("Count",low="light green",high="dark blue")+
  scale_x_continuous("month", labels = as.character(month), breaks = month,)+
  labs(y="number of races held", x = "month")+ggtitle("Distribution of Games 2010-2017")+
  theme(plot.title = element_text(hjust = 0.5))
###histogram of statusId 
ggplot(data=final_table,aes(x=statusId))+geom_histogram(aes(fill=..count..))+
  scale_fill_gradient("Count",low="light green",high="dark blue")+
  labs(y="Number of each racing status", x = "Status")+ggtitle("Distribution of Racing Status 2010-2017")+
  theme(plot.title = element_text(hjust = 0.5))
## plot of average day temperature
ggplot(data=final_table,aes(x=year,y=avg_day_temperature,color=location))+
  geom_point(size=1)+labs(y="average temperature", x = "year")+
  ggtitle("Average temperature of each location, 2010-2017")+
  theme(plot.title = element_text(hjust = 0.5))
###plot of max wind speed 
ggplot(data=final_table,aes(x=year,y=max_day_wind_speed,color=location))+
  geom_point(size=1)+labs(y="max day wind speed", x = "year")+
  ggtitle("Max day wind speed of each location, 2010-2017")+
  theme(plot.title = element_text(hjust = 0.5))
###histogram of laps 
ggplot(data=final_table,aes(x=laps))+geom_histogram(aes(fill=..count..))+
  scale_fill_gradient("Count",low="light green",high="dark blue")
###histogram of up_to_date 
ggplot(data=final_table,aes(x=up_to_date))+geom_histogram(aes(fill=..count..))+
  scale_fill_gradient("Count",low="light green",high="dark blue")+
  labs(y="Number of each historical speed", x = "Historical Average Lap Speed")+
  ggtitle("Distribution of Historical Average Lap Speed 2010-2017")+
  theme(plot.title = element_text(hjust = 0.5))
###plot of grid with respect to target
ggplot(data =final_table, mapping = aes(x=grid, y=target,color=grid))+
  geom_point()+scale_color_gradient(low="dark blue", high="light green")+
  labs(y="result", x = "grid")+ggtitle("Grid position with regard to result")+
  theme(plot.title = element_text(hjust = 0.5))
###plot of constructor with respect to target 
final_table$target = ifelse(final_table$positionOrder <= 10, 1, 0)
test01= final_table %>% group_by(constructors=final_table$constructor_name) %>% summarize(number=sum(target==1)) 
ggplot(data=test01,aes(x=constructors,fill=number))+geom_col(mapping=aes(y=number))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradient("number",low="light green",high="dark blue")+
  ggtitle("Number of wins achieved by each constructor, 2010-2017")+
  labs(y="number of wins")+theme(plot.title = element_text(hjust = 0.5))
###correlation Matrix
numerical_vars_with_target = final_table[,c(6,7,9,15,18,19,20,21,23,26)]
numerical_vars_with_target$target = as.integer(numerical_vars_with_target$target)
library(GGally)
ggcorr(numerical_vars_with_target)+scale_fill_gradient2(low="light green",high="dark blue",mid="gray91")

#################################### feature selection #####################################
###logistic Regression
detach(final_table)
attach(final_table)

final_table$target = as.factor(final_table$target)
mlogit=glm(final_table$target~grid+up_to_date+year+circuit_name+nationality+max_day_wind_speed,
           family="binomial")
summary(mlogit)

test_data=subset(final_table,select=c("grid","up_to_date","year","circuit_name",
                                      "nationality","max_day_wind_speed"))
test_logit= predict(mlogit,test_data,type="response")
test_logit = ifelse(test_logit>0.5,1,0)
(nrow(final_table)-sum(test_logit==final_table$target))/nrow(final_table)
library(randomForest)
###random forest to check importance of predictors 
myforest=randomForest(target~year+month+grid+round+location+country+lat+lng+avg_day_temperature+max_day_wind_speed
          +circuit_name+up_to_date+constructor_name+nationality,ntree=10000,data=final_table,importance=TRUE)
importance(myforest)
varImpPlot(myforest)
###numerical feature selection 
###PCA for numerical correlation 
numerical_vars_no_target = final_table[,c(6,7,9,15,18,19,20,21,23)]
numerical_vars_with_target = final_table[,c(6,7,9,15,18,19,20,21,23,26)]
numerical_vars_with_target$target = as.integer(numerical_vars_with_target$target)
pca1 = prcomp(numerical_vars_with_target, scale=TRUE)
autoplot(pca1,data=numerical_vars_with_target,loadings=TRUE,col='grey',loadings.label=TRUE)
pca1
pca2 = prcomp(numerical_vars_no_target, scale=TRUE)
autoplot(pca2,data=numerical_vars_no_target,loadings=TRUE,col=ifelse(numerical_vars_with_target$target==2,'green','light blue'),loadings.label=TRUE)
pca2
###then we found that round and month extremly correlated 
###from PCA, lodistic regression,and Random Forest
###we choose grid,constructor_name,nationality,up_to_date,year,circuit_name,max_day_wind_speed 
###we find constructor_name and nationality has higher correlation from logistic reression 
################################### model selection ###################################################
###split train/test in 70%/30%
trainIndex = sample(1:nrow(final_table), 0.7 * nrow(final_table))
train = final_table[trainIndex,]
test = final_table[-trainIndex,]
###decision tree 
detach(final_table)
attach(train)
library(tree)
library(rpart)
library(rpart.plot)
overfittedtree = rpart(target~grid+nationality+up_to_date+year+circuit_name+max_day_wind_speed
             ,control = rpart.control(cp = 0.0000000004))
printcp(overfittedtree)
plotcp(overfittedtree)
min(overfittedtree$cptable[,"xerror"])
overfittedtree$cptable[which.min(overfittedtree$cptable[,"xerror"]),"CP"]
###random forest
randomforest = randomForest(target~grid+nationality+up_to_date+year+circuit_name+max_day_wind_speed 
                            ,ntree = 100000,data = train, do.trace = 1000, importance = TRUE)
randomforest
############################################ Results ####################################################
###compare by testing accuracy score
detach(train)
attach(test)
###accuracy score for decision tree
decisiontree_final = rpart(target~grid+nationality+up_to_date+year+circuit_name+max_day_wind_speed,control = rpart.control(cp = 0.004528986),data = train)
t_pred_dt = predict(decisiontree_final,test,type = 'class')
error_rate_dt = (nrow(test)-sum(t_pred_dt==test$target))/nrow(test)
accuracy_score_dt = 1 - error_rate_dt
accuracy_score_dt
###accuracy score for random forest 
randomforest_final = randomForest(target~grid+nationality+up_to_date+year+circuit_name+max_day_wind_speed,ntree = 5000,data = train)
t_pred_rf = predict(randomforest_final,test,type='class')
error_rate_rf = (nrow(test)-sum(t_pred_rf==test$target))/nrow(test)
accuracy_score_rf = 1 - error_rate_rf
accuracy_score_rf
###random forest stat
randomforest_final
