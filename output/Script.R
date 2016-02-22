## set up
setwd('/Users/ZacharyHE/Dropbox/Kaggle (1)')
test <- read.csv('test.csv', header=TRUE)

# build date time and converge it to the same date"2015-01-01"
datetime=as.POSIXct(test$TIMESTAMP, tz='GMT', origin="1970-01-01")
datetime=format(datetime, format="%H:%M:%S")
datetime=paste("2015-01-01",datetime,sep=" ")

# put date time in test set and convert it to character
test=cbind(test,datetime)
test[,'datetime']=as.character(test[,'datetime'])

# divide test into five groups
test1=test[abs(test$TIMESTAMP-1408000000)<200000,]
test2=test[abs(test$TIMESTAMP-1412000000)<200000,]
test3=test[abs(test$TIMESTAMP-1412600000)<200000,]
test4=test[abs(test$TIMESTAMP-1414800000)<200000,]
test5=test[abs(test$TIMESTAMP-1419100000)<200000,]

# put them in a list
tests=list(test1,test2,test3,test4,test5)

# pre-process for test set
tests.process=vector(mode="list",length=5)
for (i in 1:5){
  tests.process[[i]]=pre.process(tests[[i]])
}

# remove the original test sets
rm(test1)
rm(test2)
rm(test3)
rm(test4)
rm(test5)

# build datetime in c.train1 with the same date 2015-01-01
datetime=strptime(c.train1$X,"%Y-%m-%d %H:%M:%S")
datetime=format(datetime, format="%H:%M:%S")
datetime=paste("2015-01-01",datetime,sep=" ")

# put it in c.train1 and convert it to character
c.train1=cbind(c.train1,datetime)
c.train1[,'datetime']=as.character(c.train1[,'datetime'])

# get test1 from tests.process
test1=tests.process[[1]]


# Delete irrelevent variables from c.trian1
c.train1=c.train1[,c(2,9,10)]
startpoint=Startpoint(c.train1$POLYLINE)
c.train1=cbind(c.train1,startpoint)

# Preprocess of train data for specific obs in test
# Use threshold only
trains1=vector(mode="list",length=nrow(test1))
for (i in 14:nrow(test1)) {
  test.time=test1[i,'datetime']
  train.time=c.train1[,'datetime']
  diff.time=difftime(train.time,test.time,units="mins")
  diff.time=abs(as.numeric(diff.time))
  train=c.train1[diff.time<15,]
  startpoint=train[,4:5]
  test.x=as.numeric(test1[i,'start.point.x'])
  test.y=as.numeric(test1[i,'start.point.y'])
  diff.x=abs(startpoint[,1]-test.x)
  diff.y=abs(startpoint[,2]-test.y)
  train=train[diff.x<0.00001&diff.y<0.00001,]
  train=train[complete.cases(train$start.point.x),]
  if (nrow(train)<50){
    train=c.train1[diff.time<15,]
    train=train[diff.x<0.00003&diff.y<0.00003,]
    train=train[complete.cases(train$start.point.x),]
  }
  if (nrow(train)==0){
    bad=train
    trains1[[i]]=bad
  }else{
    train=pre.process.train(train,test1[i,])
    train=train[complete.cases(train[,'end.point.x']),]
    trains1[[i]]=train
  }
}


##
rows=vector()
for (i in 1:74){
rows=c(rows,nrow(trains1[[i]]))
}

# Use threshold and numbers

for (i in 1:nrow(test1)) {
  test.time=test1[i,'datetime']
  train.time=c.train1[,'datetime']
  diff.time=difftime(train.time,test.time,units="mins")
  diff.time=abs(as.numeric(diff.time))
  train=c.train1[diff.time<15,]
  startpoint=train[,4:5]
  test.x=as.numeric(test1[i,'start.point.x'])
  test.y=as.numeric(test1[i,'start.point.y'])
  diff.x=abs(startpoint[,1]-test.x)
  diff.y=abs(startpoint[,2]-test.y)
  index=order(diff.x+diff.y)[1:100]
  train=train[index,]
  train=train[complete.cases(train$start.point.x),]
  if ((diff.x+diff.y)[index[100]]>0.002){
    train=c.train1[diff.time<20,]
    train=train[diff.x<0.002&diff.y<0.002,]
    train=train[complete.cases(train$start.point.x),]
  }
  if (nrow(train)==0){
    bad=train
    trains1[[i]]=bad
  }else{
    train=pre.process.train(train,test1[i,])
    train=train[complete.cases(train[,'end.point.x']),]
    trains1[[i]]=train
  }
}

#Kernel regression
predict1=cbind(test1[,'trip.id'],0)
colnames(predict1)=c("TRIP_ID","TRAVEL_TIME")
for (i in 72:nrow(test1)){
  test=test1[i,]
  train=trains1[[i]]
  diff.x=as.numeric(train[,'start.point.x'])*10^6-as.numeric(test['start.point.x'])*10^6
  diff.y=as.numeric(train[,'start.point.y'])*10^6-as.numeric(test['start.point.y'])*10^6
  weight=10000/((diff.x^2+diff.y^2)+1)
  time=sum((as.numeric(train[,'time'])*weight)/sum(weight))
  predict1[i,2]=time
}



train.play=c.train1[diff.time<15,]
startpoint.play=Startpoint(train.play$POLYLINE)
test.play.x=as.numeric(test1[1,'start.point.x'])
test.play.y=as.numeric(test1[1,'start.point.y'])
diff.x=abs(startpoint.play[,1]-test.play.x)
diff.y=abs(startpoint.play[,2]-test.play.y)
train.play=train.play[diff.x<0.00001&diff.y<0.00001,]
train.play=train.play[complete.cases(train.play$X),]
