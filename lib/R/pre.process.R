pre.process=function(data){
	x=vector()
	for(i in 1:nrow(data)){
     poly=as.character(data$POLYLINE[i])
     poly=strsplit(poly,",")
     poly=poly[[1]]
     time=length(poly)*15/2
     poly.length=length(poly)
     start.point=gsub("[][]","",poly[1:2])
     start.point=as.numeric(start.point)
     end.point=gsub("[][]","",poly[(length(poly)-1):length(poly)])
     end.point=as.numeric(end.point)
     route.vector=end.point-start.point
     row=c(start.point,end.point,route.vector,poly.length,time)
     x=rbind(x,row)
	}
	trip.id=data$TRIP_ID
	timestamp=data$TIMESTAMP
	datetime=data$datetime
	x=cbind(trip.id,datetime,timestamp,x)
  colnames(x)=c("trip.id","datetime","timestamp","start.point.x","start.point.y","end.point.x","end.point.y","route.vector.x","route.vector.y","poly.length","time")
 return(x)
}

