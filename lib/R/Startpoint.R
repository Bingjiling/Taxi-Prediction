Startpoint=function(POLYLINE){
   x=vector()
   for(i in 1:length(POLYLINE)){
     poly=as.character(POLYLINE[i])
     poly=strsplit(poly,",")
     poly.number=gsub("[][]","",poly[[1]][1:2])
     point=as.numeric(poly.number)
     x=rbind(x,point)
   }
   colnames(x)=c("start.point.x","start.point.y")
   return(x)
}

length.play=vector()
for(i in 1:10000){
       poly=as.character(POLYLINE[i])
       poly=strsplit(poly,",")
       length=length(poly[[1]])
       length.play=cbind(length.play,length)
}
wilcox.test(length.play,new0$LENGTH[1:10000])
prop.table(x, margin = 1)