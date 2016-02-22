cut.prob=function(before.POLYLINE,after.LENGTH){
  length.before=vector()
    for(i in 1:length(before.POLYLINE)){
        poly=as.character(POLYLINE[i])
        poly=strsplit(poly,",")
        length=length(poly[[1]])/2
        length.before=cbind(length.before,length)
    }
  length.before=table(length.before)
  length.after=table(after.LENGTH)
  frequency=vector()
  index=as.integer(row.names(length.before))
  for(i in index[1:500]){
    k=as.character(i)
    frequent=length.after[k]/length.before[k]
    frequency=rbind(frequency,frequent)
  }
  return(frequency)
}

log.y1=frequency^0.5
one.over.y1=1/frequency
lm1.1.100=lm(frequency[1:100]~index[1:100]+0)
lm1.101.350=lm(one.over.y1[101:350]~index[101:350])
prop.table(x, margin = 1)