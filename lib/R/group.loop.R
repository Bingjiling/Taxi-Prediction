tests.process=vector(mode="list",length=5)
for (i in 1:5){
	tests.process[[i]]=pre.process(tests[[i]])
}