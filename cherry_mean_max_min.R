rm(list=ls())

cherry=read.csv(file.choose(), header=FALSE, sep=",")
cherry=data.frame(Girth=cherry[,1], Height=cherry[,2], Volume=cherry[,3])

A=matrix(0,length(cherry[1,]),length(cherry[1,]))

for (i in 1:length(cherry[1,]))
  {
      A[1,i]=mean(cherry[,i])
      A[2,i]=max(cherry[,i])
      A[3,i]=min(cherry[,i])
      A=data.frame(Girth=A[,1], Height=A[,2], Volume=A[,3])
      rownames(A)=c("Mean", "Max", "Min")
  }

print(A)