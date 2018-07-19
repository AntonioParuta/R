rm(list=ls())

cherry=read.csv(file.choose(), header=FALSE, sep=",")

a=c()

for (i in 1:length(cherry[1,]))
{
  a[i]=mean(cherry[,i])
}

print(a)
