#secondscript!
#data frames

x1=rnorm(100)
x2=rnorm(100)
x3=rnorm(100)

t=data.frame(a=x1, b=x1+x2, c=x1+x2+x3)

plot(t)