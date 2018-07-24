rm(list=ls())

T=readline("Tempo di ritorno: ")
  T=as.numeric(T)
CV=readline("CV: ")
  CV=as.numeric(CV)
n=readline("n: ")
  n=as.numeric(n)
m1=readline("m1: ")
  m1=as.numeric(m1)

tc=readline("Tempo di corrivazione: ")
  tc=as.numeric(tc)
Dt=readline("Time-step dello ieogramma: ")
  Dt=as.numeric(Dt)
r=readline("Posizione del picco: ")
  r=as.numeric(r)
  
AMC=readline("AMC: ")
  AMC=as.numeric(AMC)
CN2=readline("CN2: ")
  CN2=as.numeric(CN2)
c=readline("c: ")
  c=as.numeric(c)

v=readline("Valore della velocità: ")
  v=as.numeric(v)
Ra=readline("Rapporto delle aree: ")
  Ra=as.numeric(Ra)
Rb=readline("Rapporto di biforcazione: ")
  Rb=as.numeric(Ra)
Rl=readline("Rapporto delle lunghezza: ")
  Rl=as.numeric(Rl)
L=readline("Lunghezza dell'asta principale: ")
  L=as.numeric(L)
S=readline("Superficie del bacino: ")
  S=as.numeric(S)
lag=readline("Tempo di lag: ")
  lag=as.numeric(lag)
  
d=1:24
t=seq(from=Dt, to=tc, by=Dt)
ta=r*tc; 
ts=(1-r)*tc

#CPP
wt=1-CV*(0.45+0.779*log(log(T/(T-1))))
a=m1*wt
h=a*d^(n)
plot(d, h, type = "l", col="blue")

#Ietogramma Lordo
Mat=matrix(0, length(t), 8)

tavi=seq(from=ta, to=Dt, by=-1)
Mat[,2]=c(tavi, matrix(0, length(t)-length(tavi), 1))

tsvi=seq(from=ts-Dt, to=0, by=-Dt)
tsvi=c(tsvi, matrix(0, length(t)-length(tsvi), 1))
Mat[,4]=rev(tsvi)

tsvi1=seq(from=ts, to=Dt, by=-Dt)
tsvi1=c(tsvi1, matrix(0, length(t)-length(tsvi1), 1))
Mat[,5]=rev(tsvi1)

Mat[,6]=a*((Mat[,2]^n-Mat[,3]^n)/(Dt*r^(n-1)))
Mat[,7]=a*((Mat[,5]^n-Mat[,4]^n)/(Dt*(1-r)^(n-1)))
Mat[,8]=Mat[,6]+Mat[,7]

i=Mat[,8]

barplot(i)

#CN
if (AMC==1)
{
  CN=CN2/(2.38-0.0138*CN2)
  print("CN1")
} else if (AMC==3)
{
  CN=CN2/(0.43+0.0057*CN2)
  print("CN3")
} else {
  CN=CN2
  print("CN2")
}

S=254*(100/CN-1)
Ia=c*S

P=cumsum(i*Dt)

deflusso=(((P-Ia)^2)/(P-Ia*S))
inferiori=which(P<Ia)
deflusso[inferiori]=0
deflusso2=matrix(0, length(deflusso), 1)
deflusso2[2:length(deflusso2)]=deflusso[1:length(deflusso)-1]
deflusso=deflusso-deflusso2
P=deflusso

barplot(i)
barplot(P)

#GIUH
N=(3.29*(Rb/Ra)^0.78)*(Rl^0.07)
k=(0.7*(Ra/Rb*Rl)^0.48)*(L/v)
x=seq(from=0, to=lag*3600)
ht=gammaf(x, k, N)
indici=seq(from=Dt*3600, to=lag*3600, by=Dt*3600)

UH1=ht[indici]
UH2=matrix(0, 1:length(UH1)+1, 1)
UH2[2:length(UH1)+1]=UH1
UH=UH1+UH2


