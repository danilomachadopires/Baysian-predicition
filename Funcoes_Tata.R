source("Funcoes3.R")

x1      <-read.table("TataSteel2023.txt",h=T) 
nomes1  <-read.table("nomesTata.txt") ; nomes1<-as.character(nomes1$V1)
Status1 <-read.table("Status1Tata.txt",h=T) 
Status1$Deviation <- rep(60,14)  

##-----------------ELO
system.time(saida_1 <- ELO(x1,nomes1,Status1,k=10) )# Usando ratings iniciais

#-----------------Baysian-prediction
system.time(saida_3 <- BP(x1,nomes1,init=c(2200,100,25,15,50,15),Status1))

#-----------------glicko 1 
system.time(saida_5 <- GL1(x1,nomes1,Status1))

#-----------------Glicko 2 
Status6    <- Status1[,c(1,2,3)]
Status6$Volatility <- rep(0.15,dim(Status1)[1])

Status6$Games <- rep(0,dim(Status1)[1])
Status6$Win   <- rep(0,dim(Status1)[1])
Status6$Draw  <- rep(0,dim(Status1)[1])
Status6$Loss  <- rep(0,dim(Status1)[1])
Status6$Lag   <- rep(0,dim(Status1)[1])

Status6$Player <- as.numeric(factor(Status6$Player))

saida_6 <- glicko2(x1,Status6)

saida_6[[1]]$Player <- nomes1[saida_6[[1]]$Player]
#-------------------------------------


#------------------------Figuras-----------------
GTOP4(saida_1,saida_3,d=0.8) 

QMeasureSD(x1,saida_1,saida_6,saida_5,saida_3)

