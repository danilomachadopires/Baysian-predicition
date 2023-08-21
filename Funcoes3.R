rm(list=ls(all=T))
dyn.load("newratings.so")            # chamando as funções .c

# ----------------------------Função Glicko1 implementada

#      -A entrada deve ser um matrix com 4 variáveis (Data,White,Black,Score),
#      White e Black podem ser do tipo character ou numeric, ....
#      -init corresponde ao rating inicial e o desvio inicial por definição init=c(2200,300);
#      -delta é a vantagem inicial ( uma única por banco de dados e não por jogador)
#      -rmax é o desvio maximo de rating, cv é o fator de atualização do desvio
#      nomes pode ser um vetor com os nomes dos jogadores caso o x$White e o 
#      x$Black sejam numericos. Por default names=NULL e os nomes são extraidos 
#      do próprio banco.
#       status corresponde a informações iniciais, tem o mesmo formato da saida (Player,Rating,Deviation, Games,Win,Draw,Loss,Lag) 

GL1<-function(x,nomes=NULL,status=NULL,init=c(2200,300),delta=0,rmax=350,cval=15){
  
  if(!is.data.frame(x)) x <- as.data.frame(x)
  
  names(x) <- c("Data","White","Black","Score")
  
  if(!is.numeric(x$Data)) 
    stop("Time period must be numeric")
  if(!is.numeric(x$White) && !is.character(x$White))
    stop("Player identifiers must be numeric or character")
  if(!is.numeric(x$Black) && !is.character(x$Black))
    stop("Player identifiers must be numeric or character")	
  if(!is.numeric(x$Score) || any(x$Score > 1) || any(x$Score < 0))
    stop("Game scores must be in the interval [0,1]")
  
               
  if(is.null(nomes)){
    nomes <- sort(unique(c(x$Black,x$White)))  
  }
  
  if(is.null(status)){
    PriReRd    <- data.frame(Player=nomes,Rating=rep(init[1],length(nomes)),
                             Deviation=rep(init[2],length(nomes)),Games=rep(0,length(nomes)),
                             Win=rep(0,length(nomes)),Draw=rep(0,length(nomes)),Loss=rep(0,length(nomes)),
                             Lag=rep(0,length(nomes)),Gr=rep(0,length(nomes)),
                             d2=rep(0,length(nomes))) 
    PriReRd$ref<- as.numeric(factor(PriReRd$Player))    
    PriReRd    <- PriReRd[order(PriReRd$ref),]  
  }else{
    if(!is.data.frame(status)) status <- as.data.frame(status)
    
    names(status) <- c("Player","Rating","Deviation","Games","Win","Draw","Loss","Lag")
    
    PriReRd    <- data.frame(Player=nomes,Rating=rep(init[1],length(nomes)),
                             Deviation=rep(init[2],length(nomes)),Games=rep(0,length(nomes)),
                             Win=rep(0,length(nomes)),Draw=rep(0,length(nomes)),Loss=rep(0,length(nomes)),
                             Lag=rep(0,length(nomes)),Gr=rep(0,length(nomes)),
                             d2=rep(0,length(nomes))) 
    PriReRd$ref<- as.numeric(factor(PriReRd$Player))    
    PriReRd    <- PriReRd[order(PriReRd$ref),]  
    
    for (j in 1:length(PriReRd$ref)){
      
      PriReRd$Rating[j]   <- status$Rating[which(status$Player==as.character(PriReRd$Player[j]))]
      PriReRd$Deviation[j] <- status$Deviation[which(status$Player==as.character(PriReRd$Player[j]))]
      PriReRd$Lag[j]       <- status$Lag[which(status$Player==as.character(PriReRd$Player[j]))]    
    }
    
  } 
  qv     <- log(10)/400            
  vdelta <- delta 
  np     <- length(nomes)               # Número de jogadores
  meses  <- unique(x$Data)
  nm     <- length(meses)               # Número de período (mêses)
  
  if(is.character(x$White)){ 
    x$White <- match(x$White,nomes)                 
    x$Black <- match(x$Black,nomes)}
 
   #  Ratings de cada jogador por mês. Usado para analise descritiva
  HRatings    <-matrix(0,nrow=np,ncol=nm+2) 
  HRatings[,1]<- PriReRd$ref
  HRatings[,2]<- PriReRd$Rating 
  
  # Desvios do Ratings de cada jogador por mês   # Usado para analise descritiva
  HDRatings    <-matrix(0,nrow=np,ncol=nm+2) 
  HDRatings[,1]<- PriReRd$ref
  HDRatings[,2]<- PriReRd$Deviation  
  
  aux <-1
  for( i in meses){ #LOOPING POR MÊS
    
    # banco referente, número de jogos e jogadores referentes ao mês i
    bancoaux  <- x[which(x$Data==i),]    
    njogos    <- length(bancoaux$Score)
    trainipl  <- c(bancoaux$White,bancoaux$Black)
    playi     <- unique(trainipl)  # jogadoreds presentes no banco do mes i
    
    # Atualização inicial dos jogadores do bloco ( usando lag=1)    
    PriReRd$Deviation[playi] <-  pmin(sqrt((PriReRd$Deviation[playi]^2)+(cval^2)*(PriReRd$Lag[playi]+1)),rmax) 
    
    # Cálculo da função G(RD) de cada jogador presente no mês    
    PriReRd$Gr        <-  1/sqrt(1+3*((qv* PriReRd$Deviation/pi)^2))        
    
   
    PriReRd$Games <- PriReRd$Games+tabulate(trainipl,np)  
    
    wins <- c(bancoaux$White[bancoaux$Score==1],bancoaux$Black[bancoaux$Score==0])
    draw <- c(bancoaux$White[bancoaux$Score==0.5],bancoaux$Black[bancoaux$Score==0.5])
    loss <- c(bancoaux$White[bancoaux$Score==0],bancoaux$Black[bancoaux$Score==1])
   
    PriReRd$Win   <-  PriReRd$Win + tabulate(wins, np) 
    PriReRd$Draw  <- PriReRd$Draw + tabulate(draw, np)
    PriReRd$Loss  <- PriReRd$Loss + tabulate(loss, np)
    
 
    dscore <- .C("Gli_c",
                 as.integer(np), as.integer(njogos), as.integer(bancoaux$White-1), as.integer(bancoaux$Black-1),
                 as.double(bancoaux$Score), as.double( PriReRd$Rating), as.double(PriReRd$Gr ),as.double(rep(vdelta,njogos)),
                 dscore = double(2*np))$dscore    # o resultado não sai na escola Elo
    
                    d2  <- dscore[(np+1):(2*np)]

    PriReRd$Deviation   <- 1/((1/(PriReRd$Deviation^2)) +d2)   # aqui esta sendo calculado a variância. 
    
    PriReRd$Rating      <- PriReRd$Rating+ PriReRd$Deviation*qv*dscore[1:np]
    
     
    #  Colocando os desvios no data.frame de saída, ao inves da variância.     
    PriReRd$Deviation      <- sqrt(PriReRd$Deviation)
    
    # Atualizando lag, lembrando que a primeira vez de cada jogador o lag=0, e
    # para os jogadores que jogram a partida corrente, seu lag também é 0;     
    PriReRd$Lag[PriReRd$Games!=0] <- PriReRd$Lag[PriReRd$Games!=0]+1   
    PriReRd$Lag[playi]            <- 0  
    
    # Armazenando ratings e desvios dos ratings a cada mês      
    HRatings[,aux+2] <- PriReRd$Rating
    HDRatings[,aux+2]<- PriReRd$Deviation
    aux <-aux+1
  } # FIM DO LOOPING DO MÊS
  
  HistoricoR     <- data.frame(Player=PriReRd$Player,Ref=HRatings[,1],Data=HRatings[,-1])
  HistoricoD     <- data.frame(Player=PriReRd$Player,Ref=HDRatings[,1],Data=HDRatings[,-1])
  
  Estfinal    <- PriReRd[,-c(9,10)]
  Estimativas <- Estfinal[order(Estfinal$Rating,decreasing = TRUE),]
  row.names(Estimativas ) <- 1:nrow(Estimativas )  
  
  Final <- list (ratings= Estimativas,HR=HistoricoR,HDes=HistoricoD,Delta=vdelta,type = "GLI")   
  class(Final) <- "GL1"
  return(Final)
}

#----------------------------------ELO Implementado----------------------------
#      -x um matrix com 4 variáveis (Data,White,Black,Score),
#      White e Black podem ser do tipo character ou numeric, ....
#     - init corresponde ao rating inicial e por definição init=c(2200);
#      -K is the development coefficient.
#      nomes pode ser um vetor com os nomes dos jogadores por default names=NULL e os nomes são extraidos 
#      do próprio banco.
#      -status corresponde a informações iniciais, tem o mesmo formato da saida (Player,Rating,Deviation, Games,Win,Draw,Loss,Lag) 

#K = 40 for a player new to the rating list until he has completed events with at least 30 games
#K = 20 as long as a player's rating remains under 2400.
#K = 10 once a player's published rating has reached 2400 and remains at that level subsequently, even if the rating drops below 2400.
#K = 40 for all players until their 18th birthday, as long as their rating remains under 2300.

ELO<-function(x,nomes=NULL,status=NULL,init=2200,delta=0,k=27){

  if(!is.data.frame(x)) x <- as.data.frame(x)
  names(x) <- c("Data","White","Black","Score")
 
  if(!is.numeric(x$Data)) 
    stop("Time period must be numeric")
  if(!is.numeric(x$White) && !is.character(x$White))
    stop("Player identifiers must be numeric or character")
  if(!is.numeric(x$Black) && !is.character(x$Black))
    stop("Player identifiers must be numeric or character")	
  if(!is.numeric(x$Score) || any(x$Score > 1) || any(x$Score < 0))
    stop("Game scores must be in the interval [0,1]")

  #      Operação feita para extrair os nomes do Banco de dados caso não seja fornecido como entrada
  if(is.null(nomes)){
    nomes<-sort(unique(c(x$Black,x$White)))  
  }
  
  # Data.frame usado como resumo de todas as informações dos jogadores
  # Players-> nome dos jogadores (identificação)
  # Rating -> ratings dos jogadores
  # Games  -> número de jogos de cada jogadores
  # Win    -> número de vitórias de cada jogador
  # Draw   -> número de empates de cada jogador
  # Loss    -> número de derrota de cada jogador
  # Lag    -> tempo que os jogadores ficaram ociosos desde da última partida jogada
  if(is.null(status)){
    PriReRd    <- data.frame(Player=nomes,Rating=rep(init[1],length(nomes)),
                             Deviation=rep(0,length(nomes)), Games=rep(0,length(nomes)),
                             Win=rep(0,length(nomes)),Draw=rep(0,length(nomes)),Loss=rep(0,length(nomes)),
                             Lag=rep(0,length(nomes))) 
    PriReRd$ref<- as.numeric(factor(PriReRd$Player))    
    PriReRd    <- PriReRd[order(PriReRd$ref),]  
  }else{   # Caso seja fornecido um status como entrada
    if(!is.data.frame(status)) status <- as.data.frame(status)
    
    names(status) <- c("Player","Rating","Deviation","Games","Win","Draw","Loss","Lag")
    
    PriReRd    <- data.frame(Player=nomes,Rating=rep(init[1],length(nomes)),
                             Deviation=rep(0,length(nomes)), Games=rep(0,length(nomes)),
                             Win=rep(0,length(nomes)),Draw=rep(0,length(nomes)),Loss=rep(0,length(nomes)),
                             Lag=rep(0,length(nomes))) 
    PriReRd$ref<- as.numeric(factor(PriReRd$Player))    
    PriReRd    <- PriReRd[order(PriReRd$ref),]  
    
    for (j in 1:length(PriReRd$ref)){
      
      PriReRd$Rating[j]   <- status$Rating[which(status$Player==as.character(PriReRd$Player[j]))]
      
    }
    
  }
  
 vdelta <- delta  
    np  <- length(nomes)               # Número de jogadores
  meses <- unique(x$Data)
  nm    <- length(meses)                 # Número de período (mêses)
  

  if(is.character(x$White)){ 
    x$White <- match(x$White,nomes)                 
    x$Black <- match(x$Black,nomes)}
  
  
  #  Ratings de cada jogador por mês usado para ANÁLISE DESCRITIVA e PREDITIVA
  HRatings     <- matrix(0,nrow=np,ncol=nm+2) 
  HRatings[,1] <- PriReRd$ref
  HRatings[,2] <- PriReRd$Rating
  aux<-1
  
  for( i in meses){   #Looping dos meses (cada indice da data é um mes) 
    
    bancoaux  <- x[which(x$Data==i),]  ;bancoaux 
    njogos    <- length(bancoaux$Score) 
    trainipl  <- c(bancoaux$White,bancoaux$Black)          # os jogadores de todas as partidas
    playi     <- unique(c(bancoaux$White,bancoaux$Black))  # os jogadores do mes i
    
    # número de jogos de cada jogador    
    PriReRd$Games <- PriReRd$Games+tabulate(trainipl,np)   # soma os jogos de cada jogador, aquele não tiver fica com 0 
    
    wins <- c(bancoaux$White[bancoaux$Score==1],bancoaux$Black[bancoaux$Score==0]) # jogadores que venceram de brancas e de negras
    draw <- c(bancoaux$White[bancoaux$Score==0.5],bancoaux$Black[bancoaux$Score==0.5]) #jogadores que empataram
    loss <- c(bancoaux$White[bancoaux$Score==0],bancoaux$Black[bancoaux$Score==1])  # jogadores que perderam de brancas e de pretas
    
    PriReRd$Win   <-  PriReRd$Win + tabulate(wins, np) 
    PriReRd$Draw  <- PriReRd$Draw + tabulate(draw, np)
    PriReRd$Loss  <- PriReRd$Loss + tabulate(loss, np)
    Ratingsi      <-  PriReRd$Rating
    
    # a diferença do resultado observado e o esperado para cada jogador
    dscore <- .C("ELO_c",
                 as.integer(np), as.integer(njogos), as.integer(bancoaux$White-1), as.integer(bancoaux$Black-1),
                 as.double(bancoaux$Score),as.double(rep(vdelta,njogos)) ,as.double(PriReRd$Rating), dscore = double(np))$dscore
    
    
    PriReRd$Rating   <- PriReRd$Rating + k*dscore
    HRatings[,aux+2] <- PriReRd$Rating
    aux <- aux+1
    PriReRd
  } # Fim do looping dos mêses
  
  
  Historico         <- data.frame(Player=PriReRd$Player,Ref=HRatings[,1],Data=HRatings[,-1])      
  PriReRd$Deviation <- apply(HRatings[,-1],1,sd)
  PriReRdF <- PriReRd[order(PriReRd$Rating,decreasing = TRUE),]   # saida
   row.names( PriReRdF  ) <- 1:nrow( PriReRdF  )  
  Final    <- list (ratings= PriReRdF,HR=Historico,Delta=vdelta,type = "ELO") 
  class(Final) <- "rating"
  return(Final)
}


#                  Função B_P
BP <- function (x,nomes=NULL,status=NULL,init=c(2200,100,30,10,20,10)){
  
  if(!is.data.frame(x)) x <- as.data.frame(x)
  names(x) <- c("Data","White","Black","Score")
  
  if(!is.numeric(x$Data)) 
    stop("Time period must be numeric")
  if(!is.numeric(x$White) && !is.character(x$White))
    stop("Player identifiers must be numeric or character")
  if(!is.numeric(x$Black) && !is.character(x$Black))
    stop("Player identifiers must be numeric or character")	
  if(!is.numeric(x$Score) || any(x$Score > 1) || any(x$Score < 0))
    stop("Game scores must be in the interval [0,1]")
  if(is.null(nomes)){
    nomes <-sort(unique(c(x$Black,x$White)))  
  }
  
  qv     <- (log(10)/400)
  
  if(is.null(status)){
    
    PriReRd    <- data.frame(Player=nomes,Rating=rep(init[1]*qv,length(nomes)),
                             Deviation=rep(init[2]*qv,length(nomes))^2,
                             Games=rep(0,length(nomes)), Win=rep(0,length(nomes)),
                             Draw=rep(0,length(nomes)),Loss=rep(0,length(nomes)),
                             Lag=rep(0,length(nomes))) 
    PriReRd$ref<- as.numeric(factor(PriReRd$Player))    
    PriReRd    <- PriReRd[order(PriReRd$ref),]  
    
    delta  <- init[3]*qv
    vard   <- (init[4]*qv)^2
    Lambda <- init[5]*qv
    vLam   <- (init[6]*qv)^2  

  }else{
    if(!is.data.frame(status)) status <- as.data.frame(status)
    
    names(status) <- c("Player","Rating","Deviation","Games","Win","Draw","Loss","Lag")
    
    PriReRd    <- data.frame(Player=nomes,Rating=rep(init[1]*qv,length(nomes)),
                             Deviation=rep(init[2]*qv,length(nomes))^2,
                             Games=rep(0,length(nomes)), Win=rep(0,length(nomes)),
                             Draw=rep(0,length(nomes)),Loss=rep(0,length(nomes)),
                             Lag=rep(0,length(nomes))) 
    PriReRd$ref<- as.numeric(factor(PriReRd$Player))    
    PriReRd    <- PriReRd[order(PriReRd$ref),]  

    delta  <-init[3]*qv
    vard   <-(init[4]*qv)^2
    Lambda <-init[5]*qv
    vLam   <-(init[6]*qv)^2  
    
    for (j in 1:length(PriReRd$ref)){
  
      PriReRd$Rating[j]    <- status$Rating[which(status$Player==as.character(PriReRd$Player[j]))]*qv
      PriReRd$Deviation[j]  <- (status$Deviation[which(status$Player==as.character(PriReRd$Player[j]))]*qv)^2
      
    }
    
  }
  
  np    <- length(nomes)               # Numero de jogadores
  meses <- unique(x$Data)
  nm    <- length(meses)                 # Número de período (mêses)
  
  if(is.character(x$White)){ 
    x$White <- match(x$White,nomes)                 
    x$Black <- match(x$Black,nomes)}
  
  #  Ratings de cada jogador por mês   # Usado para analise descritiva
  HRatings     <- matrix(0,nrow=np,ncol=nm+2) 
  HRatings[,1] <- PriReRd$ref
  HRatings[,2] <- PriReRd$Rating/qv    
  
  HL    <- c(0:(nm))*0 
  HD    <- HL
  HL[1] <- Lambda/qv ; HD[1] <-delta/qv
  

  Par  <- c((10*qv)^2,(15*qv)^2,(10*qv)^2)       
  nu0  <- c(10,10,10)
  S0   <- Par*nu0    

  aux <-1
  for( i in meses){
    
    bancoaux  <- x[which(x$Data==i),]    
    njogos    <- length(bancoaux$Score) 
    trainipl  <- c(bancoaux$White,bancoaux$Black)
    playi     <- unique(trainipl)
    
    Games     <- tabulate(trainipl,np) 
    PriReRd$Games <- PriReRd$Games+Games
    
    wins <- c(bancoaux$White[bancoaux$Score==1],bancoaux$Black[bancoaux$Score==0])
    draw <- c(bancoaux$White[bancoaux$Score==0.5],bancoaux$Black[bancoaux$Score==0.5])
    loss <- c(bancoaux$White[bancoaux$Score==0],bancoaux$Black[bancoaux$Score==1])

    PriReRd$Win   <-  PriReRd$Win + tabulate(wins, np) 
    PriReRd$Draw  <- PriReRd$Draw + tabulate(draw, np)
    PriReRd$Loss  <- PriReRd$Loss + tabulate(loss, np)
    
    #-----------------O delta, o var(delta) , o lambda e o var(lambda) devem ser passados
    #                                   como vetores    
    d  <- rep(delta,njogos)
    vd <- rep(vard,njogos)
    l  <- rep(Lambda,njogos)
    vl <- rep(vLam,njogos) 
    
    Retorno <- .C("Bp_c", as.integer(njogos), as.integer(bancoaux$White-1), 
                  as.integer(bancoaux$Black-1), as.double(bancoaux$Score), as.double(PriReRd$Rating),
                  as.double(PriReRd$Deviation), as.double(d),as.double(vd),as.double(l),as.double(vl),
                  ATR= double(np),ATD= double(np),ATd=double(njogos),ATl=double(njogos),
                  Avd=double(njogos),AvL=double(njogos))  
    
    ATR <- Retorno$ATR    # Atualização dos ratings
    ATD <- Retorno$ATD    # Atualização dos desvios dos ratings
    
    Atd <- sum(Retorno$ATd)    # Atualização do delta
    Atl <- sum(Retorno$ATl)    # Atualização do Lambda
    
    ATvd <- sum(Retorno$Avd) # Variância do Lambda
    ATvL <- sum(Retorno$AvL) # Variância do delta
    
    
    PriReRd$Rating <- PriReRd$Rating + ATR  # Atualização dos ratings ao fim do mês i
    delta           <- delta  + Atd           # Atualização do delta ao fim do mês i
    Lambda          <- Lambda + Atl           # Atualização do lambda ao fim do mês i
    
    HRatings[,aux+2] <- PriReRd$Rating/qv
    HD[aux+1]  <- delta/qv
    HL[aux+1]  <- Lambda/qv
    aux <- aux+1
    
    #-------------------------------------------------
    #  Atualizar variâncias
    
    PriReRd$Deviation[Games!=0] <- (ATD[Games!=0]+nu0[1]*S0[1])/(Games[Games!=0]+nu0[1]+2)

    vard   <- (ATvd +nu0[2]*S0[2])/(njogos+nu0[2]+2)
    vLam   <- (ATvL +nu0[3]*S0[3])/(njogos+nu0[3]+2)
    
    # Talvez corrigir pois está limitando a variancias em 350
    PriReRd$Deviation[Games!=0] <- pmin(PriReRd$Deviation[Games!=0],qv*350*350)

  }   # FIM DO LOOPING MÊS
  HistoricoR       <- data.frame(Player=PriReRd$Player,Ref=HRatings[,1],Data=HRatings[,-1])
  
  PriReRd$Rating <- PriReRd$Rating/qv
  PriReRd$Deviation    <- (sqrt(PriReRd$Deviation)/qv)
  
  PriReRd <-PriReRd[order(PriReRd$Rating,decreasing = TRUE),]
  row.names(PriReRd ) <- 1:nrow(PriReRd )  
  
  LADEL  <- data.frame(Delta=delta/qv,DeviationD=sqrt(vard)/qv,Draw=Lambda/qv,DeviationDraw=sqrt(vLam)/qv) 
  
  Final  <- list(ratings=  PriReRd,Parameters=LADEL,HR=HistoricoR  ,HD=HD,HL=HL,type = "B_P")
  class(Final) <- "rating"
  return(Final)
  
}  

#---------------------Glicko 2
# Adapted of the package PlayerRatings.
#Stephenson, Alec, Jeff Sonas, and Maintainer Alec Stephenson. "Package ‘PlayerRatings’."(2019).

"glicko2" <- function(x, status=NULL, init=c(2200,300,0.15), gamma=0, tau=1.2, history=FALSE, sort=TRUE, rdmax = 350, ...)
{ 
  if(!is.data.frame(x)) x <- as.data.frame(x)
  if(!is.data.frame(status) && !is.null(status)) status <- as.data.frame(status)
  qv <- log(10)/400
  if(length(init) != 3) stop("the length of 'init' must be three")
  if(init[2] <= 0) stop("initial deviation must be positive")
  if(init[2] > rdmax) stop("initial deviation cannot be greater than rdmax")
  if(init[3] <= 0) stop("initial volatility must be positive")
  if(init[3] > qv*rdmax) stop("initial volatility cannot be greater than log(10)*rdmax/400")
  if(ncol(x) != 4) stop("'x' must have four variables")
  if(nrow(x) == 0) {
    if(is.null(status)) stop("'x' is empty and 'status' is NULL")
    lout <- list(ratings=status, history=NULL, gamma=gamma, tau=tau, type = "Glicko-2")
    class(lout) <- "rating"
    return(lout)
  }
  gammas <- rep(gamma, length.out = nrow(x)) 
  names(x) <- c("Month","White","Black","Score")
  if(!is.numeric(x$Month)) 
    stop("Time period must be numeric")
  if(!is.numeric(x$White) && !is.character(x$White))
    stop("Player identifiers must be numeric or character")
  if(!is.numeric(x$Black) && !is.character(x$Black))
    stop("Player identifiers must be numeric or character")	
  if(!is.numeric(x$Score) || any(x$Score > 1) || any(x$Score < 0))
    stop("Game scores must be in the interval [0,1]")
  
  play <- sort(unique(c(x$White,x$Black)))
  np <- length(play)
  x$White <- match(x$White, play)
  x$Black <- match(x$Black, play)
  
  if(!is.null(status)) {
    npadd <- play[!(play %in% status$Player)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(Player = npadd, Rating = rep(init[1],length(npadd)), 
                           Deviation = rep(init[2],length(npadd)), Volatility = rep(init[3],length(npadd)), 
                           Games = zv, Win = zv, Draw = zv, Loss = zv, Lag = zv)
    if(!("Games" %in% names(status))) status <- cbind(status, Games = 0)
    if(!("Win" %in% names(status))) status <- cbind(status, Win = 0)
    if(!("Draw" %in% names(status))) status <- cbind(status, Draw = 0)
    if(!("Loss" %in% names(status))) status <- cbind(status, Loss = 0)
    if(!("Lag" %in% names(status))) status <- cbind(status, Lag = 0)
    status <- rbind(status[,c("Player","Rating","Deviation","Volatility","Games","Win","Draw","Loss","Lag")], npstatus)
    rinit <- status[[2]]
    dinit <- status[[3]]
    vinit <- status[[4]]
    ngames <- status[[5]]
    nwin <- status[[6]]
    ndraw <- status[[7]]
    nloss <- status[[8]]
    nlag <- status[[9]]
    names(rinit) <- names(dinit) <- names(vinit) <- names(ngames) <- status$Player
  }
  else {
    rinit <- rep(init[1], length.out=np)
    dinit <- rep(init[2], length.out=np)
    vinit <- rep(init[3], length.out=np)
    ngames <- nwin <- ndraw <- nloss <- rep(0, length.out=np)
    nlag <- rep(0,np)
    names(rinit) <- names(dinit) <- names(vinit) <- names(ngames) <- names(nlag) <- play
  }
  
  if(!all(names(rinit) == names(ngames)))
    stop("names of ratings and ngames are different")
  if(!all(play %in% names(rinit))) 
    stop("Payers in data are not within current status")
  
  # conversion to Glicko-2 scale
  rinit <- qv*(rinit - 1500)
  gammas <- qv*gammas
  dinit <- qv*dinit
  
  nm <- length(unique(x$Month))
  curplay <- match(play, names(rinit))
  orats <- rinit[-curplay] 
  odevs <- dinit[-curplay]^2
  ovols <- vinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  ondraw <- ndraw[-curplay]
  onloss <- nloss[-curplay]
  olag <- nlag[-curplay]
  olag[ongames != 0] <- olag[ongames != 0] + nm
  crats <- rinit[curplay] 
  cdevs <- dinit[curplay]^2
  cvols <- vinit[curplay]
  ngames <- ngames[curplay] 
  nwin <- nwin[curplay]
  ndraw <- ndraw[curplay]
  nloss <- nloss[curplay]
  nlag <- nlag[curplay]
  
  gammas <- split(gammas, x$Month)
  x <- split(x, x$Month)
  players <- lapply(x, function(y) unique(c(y$White, y$Black)))
  if(history) {
    histry <- array(NA, dim=c(np,nm,5), dimnames=list(play,1:nm,c("Rating","Deviation","Volatility","Games","Lag")))
  }
  
  tau2 <- tau * tau
  
  #  Ratings de cada jogador por mês. Usado para analise descritiva
  
  HRatings    <-matrix(0,nrow=np,ncol=nm+2) 
  HRatings[,1]<- play
  HRatings[,2]<- as.numeric(crats/qv + 1500) 
  
  # Desvios do Ratings de cada jogador por mês   # Usado para analise descritiva
  
  HDRatings    <- HRatings 
  HDRatings[,1]<- HRatings[,1]
  HDRatings[,2]<- as.numeric(dinit/qv)
  
  aux <-1
  
  for(i in 1:nm) {
    traini <- x[[i]]
    gammai <- gammas[[i]]
    nr <- nrow(traini)
    playi <- players[[i]]
    
    # nlag[payi]*(cvols[playi]^2) in Glicko-2, (nlag[payi]+1)*(cval^2) in Glicko
    rdmax2 <- qv * qv * rdmax * rdmax
    cdevs[playi] <- pmin(cdevs[playi] + (nlag[playi])*(cvols[playi]^2), rdmax2)
    qip3 <- 3*(1/pi)^2 
    gdevs <- 1/sqrt(1 + qip3*cdevs) 
    ngamesi <- tabulate(c(traini$White,traini$Black), np)
    dscore <- .C("glicko2_c",
                 as.integer(np), as.integer(nr), as.integer(traini$White-1), as.integer(traini$Black-1),
                 as.double(traini$Score), as.double(crats), as.double(gdevs), as.double(gammai),
                 dscore = double(2*np))$dscore
    vval <- dscore[(np+1):(2*np)]; dscore <- dscore[1:np]
    
    if(tau > 0) {
      nllh <- function(z, pz, cdev, vvi, dsco) {
        denom <- cdev + exp(z) + vvi
        delta <- vvi * dsco
        (z - pz) * (z - pz) / tau2 + log(denom) + delta*delta/denom
      }
      for(k in seq_along(playi)) {
        prv <- 2*log(cvols[playi[k]])
        oval <- optimize(nllh, lower = prv-4*tau, upper = prv+4*tau, pz = prv, 
                         cdev = cdevs[playi[k]], vvi = 1/vval[playi[k]], dsco = dscore[playi[k]])$minimum
        cvols[playi[k]] <- min(exp(oval/2), qv * rdmax) 
      }
    }
    
    cdevs[playi] <- cdevs[playi] + cvols[playi]^2
    cdevs <- pmin(1/(1/cdevs + vval), rdmax2)
    crats <- crats + cdevs * dscore
    
    trainiplw <- c(traini$White[traini$Score==1],traini$Black[traini$Score==0])
    trainipld <- c(traini$White[traini$Score==0.5],traini$Black[traini$Score==0.5])
    trainipll <- c(traini$White[traini$Score==0],traini$Black[traini$Score==1])
    ngames <- ngames + ngamesi
    nwin <- nwin + tabulate(trainiplw, np)
    ndraw <- ndraw + tabulate(trainipld, np)
    nloss <- nloss + tabulate(trainipll, np)
    nlag[ngames!=0] <- nlag[ngames!=0] + 1
    nlag[playi] <- 0
    
    if(history) {
      histry[,i,1] <- crats
      histry[,i,2] <- sqrt(cdevs)
      histry[,i,3] <- cvols
      histry[,i,4] <- ngames
      histry[,i,5] <- nlag
    }
    
    HRatings[,aux+2] <- as.numeric(crats/qv + 1500) 
    HDRatings[,aux+2]<- as.numeric(sqrt(cdevs)/qv)
    aux<- aux+1
  }
  
  if(!history) histry <- NULL
  player <- suppressWarnings(as.numeric(names(c(crats,orats))))
  if (any(is.na(player))) player <- names(c(crats,orats))
  ref<-1:length(player)
  
  HistoricoR         <- data.frame(Player=play,Ref=ref,Data=HRatings[,-1])
  HistoricoD         <- data.frame(Player=play,Ref=ref,Data=HDRatings[,-1]) ####################MEEXI AQUI
  
  dfout <- data.frame(Player=player, Rating=c(crats,orats), Deviation=sqrt(c(cdevs,odevs)), Volatility=c(cvols,ovols), 
                      Games=c(ngames,ongames), Win=c(nwin,onwin), Draw=c(ndraw,ondraw), Loss=c(nloss,onloss), 
                      Lag=c(nlag,olag),ref=ref, stringsAsFactors = FALSE)
  if(sort) dfout <- dfout[order(dfout$Rating,decreasing=TRUE),] else dfout <- dfout[order(dfout$Player),]
  row.names(dfout) <- 1:nrow(dfout)
  
  # conversion from Glicko-2 scale
  dfout$Rating <- dfout$Rating/qv + 1500
  dfout$Deviation <- dfout$Deviation/qv
  if(history) {
    histry[,,1] <- histry[,,1]/qv + 1500
    histry[,,2] <- histry[,,2]/qv
  }
  
  lout <- list(ratings=dfout, history=histry, Delta=gamma, tau=tau,HR=HistoricoR,HDes=HistoricoD, type = "Glicko-2")
  class(lout) <- "rating"
  lout
}

QMeasureSD<- function(x,saida_1,saida_2,saida_3,saida_4,op=T){  
 
  names(x) <- c("Data","White","Black","Score")
  
  if(!is.numeric(x$Data)) 
    stop("Time period must be numeric")
  if(!is.numeric(x$White) && !is.character(x$White))
    stop("Player identifiers must be numeric or character")
  if(!is.numeric(x$Black) && !is.character(x$Black))
    stop("Player identifiers must be numeric or character")	
  if(!is.numeric(x$Score) || any(x$Score > 1) || any(x$Score < 0))
    stop("Game scores must be in the interval [0,1]")
  
  nomes <- saida_1$ratings$Player 
  
  if(is.character(x$White)){ 
    x$White <- match(x$White,nomes)                 
    x$Black <- match(x$Black,nomes)} 

  if(op){
  meses <- unique(x$Data)
  nm    <- length(meses)                 # Número de período (mêses)
  nj    <- length(x$White)               # Jogos
  
  cont  <- 2
  contm <- 4
  
  AICD<-matrix(0,8,ncol=nm)


  rownames(AICD)<- c(saida_1$type,saida_2$type,saida_3$type,saida_4$type,
                                      saida_1$type,saida_2$type,saida_3$type,saida_4$type)
  aux<-  1    # necessário pq nem sempre a variavel meses coincide com a posiçaõ no vetor. 
  
  for( i in meses){    # Looping por mes
    
    bancoaux  <- x[which(x$Data==i),]    
    njogos    <- length(bancoaux$Score) 
    
    Y1  <- as.numeric(bancoaux$Score==1)
    Y05 <- as.numeric(bancoaux$Score==0.5)  
    Y0  <- as.numeric(bancoaux$Score==0)
    
    # Criando entradas para a função em C 
    Delo <- as.double(rep(saida_1$Delta,njogos)) 
    Dgli <- as.double(rep(saida_2$Delta,njogos))
    Dgli2<- as.double(rep(saida_3$Delta,njogos))
    dBP  <- rep(saida_4$HD[cont],njogos)  # 1 delta para  cada jogo comença do mês 2
    lBP  <- rep(saida_4$HL[cont],njogos)  # 1 lambda para cada jogo começa do mês 2
    
    
    Resultados <- .C("Analise_c",
                     as.integer(njogos), as.integer(bancoaux$White-1), as.integer(bancoaux$Black-1),
                     as.double(Y1),as.double(Y05),as.double(Y0),as.double(bancoaux$Score),
                     as.double(saida_1$HR[,contm]),as.double(Delo),
                     as.double(saida_2$HR[,contm]),as.double(saida_2$HDes[,contm]),as.double(Dgli),
                     as.double(saida_3$HR[,contm]),as.double(saida_3$HDes[,contm]),as.double(Dgli2),
                     as.double(saida_4$HR[,contm]),as.double(dBP), as.double(lBP), 
                     LvELO2=double(njogos),DELO2=double(njogos),
                     LvELO3=double(njogos),DELO3=double(njogos), 
                     LvGLI2=double(njogos),DGLI2=double(njogos),
                     LvGLI3=double(njogos),DGLI3=double(njogos),
                     LvGLI22=double(njogos),DGLI22=double(njogos),
                     LvGLI23=double(njogos),DGLI23=double(njogos),
                     LvBP2=double(njogos), DBP2=double(njogos),
                     LvBP3=double(njogos), DBP3=double(njogos))

    AICD[1,aux] <-  -2*mean(Resultados$LvELO2)
    AICD[2,aux] <-  -2*mean(Resultados$LvGLI2)
    AICD[3,aux] <-  -2*mean(Resultados$LvGLI22)
    AICD[4,aux] <-  -2*mean(Resultados$LvBP2)
   
    
    AICD[5,aux] <-  -2*mean(Resultados$LvELO3)
    AICD[6,aux] <-  -2*mean(Resultados$LvGLI3)
    AICD[7,aux] <-  -2*mean(Resultados$LvGLI23)
    AICD[8,aux] <-  -2*mean(Resultados$LvBP3)
    
    contm <- contm + 1
    cont  <- cont + 1
    aux   <- aux+1
    
  }
  op<-F
}
  

  if(!op){ 
    
    meses <- meses[-length(meses)]
    contm <- 3
    cont  <- 1
    AICT <- matrix(0,8,ncol=(nm-1))
   
 rownames(AICT) <- c(saida_1$type,saida_2$type,saida_3$type,saida_4$type,
                                      saida_1$type,saida_2$type,saida_3$type,saida_4$type)
  aux<-  1    # necessário pq nem sempre a variavel meses coincide com a posiçaõ no vetor. 
  
  for( i in meses){    # Looping por mes
    
    bancoaux  <- x[which(x$Data==i),]    
    njogos    <- length(bancoaux$Score) 
    
    Y1  <- as.numeric(bancoaux$Score==1)
    Y05 <- as.numeric(bancoaux$Score==0.5)  
    Y0  <- as.numeric(bancoaux$Score==0)
    
 
    Delo <- as.double(rep(saida_1$Delta,njogos)) 
    Dgli <- as.double(rep(saida_2$Delta,njogos))
    Dgli2<- as.double(rep(saida_3$Delta,njogos))
    dBP  <- rep(saida_4$HD[cont],njogos)  # 1 delta para  cada jogo comença do mês 2
    lBP  <- rep(saida_4$HL[cont],njogos)  # 1 lambda para cada jogo começa do mês 2
    
    
    Resultados <- .C("Analise_c",
                     as.integer(njogos), as.integer(bancoaux$White-1), as.integer(bancoaux$Black-1),
                     as.double(Y1),as.double(Y05),as.double(Y0),as.double(bancoaux$Score),
                     as.double(saida_1$HR[,contm]),as.double(Delo),
                     as.double(saida_2$HR[,contm]),as.double(saida_2$HDes[,contm]),as.double(Dgli),
                     as.double(saida_3$HR[,contm]),as.double(saida_3$HDes[,contm]),as.double(Dgli2),
                     as.double(saida_4$HR[,contm]),as.double(dBP), as.double(lBP), 
                     LvELO2=double(njogos),DELO2=double(njogos),
                     LvELO3=double(njogos),DELO3=double(njogos), 
                     LvGLI2=double(njogos),DGLI2=double(njogos),
                     LvGLI3=double(njogos),DGLI3=double(njogos),
                     LvGLI22=double(njogos),DGLI22=double(njogos),
                     LvGLI23=double(njogos),DGLI23=double(njogos),
                     LvBP2=double(njogos), DBP2=double(njogos),
                     LvBP3=double(njogos), DBP3=double(njogos))
    
    AICT[1,aux] <-  -2*mean(Resultados$LvELO2)
    AICT[2,aux] <-  -2*mean(Resultados$LvGLI2)
    AICT[3,aux] <-  -2*mean(Resultados$LvGLI22)
    AICT[4,aux] <-  -2*mean(Resultados$LvBP2)
    
    AICT[5,aux] <-  -2*mean(Resultados$LvELO3)
    AICT[6,aux] <-  -2*mean(Resultados$LvGLI3)
    AICT[7,aux] <-  -2*mean(Resultados$LvGLI23)
    AICT[8,aux] <-  -2*mean(Resultados$LvBP3)
      
    contm <- contm + 1
    cont  <- cont + 1
    aux   <- aux+1
      
    }  
    
  }

  
  Title2 <- "Two outcomes-Description"
  Title3 <- "Three outcomes-Description"
  Title4 <- "Two outcomes-Prediction"
  Title5 <- "Three outcomes-Prediction"
  
  par(mfrow = c(2, 2))
  
  # DOIS RESULTADOS
  
  miAicd <- min(AICD[1:4,])*0.9
  maAicd <- max(AICD[1:4,])*1.1
  plot(AICD[1,],ylim=c(miAicd,maAicd),main=Title2,ylab="log-Likehood",xlab="Rounds",type='l',lwd=2)
  par(new=T)
  plot(AICD[2,],ylim=c(miAicd,maAicd),main="",ylab="",xlab="",col=2,type='l',lwd=2)
  par(new=T)
  plot(AICD[3,],ylim=c(miAicd,maAicd),main="",ylab="",xlab="",col=4,type='l',lwd=2)
  par(new=T)
  plot(AICD[4,],ylim=c(miAicd,maAicd),main="",ylab="",xlab="",col=5,type='l',lwd=2)
  legend("topright",c(saida_1$type,saida_2$type,saida_3$type,saida_4$type),fill=c(1,2,4,5),cex = 0.6,bty="n")
  

  minAicd <- min(AICD[5:8,])*0.9
  maxAicd <- max(AICD[5:8,])*1.1
  plot(AICD[5,],ylim=c(minAicd,maxAicd),main=Title3,ylab="log-Likehood",xlab="Rounds",type='l',lwd=2)
  par(new=T)
  plot(AICD[6,],ylim=c(minAicd,maxAicd),main="",ylab="",xlab="",col=2,type='l',lwd=2)
  par(new=T)
  plot(AICD[7,],ylim=c(minAicd,maxAicd),main="",ylab="",xlab="",col=4,type='l',lwd=2)
  par(new=T)
  plot(AICD[8,],ylim=c(minAicd,maxAicd),main="",ylab="",xlab="",col=5,type='l',lwd=2)
  legend("topright",c(saida_1$type,saida_2$type,saida_3$type,saida_4$type),fill=c(1,2,4,5),cex = 0.6,bty="n") 
  
 #TRES RESULTADOS
  miAict <- min(AICT[1:4,])*0.9
  maAict <- max(AICT[1:4,])*1.1
  plot(AICT[1,],ylim=c(miAict,maAict),main=Title4,ylab="log-Likehood",xlab="Rounds",type='l',lwd=2)
  par(new=T)
  plot(AICT[2,],ylim=c(miAict,maAict),main="",ylab="",xlab="",col=2,type='l',lwd=2)
  par(new=T)
  plot(AICT[3,],ylim=c(miAict,maAict),main="",ylab="",xlab="",col=4,type='l',lwd=2)
  par(new=T)
  plot(AICT[4,],ylim=c(miAict,maAict),main="",ylab="",xlab="",col=5,type='l',lwd=2)
  legend("topright",c(saida_1$type,saida_2$type,saida_3$type,saida_4$type),fill=c(1,2,4,5),cex = 0.6,bty="n")
  
  
  
  minAict <- min(AICT[5:8,])*0.9
  maxAict <- max(AICT[5:8,])*1.1
  plot(AICT[5,],ylim=c(minAict,maxAict),main=Title5,ylab="log-Likehood",xlab="Rounds",type='l',lwd=2)
  par(new=T)
  plot(AICT[6,],ylim=c(minAict,maxAict),main="",ylab="",xlab="",col=2,type='l',lwd=2)
  par(new=T)
  plot(AICT[7,],ylim=c(minAict,maxAict),main="",ylab="",xlab="",col=4,type='l',lwd=2)
  par(new=T)
  plot(AICT[8,],ylim=c(minAict,maxAict),main="",ylab="",xlab="",col=5,type='l',lwd=2)
  legend("topright",c(saida_1$type,saida_2$type,saida_3$type,saida_4$type),fill=c(1,2,4,5),cex = 0.6,bty="n") 
}



#Somente para dois modelos  ( em cima os maiores e embaixo os menores)


GTOP4 <- function(saida_1,saida_2,d ){

# Maiores ratings    
    Top1   <- head(saida_1$ratings,4)
    Top2   <- head(saida_2$ratings,4)

    Tit1 <- c("Highest Ratings",saida_1$type)
    Tit2 <- c("Highest Ratings",saida_2$type)

    TR1 <- Top1$ref 
    TR2 <- Top2$ref  
    # limites dos gráficos
    L1 <- c(min(as.numeric(as.matrix(saida_1$HR[TR1,-c(1,2)]))),max(as.numeric(as.matrix(saida_1$HR[TR1,-c(1,2)]))))
    L2 <- c(min(as.numeric(as.matrix(saida_2$HR[TR2,-c(1,2)]))),max(as.numeric(as.matrix(saida_2$HR[TR2,-c(1,2)]))))
    Posi<-"bottomright"
    
#   menores ratings
    NTop1   <- tail(saida_1$ratings,4)
    NTop2   <- tail(saida_2$ratings,4)

    NTit1 <- c("Lowest Ratings",saida_1$type)
    NTit2 <- c("Lowest Ratings",saida_2$type)

    NTR1 <- NTop1$ref 
    NTR2 <- NTop2$ref  

    NL1 <- c( min(saida_1$HR[NTR1,-c(1,2)]), max(saida_1$HR[NTR1,-c(1,2)]))
    NL2 <- c( min(saida_2$HR[NTR2,-c(1,2)]), max(saida_2$HR[NTR2,-c(1,2)]))
    NPosi<-"bottomleft"  
 
  
  H1 <- as.matrix(saida_1$HR)
  H2 <- as.matrix(saida_2$HR)
  
  NH1 <- as.matrix(saida_1$HR)
  NH2 <- as.matrix(saida_2$HR)
  
  
  par(mfrow = c(2, 2))  
  color <-1
  
  for (i in TR1){
    plot(as.numeric(H1[i,-c(1,2)]),main=Tit1,xlab="Rounds",
         ylim=L1,type='l',col=color,ylab="Ratings",lwd=2) 
    par(new=T)
    color <-color+1
  }  
  legend(Posi,as.character(Top1$Player),fill=1:color,bty="n",cex = d)  
  
  par(new=F); color<-1
  
  for (i in TR2){
    plot (as.numeric(H2[i,-c(1,2)]),main=Tit2,xlab="Rounds",
          ylim=L2,type='l',col=color,ylab="Ratings",lwd=2) 
    par(new=T)
    
    color <-color+1
  }  
  legend(Posi,as.character(Top2$Player),fill=1:color,bty="n",cex = d)  
  
  par(new=F); color<-1
  
  for (i in NTR1){  # corrigir esse formato
    plot (as.numeric(NH1[i,-c(1,2)]),main= NTit1,xlab="Rounds",
          ylim=NL1,type='l',col=color,ylab="Ratings",lwd=2) 
    par(new=T)
    color <-color+1
  }  
  legend(NPosi,as.character(NTop1$Player),fill=1:color,bty="n",cex =d)  
  
  par(new=F); color<-1
  
  for (i in NTR2){
    plot (as.numeric(NH2[i,-c(1,2)]),main=NTit2,xlab="Rounds",
          ylim=NL2,type='l',col=color,ylab="Ratings",lwd=2) 
    par(new=T)
    color <-color+1
  }  
  legend(NPosi,as.character(NTop2$Player),fill=1:color,bty="n",cex = d)  
  par(new=F)
  par(mfrow = c(1, 1))  
}

