###################################################################################################
###################################################################################################
## rotina de calculo dos ads tarifarios

setwd("C:\\Estágio\\benchMaio")

source("CalculoAdicionais2019_Rev1.R")

f_ccee = 1.08

###################################################################################################
###################################################################################################
####FUNCAO QUE LE O ARQUIVO PATAMAR
leitura_patamar <- function(linha_comeco, pat)
{
  
  pat <- pat[linha_comeco:(linha_comeco+14),]               #seleciona as linhas 2 a 16
  
  pat <- as.numeric(as.matrix(pat))                       #transforma em vetor
  
  pat_saida <- matrix(data = NA, nrow = 15, ncol = 13)    #cria um auxiliar
  
  pat_saida <- as.numeric(as.matrix(pat_saida))         #transforma em vetor
  
  aux = 16                                                #numero de colunas mais 01
  i = 1
  
  for (i in 1:195){                                       # colunas x linhas 
    
    if(i == aux){
      
      pat_saida[i-15] = pat[i]                            #coloca o elemento em sua posicao correta
      aux = aux + 3
      
    } else {
      pat_saida[i] = pat[i]
    }
    
  }
  
  pat_aux <- matrix(data = NA, nrow = 15, ncol = 13)
  
  for(j in 1:12){
    for(i in 1:15){
      pat_aux[i,j] = pat_saida[i + (j-1)*15]              #cont(linha) + (cont(col)-1)xnum_linha
    }
  }
  
  pat_saida <- pat_aux[,1:12]                             #excluir a ultima linha que é NA
  
  return(pat_saida)
}
###LEITURA DO PATAMAR #####

library(readr)

if(file.exists("patamar.dat")){
  
  patamar <- read_table2("patamar.dat", skip = 4)
  
  anoini <- read.table("cmarg001.out", header=FALSE, sep="",skip=2,fill=TRUE,dec=".", nrows = 1)
  
  anoini <- unlist(anoini[2])
  
  linha <- which(patamar == anoini)
  
  pat_duracao <- leitura_patamar(linha[1],patamar)
  
  pat <- array(NA, c(15,12,4))
  
  for(SM in 1:4){
    pat[,,SM] <- leitura_patamar(linha[SM+1],patamar)
  }
  
}else if(file.exists("patamar.eas")){
  
  patamar <- read_table2("patamar.eas", skip = 4)
  
}


pat_s1 <- pat[1:3,,1]
pat_s2 <- pat[1:3,,2]
pat_s3 <- pat[1:3,,3]
pat_s4 <- pat[1:3,,4]
pat_t = pat_duracao[1:3,]


###################################################################################################
###################################################################################################
## leitura dos dados de entrada

setwd("C:\\Estágio\\nwlistop_2020_11")

## CMO
cmo = list()

for(periodo in 1:5)
{
  k = round(nrow(read.table("cmarg001.out", header=FALSE, sep="",skip=4,fill=TRUE,dec="."))/5000)
  
  skip=5 + (periodo-1)*(k*1000+9)
  
  cmox = list()
  
  cmox[[1]] <- read.table("cmarg001.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 6000)
  cmox[[2]] <- read.table("cmarg002.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 6000)
  cmox[[3]] <- read.table("cmarg003.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 6000)
  cmox[[4]] <- read.table("cmarg004.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 6000)
  
  cmo_modif <- array(data = NA, dim = c(6000,15,4))
  for(z in 1:dim(cmo_modif)[3])
  {
    aux=1
    for (i in 1:6000){ 
      if(i==aux){
        for (j in 1:14) {
          cmo_modif[i,j,z] <-cmox[[z]][i,j+1]
        }
        aux=aux+3
      } else {
        for (j in 1:15){
          cmo_modif[i,j,z] <- cmox[[1]][i,j]
        }
      }
    }
  }
  aux = cmo_modif
  cmo_modif = array(data = NA, dim = c(6000,12,4))
  
  for(z in 1:dim(cmo_modif)[3])
  {
    cmo_modif[,,z] = aux[1:6000,2:13,z]
  }
  
  
  aux = array(dim = c(2000,12,4))
  t=1
  sum=rep(0,4)
  for(z in 1:length(sum))
  {
    for (j in 1:12){
      for (i in 1:2000){
        for (p in 1:3){
          sum[z] <- sum[z]+cmo_modif[t,j,z]*pat_s1[p,j]*pat_t[p,j]
          t=t+1
        }
        aux[i,j,z] <- sum[z]
        sum=rep(0,4)
      }
      t=1
    }
  }
  
  
  
  cmox <- aux
  
  cmo[[periodo]] = cmox
  
}

## PLD
pld=list()

for(periodo in 1:5)
{
  pldx = array(dim = c(2000,12,5))
  for(subs in 1:4)
  {
    aux = cmo[[periodo]][,,subs]
    
    for(i in 1:nrow(aux))
    {
      for(j in 1:ncol(aux))
      {
        if(aux[i,j]>PLDmax){aux[i,j]=PLDmax}
        if(aux[i,j]<PLDmin){aux[i,j]=PLDmin}
      }
    }
    
    pldx[,,subs] = aux
  }
  
  pldx[,,5] = p1*pldx[,,1]+p2*pldx[,,2]+p3*pldx[,,3]+p4*pldx[,,4]
  
  pld[[periodo]] = pldx
}

aux=pld[[2]][,,5]

## GH
ghband = array(dim = c(2000,12,5))
gh_pch = read.csv("gh_pch_2020_11.csv")

for(z in 1:dim(ghband)[3])
{
  k = round(nrow(read.table("ghtotm001.out", header=FALSE, sep="",skip=4,fill=TRUE,dec="."))/5000)
  
  skip=5 + (z-1)*(k*1000+9)
  
  gh1 <- read.table("ghtotm001.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 8000)
  gh2 <- read.table("ghtotm002.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 8000)
  gh3 <- read.table("ghtotm003.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 8000)
  gh4 <- read.table("ghtotm004.out", header=FALSE, sep="",skip=skip,fill=TRUE,dec=".", nrows = 8000)
  
  gh1_t <- matrix(data = NA, nrow = 2000, ncol = 12)
  gh2_t <- matrix(data = NA, nrow = 2000, ncol = 12)
  gh3_t <- matrix(data = NA, nrow = 2000, ncol = 12)
  gh4_t <- matrix(data = NA, nrow = 2000, ncol = 12)
  
  aux=4
  k=1
  t=1
  for (i in 1:8000){ 
    if(i==aux){ 
      for (j in 2:13) {
        gh1_t[k,j-1] <- gh1[t,j]
        gh2_t[k,j-1] <- gh2[t,j]
        gh3_t[k,j-1] <- gh3[t,j]
        gh4_t[k,j-1] <- gh4[t,j]
      }
      aux=aux+4
      k=k+1
      t=t+1
    } else {t=t+1}
  }
  
  gh_sin <- gh1_t+gh2_t+gh3_t+gh4_t
  
  gh_bruto <- matrix(data = NA, nrow = 2000, ncol = 12)
  
  gfband <- c(55456,55184,55125,55129,55190,55435,55586,55654,55654,55682,55673,55653) # InformaCCEE 18/11/2020
  
  #gh_pch = rep(3000,12)
  
  gfom=0	# Eventual acionamento fora da ordem de mérito do CMSE
  
  for (i in 1:2000){
    for (j in 1:12)
    {
      gh_bruto[i,j] <- gh_pch[z,(j+1)] + gh_sin[i,j] + gfom
      if(is.na(gh_bruto[i,j])){gh_bruto[i,j]=0}
    }
  } 
  
  ghband[,,z] <- gh_bruto-(gh_bruto*(f_ccee-1))
  
}

## GSF
gsfband <- array(data = NA, dim = c(2000,12,5))

for(z in 1:dim(gsfband)[3])
{
  for (i in 1:2000){
    for (j in 1:12){gsfband[i,j,z] <- ghband[i,j,z]/gfband[j]}
  } 
  
}

## VU
vugsf <- array(data = NA, dim = c(2000,12,5))
for(z in 1:dim(gsfband)[3])
{
  for (i in 1:2000){
    for (j in 1:12){vugsf[i,j,z] <- (-1 + gsfband[i,j,z])*pld[[z]][i,j,5]}
  }
  
}

verde = lim1
amarela = lim2
vermelha1 = lim3
vermelha2 = lim4

band = vugsf
probs = array(dim = c(4,12,5))
for(z in 1:dim(gsfband)[3])
{
  for(j in 1:dim(gsfband)[2])
  {
    for(i in 1:dim(gsfband)[1])
    {
      if (vugsf[i,j,z] >= verde){
        band[i,j,z]="verde"
      } else if (vugsf[i,j,z] >= amarela) {
        band[i,j,z]="amarela"
      } else if (vugsf[i,j,z] >= vermelha1) {
        band[i,j,z]="vermelho1"
      } else {
        band[i,j,z]="vermelho2"
      }
    }
    
    probs[1,j,z] = length(which(band[,j,z]=="verde"))/length(band[,j,z])
    probs[2,j,z] = length(which(band[,j,z]=="amarela"))/length(band[,j,z])
    probs[3,j,z] = length(which(band[,j,z]=="vermelho1"))/length(band[,j,z])
    probs[4,j,z] = length(which(band[,j,z]=="vermelho2"))/length(band[,j,z])
    
  }
}

probs

gsf = seq(0,.99,length.out = 100)

## vu = (-1+gsf)*pld -> pld = vu/(-1+gsf)

pldv = verde/(-1+gsf)
plda = amarela/(-1+gsf)
pldv1 = vermelha1/(-1+gsf)
pldv2 = vermelha2/(-1+gsf)

meses = c("Janeiro","Fevereiro","Marco")

pdf("abaco2021_NW_2020_11.pdf")

plot(gsf,pldv,xlim = c(0.6,max(gsfband[,,1])),ylim = c(0,PLDmax),col="white",
     main=paste0("Acionamento previsto para ", "Novembro", " de 2020 \n (NEWAVE - 11/2020)"),
     ylab = "PLD (R$/MWh)", xlab = "GSF",cex.sub = .9,cex.lab = .8,cex.axis = .8)
polygon(c(gsf,rev(gsf)),c(pldv,rep(0,length(pldv))),col="green",border = "green")
polygon(c(seq(.99,1.2,length.out = 5),rev(seq(.99,1.2,length.out = 5))),
        c(rep(0,5),rep(700,5)),col="green",border = "green")
polygon(c(gsf,rev(gsf)),c(pldv,rev(plda)),col="gold",border = "gold")
polygon(c(gsf,rev(gsf)),c(pldv1,rev(plda)),col="red",border = "red")
polygon(c(gsf,rev(gsf)),c(pldv1,rev(pldv2)),col="darkred",border = "darkred")

points(gsfband[,11,1],pld[[1]][,11,5],pch=16)
legend("bottomleft",paste(c("Verde:","Amarela:","Vermelha 1:","Vermelha 2:"),probs[,11,1]*100,rep("%",4)),
       pch = 16, col = c("green","gold","red","darkred"),bg = "white")

plot(gsf,pldv,xlim = c(0.6,max(gsfband[,,1])),ylim = c(0,PLDmax),col="white",
     main=paste0("Acionamento previsto para ", "Dezembro", " de 2020 \n (NEWAVE - 11/2020)"),
     ylab = "PLD (R$/MWh)", xlab = "GSF",cex.sub = .9)
polygon(c(gsf,rev(gsf)),c(pldv,rep(0,length(pldv))),col="green",border = "green")
polygon(c(seq(.99,1.2,length.out = 5),rev(seq(.99,1.2,length.out = 5))),
        c(rep(0,5),rep(700,5)),col="green",border = "green")
polygon(c(gsf,rev(gsf)),c(pldv,rev(plda)),col="gold",border = "gold")
polygon(c(gsf,rev(gsf)),c(pldv1,rev(plda)),col="red",border = "red")
polygon(c(gsf,rev(gsf)),c(pldv1,rev(pldv2)),col="darkred",border = "darkred")

points(gsfband[,12,1],pld[[1]][,12,5],pch=16)
legend("bottomleft",paste(c("Verde:","Amarela:","Vermelha 1:","Vermelha 2:"),probs[,12,1]*100,rep("%",4)),
       pch = 16, col = c("green","gold","red","darkred"),bg = "white")

for(i in 1:length(meses))
{
  plot(gsf,pldv,xlim = c(0.6,max(gsfband[,,2])),ylim = c(0,PLDmax),col="white",
       main=paste0("Acionamento previsto para ", meses[i]," de 2021 \n (NEWAVE - 11/2020)"),
       ylab = "PLD (R$/MWh)", xlab = "GSF",cex.sub = .9)
  polygon(c(gsf,rev(gsf)),c(pldv,rep(0,length(pldv))),col="green",border = "green")
  polygon(c(seq(.99,1.2,length.out = 5),rev(seq(.99,1.2,length.out = 5))),
          c(rep(0,5),rep(700,5)),col="green",border = "green")
  polygon(c(gsf,rev(gsf)),c(pldv,rev(plda)),col="gold",border = "gold")
  polygon(c(gsf,rev(gsf)),c(pldv1,rev(plda)),col="red",border = "red")
  polygon(c(gsf,rev(gsf)),c(pldv1,rev(pldv2)),col="darkred",border = "darkred")
  
  points(gsfband[,i,2],pld[[2]][,i,5],pch=16)
  
  legend("bottomleft",paste(c("Verde:","Amarela:","Vermelha 1:","Vermelha 2:"),probs[,i,2]*100,rep("%",4)),
         pch = 16, col = c("green","gold","red","darkred"),bg = "white")
  
  
}
dev.off()
getwd()






