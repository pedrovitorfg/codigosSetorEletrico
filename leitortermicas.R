#### leitor pdo_term e pdo_oper

agoraa = now()
## funcao que transforma valores expressos no tipo caracter em tipo numérico
transf <- function(gf)
{
  aux = as.numeric(gf)
  
  if(is.numeric(aux) && length(aux))
  {
    
    for(i in 1:length(aux))
    {
      if(is.na(aux[i]))
      {
        a8 = strsplit(gf[i],split = "")
        if(is.integer(which(a8[[1]] == ",")) && length(which(a8[[1]] == ",")) == 1)
        {
          
          a8[[1]] = a8[[1]][-which(a8[[1]] == ",")]
          
        }
        if(is.integer(which(a8[[1]] == " ")) && length(which(a8[[1]] == " ")) == 1)
        {
          
          a8[[1]] = a8[[1]][-which(a8[[1]] == " ")]
          
        }
        a10 = paste0(a8[[1]],collapse = "")
        aux[i] = as.numeric(a10)
      }
      
    }
    
  }
  
  # caso nao haja valor algum em tal premio, por o número 0
  if(is.numeric(aux) && length(aux) == 0)
  {
    aux = 0
  }
  
  gf = aux
  
  return(gf)
  
}

tirarEspaco = function(aux)
{
  for(i in 1:length(aux))
  {
    a8 = strsplit(aux[i],split = "")
    a8[[1]] = a8[[1]][-which(a8[[1]] == " ")]
    aux[i] = paste0(a8[[1]],collapse = "")
  }
  
  return(aux)
  
}

library(readr)
library(tidyverse)
library(lubridate)
library(rapportools)

raiz = "C:\\Estágio\\dessem_semana"
setwd(raiz)

meses = c("Janeiro","Fevereiro","Marco","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")
earm_max = c(202691.938,19897.047,51602.07,15164.887) ## fonte ONS - energia agora - reservatórios

### Verificar quais os decks do dessem estão na pasta indicada

ds = list.files(getwd(), pattern = "DS_ONS")

a = strsplit(ds, split = "")
rv = NULL
data = NULL
for(i in 1:length(a))
{
  rv[i] = paste(a[[i]][15:17],sep = "",collapse = "")
  dia = paste(a[[i]][19:20],sep = "",collapse = "")
  mes = paste(a[[i]][8:9],sep = "",collapse = "")
  ano = paste(a[[i]][10:13],sep = "",collapse = "")
  if((rv[i]=="RV0")&&(as.numeric(dia)>23)&&(mes!="01"))
  {
    mes = as.numeric(mes)-1
  }
  if((rv[i]=="RV0")&&(as.numeric(dia)>23)&&(mes=="01"))
  {
    mes = 12
    ano = as.numeric(ano)-1
  }
  
  data[i] = as.character(as.Date(paste(ano,mes,dia,sep = "-")))
  
}
ds = cbind(ds,data,rv)

dadosTerm = NULL
dadosTerm2 = NULL

for(cont in 1:nrow(ds))   
{
  arq2 = list.files(getwd())
  unzip(ds[cont,1],junkpaths=TRUE)
  arq1 = list.files(getwd())
  arq = list.files(getwd(), pattern = ".dat")
  aux = setdiff(arq1,arq2)
  
  term = read_lines("pdo_term.dat")
  
  marc = grep("IPER",term)
  
  matriz = NULL
  
  for(i in marc:(length(term)))
  {
    matriz = rbind(matriz,strsplit(term[i],split = ";")[[1]])
  }
  
  matriz = cbind(matriz,rep(ds[cont,2],nrow(matriz)))
  
  dadosTerm = rbind(dadosTerm,matriz)
  
  if(sum(grepl("pdo_oper_titulacao_usinas.dat",arq))!=0)
  {
    term = read_lines("pdo_oper_titulacao_usinas.dat")
    
    marc = grep("IPER ;",term)
    
    matriz = NULL
    
    for(i in marc:(length(term)))
    {
      matriz = rbind(matriz,strsplit(term[i],split = ";")[[1]])
    }
    
    matriz = cbind(matriz,rep(ds[cont,2],nrow(matriz)))
    
    dadosTerm2 = rbind(dadosTerm2,matriz)
    
  }
  else
  {
    matriz = rep(NA,13)
    matriz = cbind(matriz,rep(ds[cont,2],nrow(matriz)))
    dadosTerm2 = rbind(dadosTerm2,matriz)
  }
  
  file.remove(aux)
  
  print(ds[cont,2])
}


dadosTerm = list(dadosTerm,dadosTerm2)

#filtros
usinas = as.matrix(dadosTerm[[1]][-which(duplicated(dadosTerm[[1]][,4])),4])
usinas = usinas[-(1:3)]
subsis = as.matrix(dadosTerm[[1]][-which(duplicated(dadosTerm[[1]][,6])),6])
subsis = subsis[-(1:3)]
periodo = as.matrix(dadosTerm[[1]][-which(duplicated(dadosTerm[[1]][,1])),1])
periodo = periodo[-(1:3)]
datas = as.matrix(dadosTerm[[1]][-which(duplicated(dadosTerm[[1]][,ncol(dadosTerm[[1]])])),ncol(dadosTerm[[1]])])

a =intersect(dadosTerm[[1]][,1],periodo[49:length(periodo)])

for(i in 1:length(a))
{
  dadosTerm[[1]] = dadosTerm[[1]][-which(dadosTerm[[1]][,1]==a[i]),]
}
periodo = as.matrix(dadosTerm[[1]][-which(duplicated(dadosTerm[[1]][,1])),1])
periodo = periodo[-(1:3)]

marcs = list(usinas, subsis, periodo)

## geracao termeletrica por subs individualizada
gerSubs = list()
for(i in 1:length(marcs[[2]]))
{
  gerSubs[[i]] = dadosTerm[[1]][which(dadosTerm[[1]][,6]==marcs[[2]][i]),]
}

## geracao termeletrica por subs a cada instante
gerTot = list()
for(i in 1:length(gerSubs))
{
  matriz = matrix(nrow = length(datas), ncol = length(periodo))
  
  for(idata in 1:length(datas))
  {
    mm = which(gerSubs[[i]][,ncol(gerSubs[[i]])]==datas[idata])
    if(sum(is.empty(mm))==0)
    {
      aux = gerSubs[[i]][mm,]
      a = NULL
      for(per in 1:length(periodo))
      {
        matriz[idata,per] = sum(as.numeric(tirarEspaco(as.matrix(aux[which((aux[,1]==periodo[per])&(aux[,5]==" 99 ")),7]))))
      }
    }
  }
  
  row.names(matriz) = datas
  gerTot[[i]] = t(matriz) ## linha = semi-hora, col = dia
}


## geracao individualizada
gerInd = list()
for(i in 1:length(usinas))
{
  xx = dadosTerm[[1]][which(dadosTerm[[1]][,4]==marcs[[1]][i]),]
  
  y = as.matrix(xx[-which(duplicated(xx[,ncol(xx)])),ncol(xx)])
  x = as.matrix(xx[-which(duplicated(xx[,1])),1])
  
  matriz = NULL
  for(idata in y)
  {
    a = NULL
    for(per in x)
    {
      a = c(a,sum(as.numeric(tirarEspaco(as.matrix(xx[which((xx[,1]==per)&(xx[,ncol(xx)]==idata)&(xx[,5]==" 99 ")),7])))))
    }
    
    matriz = rbind(matriz,a)
  }
  
  row.names(matriz) = y
  gerInd[[i]] = t(matriz) ## linha = semi-hora, col = dia
}



## plots
for(i in 1:length(gerTot))
{
  plot(as.numeric(gerTot[[i]]),col="white", main = paste0("GT - ",tirarEspaco(subsis[[i]])),
       ylab = "MWm", xlab = "Semi-hora")
  lines(as.numeric(gerTot[[i]]))
  grid()
}

for(i in 1:length(gerInd))
{
  plot(as.numeric(gerInd[[i]]),col="white", main = paste0("GT - ",tirarEspaco(usinas[[i]])),
       ylab = "MWm", xlab = "Semi-hora")
  lines(as.numeric(gerInd[[i]]))
  grid()
}


#############################################
## pdo_oper

#filtros
usinas = as.matrix(dadosTerm[[2]][-which(duplicated(dadosTerm[[2]][,3])),3])
usinas = usinas[-(1:3)]
subsis = as.matrix(dadosTerm[[2]][-which(duplicated(dadosTerm[[2]][,4])),4])
subsis = subsis[-(1:3)]
periodo = as.matrix(dadosTerm[[2]][-which(duplicated(dadosTerm[[2]][,1])),1])
periodo = periodo[-(1:3)]
datas = as.matrix(dadosTerm[[2]][-which(duplicated(dadosTerm[[2]][,ncol(dadosTerm[[2]])])),ncol(dadosTerm[[2]])])

a =intersect(dadosTerm[[2]][,1],periodo[49:length(periodo)])

for(i in 1:length(a))
{
  dadosTerm[[2]] = dadosTerm[[2]][-which(dadosTerm[[2]][,1]==a[i]),]
}
periodo = as.matrix(dadosTerm[[2]][-which(duplicated(dadosTerm[[2]][,1])),1])
periodo = periodo[-(1:3)]

marcs = list(usinas, subsis, periodo)

## geracao termeletrica por subs individualizada
gerSubs = list()
for(i in 1:length(marcs[[2]]))
{
  gerSubs[[i]] = dadosTerm[[2]][which(dadosTerm[[2]][,4]==marcs[[2]][i]),]
}

## geracao termeletrica inflexivel por subs a cada instante
gerInflex = list()
## geracao termeletrica por ordem de merito por subs a cada instante
gerOM = list()
## geracao termeletrica por subs a cada instante
gerTot = list()
for(i in 1:length(gerSubs))
{
  matriz = NULL
  matriz2 = NULL
  matriz3 = NULL
  
  for(idata in datas)
  {
    aux = gerSubs[[i]][which(gerSubs[[i]][,ncol(gerSubs[[i]])]==idata),]
    a = NULL
    a2 = NULL
    a3 = NULL
    for(per in periodo)
    {
      a = c(a,sum(as.numeric(tirarEspaco(as.matrix(aux[which((aux[,1]==per)),7])))))
      a2 = c(a2,sum(as.numeric(tirarEspaco(as.matrix(aux[which((aux[,1]==per)),6])))))
      a3 = c(a3,sum(as.numeric(tirarEspaco(as.matrix(aux[which((aux[,1]==per)),5])))))
    }
    
    matriz = rbind(matriz,a)
    matriz2 = rbind(matriz2,a2)
    matriz3 = rbind(matriz3,a3)
  }
  
  row.names(matriz) = datas
  row.names(matriz2) = datas
  row.names(matriz3) = datas
  gerInflex[[i]] = t(matriz) ## linha = semi-hora, col = dia
  gerOM[[i]] = t(matriz2)
  gerTot[[i]] = t(matriz3)
}

## geracao individualizada
gerIndInflex = list()
gerIndOM = list()
gerInd = list()
for(i in 1:length(usinas))
{
  xx = dadosTerm[[2]][which(dadosTerm[[2]][,3]==marcs[[1]][i]),]
  
  y = as.matrix(xx[-which(duplicated(xx[,ncol(xx)])),ncol(xx)])
  x = as.matrix(xx[-which(duplicated(xx[,1])),1])
  
  matriz = NULL
  matriz2 = NULL
  matriz3 = NULL
  for(idata in y)
  {
    a = NULL
    a2 = NULL
    a3 = NULL
    for(per in x)
    {
      a = c(a,sum(as.numeric(tirarEspaco(as.matrix(xx[which((xx[,1]==per)&(xx[,ncol(xx)]==idata)),7])))))
      a2 = c(a2,sum(as.numeric(tirarEspaco(as.matrix(xx[which((xx[,1]==per)&(xx[,ncol(xx)]==idata)),6])))))
      a3 = c(a3,sum(as.numeric(tirarEspaco(as.matrix(xx[which((xx[,1]==per)&(xx[,ncol(xx)]==idata)),5])))))
    }
    
    matriz = rbind(matriz,a)
    matriz2 = rbind(matriz2,a2)
    matriz3 = rbind(matriz3,a3)
  }
  
  row.names(matriz) = y
  row.names(matriz2) = y
  row.names(matriz3) = y
  gerIndInflex[[i]] = t(matriz) ## linha = semi-hora, col = dia
  gerIndOM[[i]] = t(matriz2)
  gerInd[[i]] = t(matriz3)
}



## plots
for(i in 1:length(gerTot))
{
  a = min(c(as.numeric(gerInflex[[i]]),as.numeric(gerOM[[i]]),as.numeric(gerTot[[i]])))
  b = max(c(as.numeric(gerInflex[[i]]),as.numeric(gerOM[[i]]),as.numeric(gerTot[[i]])))
  plot(as.numeric(gerTot[[i]]),col="white", main = paste0("GT - ",tirarEspaco(subsis[[i]])),
       ylab = "MWm", xlab = "Semi-hora",ylim = c(a,b))
  lines(as.numeric(gerInflex[[i]]),col="blue")
  #polygon(c(1:length(as.numeric(gerTot[[i]])),rev(1:length(as.numeric(gerTot[[i]])))),
  #        c(as.numeric(gerInflex[[i]]),rep(0,length(as.numeric(gerTot[[i]])))),
  #        col = "blue")
  lines(as.numeric(gerInflex[[i]])+as.numeric(gerOM[[i]]),col="gold")
  #polygon(c(rev(1:length(as.numeric(gerTot[[i]]))),(1:length(as.numeric(gerTot[[i]])))),
  #         c((as.numeric(gerTot[[i]])),as.numeric(gerInflex[[i]])),
  #         col = "gold")
  lines(as.numeric(gerTot[[i]]))
  grid()
  legend("bottomright",c("Total","Parcela Inflexível","Inflex + Ordem de Mérito"),
         col=c("black","blue","gold"),lty = 1)
}

for(i in 1:length(gerInd))
{
  a = min(c(as.numeric(gerIndInflex[[i]]),as.numeric(gerIndOM[[i]]),as.numeric(gerInd[[i]])))
  b = max(c(as.numeric(gerIndInflex[[i]]),as.numeric(gerIndOM[[i]]),as.numeric(gerInd[[i]])))
  if(a!=b)
  {
    plot(as.numeric(gerInd[[i]]),col="white", main = paste0("GT - ",tirarEspaco(usinas[[i]])),
         ylab = "MWm", xlab = "Semi-hora",ylim = c(a,b))
  }
  else
  {
    plot(as.numeric(gerInd[[i]]),col="white", main = paste0("GT - ",tirarEspaco(usinas[[i]])),
         ylab = "MWm", xlab = "Semi-hora")
  }
  lines(as.numeric(gerIndInflex[[i]]),col="blue")
  lines(as.numeric(gerIndInflex[[i]])+as.numeric(gerIndOM[[i]]),col="gold")
  lines(as.numeric(gerInd[[i]]))
  grid()
  legend("bottomright",c("Total","Parcela Inflexível","Inflex + Ordem de Mérito"),
         col=c("black","blue","gold"),lty = 1)
}



agoraa = now() - agoraa ## 12 minutos para execucao