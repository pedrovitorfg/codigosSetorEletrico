#### leitor pdo_term

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
  
  for(i in marc:(length(term)-1))
  {
    matriz = rbind(matriz,strsplit(term[i],split = ";")[[1]])
  }
  
  matriz = cbind(matriz,rep(ds[cont,2],nrow(matriz)))
  
  dadosTerm = rbind(dadosTerm,matriz)
  
  file.remove(aux)
  
  print(ds[cont,2])
}

#filtros
usinas = as.matrix(dadosTerm[-which(duplicated(dadosTerm[,4])),4])
usinas = usinas[-(1:3)]
subsis = as.matrix(dadosTerm[-which(duplicated(dadosTerm[,6])),6])
subsis = subsis[-(1:3)]
periodo = as.matrix(dadosTerm[-which(duplicated(dadosTerm[,1])),1])
periodo = periodo[-(1:3)]
datas = as.matrix(dadosTerm[-which(duplicated(dadosTerm[,ncol(dadosTerm)])),ncol(dadosTerm)])

a =intersect(dadosTerm[,1],periodo[49:length(periodo)])

for(i in 1:length(a))
{
  dadosTerm = dadosTerm[-which(dadosTerm[,1]==a[i]),]
}
periodo = as.matrix(dadosTerm[-which(duplicated(dadosTerm[,1])),1])
periodo = periodo[-(1:3)]

marcs = list(usinas, subsis, periodo)

## geracao termeletrica por subs individualizada
gerSubs = list()
for(i in 1:length(marcs[[2]]))
{
  gerSubs[[i]] = dadosTerm[which(dadosTerm[,6]==marcs[[2]][i]),]
}

## geracao termeletrica por subs a cada instante
gerTot = list()
for(i in 1:length(gerSubs))
{
  matriz = NULL
  
  for(idata in datas)
  {
    aux = gerSubs[[i]][which(gerSubs[[i]][,ncol(gerSubs[[i]])]==idata),]
    a = NULL
    for(per in periodo)
    {
      a = c(a,sum(as.numeric(tirarEspaco(as.matrix(aux[which((aux[,1]==per)&(aux[,5]==" 99 ")),7])))))
    }
    
    matriz = rbind(matriz,a)
  }
  
  row.names(matriz) = datas
  gerTot[[i]] = t(matriz) ## linha = semi-hora, col = dia
}


## geracao individualizada
gerInd = list()
for(i in 1:length(usinas))
{
  xx = dadosTerm[which(dadosTerm[,4]==marcs[[1]][i]),]
  
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

agoraa = now() - agoraa ## 12 minutos para execucao