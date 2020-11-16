optimEnergy = function(inputUTE)
{
  ## ute
  cvu = c(10,15,30)
  gtmax = c(17,15,13)
  n = switch(inputUTE,Uma=1,Duas=2,Tres=3)
  
  ## uhe
  volMax = 100
  volMin = 20
  prod = .85
  engol = 50
  
  ## dg
  volI = 40
  afl = c(13,10,60,34,27,54,10)
  cdef = 500
  
  carga = c(30,43,30,27,90,70,37)
  
  sol = list()
  
  for(i in 1:length(carga))
  {
    
    ## variaveis: gt[i] (i in 1:n),vtur,vf,vv,def
    
    objective.in <- c(cvu[1:n],rep(0,2),.01,cdef)
    const.mat <- rbind(c(rep(1,n),prod,0,0,1),cbind(diag(rep(1,n+2)),rep(0,n+2),rep(0,n+2)),
                       c(rep(0,n+1),1,0,0),c(rep(0,n),rep(1,3),0))
    
    if(i==1){const.rhs <- c(carga[i],gtmax[1:n],engol,volMax,volMin,(volI+afl[i]))}
    else{const.rhs <- c(carga[i],gtmax[1:n],engol,volMax,volMin,(sol[[i-1]]$solution[5]+afl[i]))}
    
    const.dir <- c("==", rep("<=",length(gtmax[1:n])), "<=", "<=",">=","==")
    optimum <- lp(direction="min", objective.in, const.mat,
                  const.dir, const.rhs)
    
    cmo = 0
    
    optimum$cmo = cmo
    
    sol[[i]] = optimum
    
  }
  
  matrizS = NULL
  
  for(i in 1:length(sol))
  {
    matrizS = rbind(matrizS,c(carga[i],sol[[i]]$solution[1:n],sol[[i]]$solution[n+1]*prod,
                              sol[[i]]$solution[(n+1):(n+4)],sol[[i]]$cmo,sol[[i]]$objval))
  }
  
  colnames(matrizS) = c("carga",paste0(rep("gt",n),1:n),"gh1","vtur","vf","vv","def","cmo","z")
  
  return(matrizS)
}


library(shiny)
library(lpSolve)

ui <- fluidPage(
  radioButtons("ute", "Número de UTEs:",
               c("1" = "Uma",
                 "2" = "Duas",
                 "3" = "Tres")),
  # radioButtons("uhe", "Há UHE:",
  #              c("Sim" = "sim",
  #                "Não" = "nao")),
  fluidRow(
    splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot"), plotOutput("distPlot2"))
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    matrizS = optimEnergy(input$ute)
    
    plot(matrizS[,(ncol(matrizS)-2)],col="white",main="Deficit",ylab = "MW")
    lines(matrizS[,(ncol(matrizS)-2)])
    
  })
  
  output$distPlot2 <- renderPlot({
    matrizS = optimEnergy(input$ute)
    
    plot(matrizS[,(ncol(matrizS)-6)],col="white",main="GH",ylab = "MW")
    lines(matrizS[,(ncol(matrizS)-6)])
  })
}

shinyApp(ui, server)
