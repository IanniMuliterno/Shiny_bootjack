# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(MASS)
# library(dplyr)
# library(tidyr)

shinyServer(function(input, output) {
  
  
  data <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   gam = rgamma,
                   lnorm = rlnorm,
                   exp = rexp,
                   wei = rweibull,
                   rnorm)
    
    dist
  })
  
  output$paramet <- renderUI({
    if(input$dist == "norm"){
      tagList(
        numericInput("mean_norm", "mean", 0),
        numericInput("sd_norm", "sd", 1))
    }else{
      if(input$dist == "gam"){
        tagList(
          numericInput("gam_shape", "shape", 2),
          numericInput("gam_rate", "rate", 2))
      }else{
        if(input$dist == "lnorm"){
          tagList(
            numericInput("mean_log", "meanlog", 0),
            numericInput("sd_log", "sdlog", 1))
        }else{
          if(input$dist == "exp"){
            numericInput("rate_exp", "rate", 1)
          }else{
            tagList(
              numericInput("shape_wei", "shape", 2),
              numericInput("scale_wei", "scale", 1)
            )
          } } } }
    
  })
  
  #plot 1
  
  output$plot1 <- renderPlot({
    set.seed(100)
    dist <- data()
    
    # setando bootstrap
    
    B1 <- seq(50,500,50) #Numero de replicas
    n <- input$amostra #tamanho da amostra (5 a 100)
    #print(n)
    #Setar parametros de acordo com a escolha do usuario
    if(input$dist == "norm"){
      mean_by_resample <- numeric(length(B1))
      upperval_by_resample <- numeric(length(B1))
      lowerval_by_resample <- numeric(length(B1))
      
      mean_by_resamplesd <- numeric(length(B1))
      upperval_by_resamplesd <- numeric(length(B1))
      lowerval_by_resamplesd <- numeric(length(B1))
      vetor <-dist(n,input$mean_norm,input$sd_norm)
     
       # iniciando bootstrap
      for (k in 1:length(B1)){
        B <- B1[k]   
        R <- numeric(B) #Estoque de rep
        R_sd <- numeric(B)
        #estimativa da media e IC pelo metodo bootstrap
        for (b in 1:B){
          
          #coleta aleatoria de indices
          i <- sample(1:n, size = n, replace = TRUE)
          vetor_boot <- vetor[i] #i vetor de indices
          R[b] <- mean(vetor_boot) #estimando a média da normal
          R_sd[b] <- sd(vetor_boot) #estimando o desvio padrão da normal
        }
        # IC por reamostra
        mean_by_resample[k] <- mean(R) 
        upperval_by_resample[k] <- mean(R)+1.96*sd(R)
        lowerval_by_resample[k] <- mean(R)-1.96*sd(R)
        
        mean_by_resamplesd[k] <- mean(R_sd) 
        upperval_by_resamplesd[k] <- mean(R_sd)+1.96*sd(R_sd)
        lowerval_by_resamplesd[k] <- mean(R_sd)-1.96*sd(R_sd)
      }
      #estimativa da media e IC pelo metodo jacknife
      jack<-numeric(n)
      jack_sd<-numeric(n)
      for(i in 1:n){
        vetor_jack <- vetor[-i]
        jack[i]<- mean(vetor_jack)
       }
      mean_jack <- mean(jack) # para criação do IC da média
      upper_jack <-mean_jack + sqrt((n-1)*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
      lower_jack <-mean_jack - sqrt((n-1)*mean((jack-mean_jack)^2))*1.96
      
      
      #Data frame para apresentar
      boot_jack_presentation <- data.frame(length_resample_boot = B1,boot_estimation=mean_by_resample,
                              boot_upper_lim= upperval_by_resample,boot_lower_lim = lowerval_by_resample,
                              jack_estimation = rep(mean_jack,length(B1)) ,jack_upper_lim=rep(upper_jack,length(B1))
                              , jack_lower_lim =rep(lower_jack,length(B1)))
      
      ################ Arranjando data_frame para plotar com ggplot #####################
      
      # 0 - valor real do parâmetro
      real<- data.frame(length_resample_boot = B1, v=input$mean_norm, nome = 'Real value')
      
      # 1 - valor estimado bootstrap
      d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap')
      
      # 2 - limite superior bootstrap
      d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim')
      
      # 3 - limite inferior bootstrap
      d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim')
      
      # 4 - valor estimado jackknife
      d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation')
      
      # 5 - limite superior jackknife
      d5<- data.frame(length_resample_boot = B1, v= rep(lower_jack,length(B1)), nome = 'jack upper lim')
      
      # 6 - limite inferior jackknife
      d6<- data.frame(length_resample_boot = B1,v =rep(lower_jack,length(B1)), nome = 'jack lower lim')
      
      boot_jack <-rbind.data.frame(real,d1,d2,d3,d4,d5,d6)
      colnames(boot_jack) <- c("length_resample_boot","estimation","nome")
      ##############
      
      #plot
      demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=v,colour=nome)) + geom_point() + geom_line() 
      + ggtitle("Estimation of the mean") + theme(plot.title = element_text(lineheight=.8, face="bold"))
      
      #demo_plot+  
      }else{
      if(input$dist == "gam"){
        
        # >> declaração de variaveis para o ggplot
        mean_by_resample <- numeric(length(B1))    
        upperval_by_resample <- numeric(length(B1))
        lowerval_by_resample <- numeric(length(B1))
        
        mean_by_resample2 <- numeric(length(B1))
        upperval_by_resample2 <- numeric(length(B1))
        lowerval_by_resample2 <- numeric(length(B1))
        vetor <-dist(n,shape=input$gam_shape,rate=input$gam_rate)
        # >> fim de declaração de variáveis para o ggplot
        
        # iniciando bootstrap
        for (k in 1:length(B1)){
          B <- B1[k]   
          R <- numeric(B) #Estoque de rep
          R_2 <- numeric(B)
          #estimativa da media e IC pelo metodo bootstrap
          for (b in 1:B){
            
            #coleta aleatoria de indices
            i <- sample(1:n, size = n, replace = TRUE)
            vetor_boot <- vetor[i] #i vetor de indices
            
            
            R[b] <- fitdistr(vetor_boot, "gamma", start=list(shape=1.1, rate=1))$estimate[1] #estimando shape 
            R_2[b] <- fitdistr(vetor_boot, "gamma", start=list(shape=1.1, rate=1))$estimate[2] #estimando rate
          
            while(is.na(R[b])==TRUE){R[b] <- fitdistr(vetor_boot, "gamma", start=list(shape=1.1, rate=1))$estimate[1] #estimando shape 
            }
            while(is.na(R_2[b])==TRUE){R_2[b] <- fitdistr(vetor_boot, "gamma", start=list(shape=1.1, rate=1))$estimate[2] #estimando rate
            }
            }
          # IC por reamostra
          mean_by_resample[k] <- mean(R) 
          upperval_by_resample[k] <- mean(R)+1.96*sd(R)
          lowerval_by_resample[k] <- mean(R)-1.96*sd(R)
          
          mean_by_resample2[k] <- mean(R_2) 
          upperval_by_resample2[k] <- mean(R_2)+1.96*sd(R_2)
          lowerval_by_resample2[k] <- mean(R_2)-1.96*sd(R_2)
        }
        #estimativa da media e IC pelo metodo jacknife
        jack<-numeric(n)
        jack_2<-numeric(n)
        for(i in 1:n){
          vetor_jack <- vetor[-i]
          jack[i] <- fitdistr(vetor_jack, "gamma", start=list(shape=1, rate=1))$estimate[1] #estimando shape 
          jack_2[i]<- fitdistr(vetor_jack, "gamma", start=list(shape=1, rate=1))$estimate[2] #estimando rate
          
          while(is.na(jack[b])==TRUE){jack[b] <- fitdistr(vetor_boot, "gamma", start=list(shape=1.1, rate=1))$estimate[1] #estimando shape 
          }
          while(is.na(jack_2[b])==TRUE){jack_2[b] <- fitdistr(vetor_boot, "gamma", start=list(shape=1.1, rate=1))$estimate[2] #estimando rate
          }
        }
        mean_jack <- mean(jack) # para criação do IC da média
        mean_jack_2 <- mean(jack_2) # para criação do IC da média
        upper_jack <-mean_jack + sqrt(((n-1)/n)*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
        lower_jack <-mean_jack - sqrt(((n-1)/n)*mean((jack-mean_jack)^2))*1.96
        
        boot_jack <- data.frame(res_boot = B1,n=rep(n,length(B1)),media_boot=mean_by_resample,upper_lim= upperval_by_resample,lower_lim = lowerval_by_resample)
        
        #PLOT
        demo_plot<-ggplot(data=boot_jack, aes(x=res_boot, y=media_boot)) + geom_point() + geom_line() + ggtitle("Estimation of the shape") + theme(plot.title = element_text(lineheight=.8, face="bold"))
        demo_plot+ geom_hline(yintercept=input$gam_shape, linetype=3, color = "blue", size=1) +
        geom_line(aes(res_boot, upper_lim, colour="UL_boot",color="red"), boot_jack) +  
          geom_line(aes(res_boot, lower_lim, colour="LL_boot",color="red"), boot_jack) + 
          geom_hline(yintercept=mean_jack, color = "grey",colour="jack") +
          geom_hline(yintercept=upper_jack,linetype=4, color = "grey",colour="jack UL",size=1) +
          geom_hline(yintercept=lower_jack,linetype=4, color = "grey",colour="jack LM",size=1)
        
        }else{
        if(input$dist == "lnorm"){
          vetor <-dist(n,meanlog=input$mean_log,sdlog=input$sd_log)
          
          # >> declaração de variaveis para o ggplot
          mean_by_resample <- numeric(length(B1))    
          upperval_by_resample <- numeric(length(B1))
          lowerval_by_resample <- numeric(length(B1))
          
          mean_by_resample2 <- numeric(length(B1))
          upperval_by_resample2 <- numeric(length(B1))
          lowerval_by_resample2 <- numeric(length(B1))
          # >> fim de declaração de variáveis para o ggplot
          
          # iniciando bootstrap
          for (k in 1:length(B1)){
            B <- B1[k]   
            R <- numeric(B) #Estoque de rep
            R_2 <- numeric(B)
            #estimativa da media e IC pelo metodo bootstrap
            for (b in 1:B){
              
              #coleta aleatoria de indices
              i <- sample(1:n, size = n, replace = TRUE)
              vetor_boot <- vetor[i] #i vetor de indices
              
              
              R[b] <- mean(log(vetor_boot))  
              R_2[b] <- sd(log(vetor_boot)) #estimando rate
              
            }
            # IC por reamostra
            mean_by_resample[k] <- mean(R) 
            upperval_by_resample[k] <- mean(R)+1.96*sd(R)
            lowerval_by_resample[k] <- mean(R)-1.96*sd(R)
            
            mean_by_resample2[k] <- mean(R_2) 
            upperval_by_resample2[k] <- mean(R_2)+1.96*sd(R_2)
            lowerval_by_resample2[k] <- mean(R_2)-1.96*sd(R_2)
          }
          #estimativa da media e IC pelo metodo jacknife
          jack<-numeric(n)
          jack_2<-numeric(n)
          for(i in 1:n){
            vetor_jack <- vetor[-i]
            jack[i] <- mean(log(vetor_jack)) #estimando log mean 
            jack_2[i]<- sd(log(vetor_jack))  #estimando log sd
              
          }
          mean_jack <- mean(jack) # para criação do IC da média
          mean_jack_2 <- mean(jack_2) # para criação do IC da média
          upper_jack <-mean_jack + sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
          lower_jack <-mean_jack - sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96
          
          boot_jack <- data.frame(res_boot = B1,n=rep(n,length(B1)),media_boot=mean_by_resample,upper_lim= upperval_by_resample,lower_lim = lowerval_by_resample)
          
          #PLOT
          demo_plot<-ggplot(data=boot_jack, aes(x=res_boot, y=media_boot)) + geom_point() + geom_line() + ggtitle("Estimation of the Log mean") + theme(plot.title = element_text(lineheight=.8, face="bold"))
          demo_plot+ geom_hline(yintercept=input$mean_log, linetype=3, color = "blue", size=1) +
            geom_line(aes(res_boot, upper_lim, colour="UL_boot",color="red"), boot_jack) +  
            geom_line(aes(res_boot, lower_lim, colour="LL_boot",color="red"), boot_jack) + 
            geom_hline(yintercept=mean_jack, color = "grey",colour="jack") +
            geom_hline(yintercept=upper_jack,linetype=4, color = "grey",colour="jack UL",size=1) +
            geom_hline(yintercept=lower_jack,linetype=4, color = "grey",colour="jack LM",size=1)
        }else{
          if(input$dist == "exp"){
            vetor <-dist(n,input$rate_exp)
            
            # >> declaração de variaveis para o ggplot
            mean_by_resample <- numeric(length(B1))    
            upperval_by_resample <- numeric(length(B1))
            lowerval_by_resample <- numeric(length(B1))
            
            # >> fim de declaração de variáveis para o ggplot
            
            # iniciando bootstrap
            for (k in 1:length(B1)){
              B <- B1[k]   
              R <- numeric(B) #Estoque de rep
              #estimativa da media e IC pelo metodo bootstrap
              for (b in 1:B){
                
                #coleta aleatoria de indices
                i <- sample(1:n, size = n, replace = TRUE)
                vetor_boot <- vetor[i] #i vetor de indices
                
                
                R[b] <- 1/mean(vetor_boot)  
                
              }
              # IC por reamostra
              mean_by_resample[k] <- mean(R) 
              upperval_by_resample[k] <- mean(R)+1.96*sd(R)
              lowerval_by_resample[k] <- mean(R)-1.96*sd(R)
              }
            #estimativa da media e IC pelo metodo jacknife
            jack<-numeric(n)
            for(i in 1:n){
              vetor_jack <- vetor[-i]
              jack[i] <- 1/mean(vetor_jack) #estimando log mean 
              
            }
            mean_jack <- mean(jack) # para criação do IC da média
            upper_jack <-mean_jack + sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
            lower_jack <-mean_jack - sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96
            
            boot_jack <- data.frame(res_boot = B1,n=rep(n,length(B1)),media_boot=mean_by_resample,upper_lim= upperval_by_resample,lower_lim = lowerval_by_resample)
            
            #PLOT
            demo_plot<-ggplot(data=boot_jack, aes(x=res_boot, y=media_boot)) + geom_point() + geom_line() + ggtitle("Estimation of the Log mean") + theme(plot.title = element_text(lineheight=.8, face="bold"))
            demo_plot+ geom_hline(yintercept=input$rate_exp, linetype=3, color = "blue", size=1) +
              geom_line(aes(res_boot, upper_lim, colour="UL_boot",color="red"), boot_jack) +  
              geom_line(aes(res_boot, lower_lim, colour="LL_boot",color="red"), boot_jack) + 
              geom_hline(yintercept=mean_jack, color = "grey",colour="jack") +
              geom_hline(yintercept=upper_jack,linetype=4, color = "grey",colour="jack UL",size=1) +
              geom_hline(yintercept=lower_jack,linetype=4, color = "grey",colour="jack LM",size=1)
          }else{
            vetor <-dist(n,shape=input$shape_wei,scale=input$scale_wei)
           
          } } } }
    
  })
  
  #plot 2
  
  output$plot2 <- renderPlot({ set.seed(1)
    dist <- data()
    
    # setando bootstrap
    
    B1 <- seq(50,500,50) #Numero de replicas
    n <- input$amostra #tamanho da amostra (5 a 100)
    #print(n)
    #Setar parametros de acordo com a escolha do usuario
    if(input$dist == "norm"){
      mean_by_resample <- numeric(length(B1))
      upperval_by_resample <- numeric(length(B1))
      lowerval_by_resample <- numeric(length(B1))
      
      mean_by_resamplesd <- numeric(length(B1))
      upperval_by_resamplesd <- numeric(length(B1))
      lowerval_by_resamplesd <- numeric(length(B1))
      vetor <-dist(n,input$mean_norm,input$sd_norm)
      
      # iniciando bootstrap
      for (k in 1:length(B1)){
        B <- B1[k]   
        R <- numeric(B) #Estoque de rep
        R_sd <- numeric(B)
        #estimativa da media e IC pelo metodo bootstrap
        for (b in 1:B){
          
          #coleta aleatoria de indices
          i <- sample(1:n, size = n, replace = TRUE)
          vetor_boot <- vetor[i] #i vetor de indices
          R_sd[b] <- sd(vetor_boot) #estimando o desvio padrão da normal
        }
        # IC por reamostra
        mean_by_resample[k] <- mean(R_sd) 
        upperval_by_resample[k] <- mean(R_sd)+1.96*sd(R_sd)
        lowerval_by_resample[k] <- mean(R_sd)-1.96*sd(R_sd)
        }
      #estimativa da media e IC pelo metodo jacknife
      
      jack_sd<-numeric(n)
      for(i in 1:n){
        vetor_jack <- vetor[-i]
        jack_sd[i]<- sd(vetor_jack)
      }
      mean_jack <- mean(jack_sd) # para criação do IC da média
      upper_jack <-mean_jack + sqrt(((n-1))*mean((jack_sd-mean_jack)^2))*1.96 # para criação do IC da média
      lower_jack <-mean_jack - sqrt(((n-1))*mean((jack_sd-mean_jack)^2))*1.96
      
      boot_jack <- data.frame(resample_boot = B1,n=rep(n,length(B1)),estimation=mean_by_resample,upper_lim= upperval_by_resample,lower_lim = lowerval_by_resample,sd_boot=mean_by_resample)
      demo_plot<-ggplot(data=boot_jack, aes(x=resample_boot, y=estimation)) + geom_point() + geom_line() + ggtitle("Estimation of the standart deviation") + theme(plot.title = element_text(lineheight=.8, face="bold"))
      demo_plot+ geom_hline(yintercept=input$sd_norm, linetype=3, color = "blue", size=1) +
        geom_line(aes(resample_boot, upper_lim, colour="IC bootstrap"), boot_jack) +  
        geom_line(aes(resample_boot, lower_lim, colour="IC bootstrap"), boot_jack) + 
        geom_hline(yintercept=mean_jack, color = "grey",colour="jack") +
        geom_hline(yintercept=upper_jack,linetype=4, color = "grey",colour="jack UL",size=1) +
        geom_hline(yintercept=lower_jack,linetype=4, color = "grey",colour="jack LM",size=1)
      
    }})
  output$dis <- renderDataTable({})
})
