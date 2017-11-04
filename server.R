library(shiny)
library(ggplot2)
library(MASS)
library(dplyr)

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
  })   # reage a seleção da distribuição
  
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
          }
          else{
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
    
    #Setar parametros de acordo com a escolha do usuario
    if(input$dist == "norm"){
      req(input$mean_norm,input$sd_norm)
      
      mean_by_resample <- numeric(length(B1))
      upperval_by_resample <- numeric(length(B1))
      lowerval_by_resample <- numeric(length(B1))
      
     vetor <-dist(n,input$mean_norm,input$sd_norm)
     
       # iniciando bootstrap
      for (k in 1:length(B1)){
        B <- B1[k]   
        R <- numeric(B) #Estoque de rep
         #estimativa da media e IC pelo metodo bootstrap
        for (b in 1:B){
          
          #coleta aleatoria de indices
          i <- sample(1:n, size = n, replace = TRUE)
          vetor_boot <- vetor[i] #i vetor de indices
          R[b] <- mean(vetor_boot) #estimando a média da normal
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
        jack[i]<- mean(vetor_jack)
       }
      mean_jack <- mean(jack) # para criação do IC da média
      upper_jack <-mean_jack + sqrt((n-1)*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
      lower_jack <-mean_jack - sqrt((n-1)*mean((jack-mean_jack)^2))*1.96
      
      
      
      ################ Arranjando data_frame para plotar com ggplot #####################
      
      # 0 - valor real do parâmetro
      real<- data.frame(length_resample_boot = B1, v=input$mean_norm, nome = 'Real value', stringsAsFactors = FALSE)
      
      # 1 - valor estimado bootstrap
      d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap', stringsAsFactors = FALSE)
      
      # 2 - limite superior bootstrap
      d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim', stringsAsFactors = FALSE)
      
      # 3 - limite inferior bootstrap
      d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim', stringsAsFactors = FALSE)
      
      # 4 - valor estimado jackknife
      d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
      
      # 5 - limite superior jackknife
      d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
      
      # 6 - limite inferior jackknife
      d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
      
      boot_jack <- real %>% 
        bind_rows(d1) %>% 
        bind_rows(d2) %>%
        bind_rows(d3) %>%
        bind_rows(d4) %>%
        bind_rows(d5) %>%
        bind_rows(d6) %>%
        rename(estimation = v)
        #rbind.data.frame(real,d1,d2,d3,d4,d5,d6)
      #colnames(boot_jack) <- c("length_resample_boot","estimation","nome")
      ##############
      #print(head(boot_jack))
      #plot
      demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the mean") + theme(plot.title = element_text(lineheight=.8, face="bold"))
      return(demo_plot)
      #demo_plot+  
      }else{  
        if(input$dist == "gam"){
          req(input$gam_shape,input$gam_rate)
          
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
            
            
            R[b] <- fitdistr(vetor_boot, "gamma")$estimate[1] #estimando shape 
           
            while(is.na(R[b])==TRUE){R[b] <- fitdistr(vetor_boot, "gamma")$estimate[1] #estimando shape 
            }
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
          jack[i] <- fitdistr(vetor_jack, "gamma")$estimate[1] #estimando shape 
          
          while(is.na(jack[i])==TRUE){jack[i] <- fitdistr(vetor_jack, "gamma")$estimate[1] #estimando shape 
          }
        }
        mean_jack <- mean(jack) # para criação do IC da média
        upper_jack <-mean_jack + sqrt(((n-1)/n)*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
        lower_jack <-mean_jack - sqrt(((n-1)/n)*mean((jack-mean_jack)^2))*1.96
        
        ################ Arranjando data_frame para plotar com ggplot #####################
        
        # 0 - valor real do parâmetro
        real<- data.frame(length_resample_boot = B1, v=input$gam_shape, nome = 'Real value', stringsAsFactors = FALSE)
        
        # 1 - valor estimado bootstrap
        d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap', stringsAsFactors = FALSE)
        
        # 2 - limite superior bootstrap
        d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim', stringsAsFactors = FALSE)
        
        # 3 - limite inferior bootstrap
        d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim', stringsAsFactors = FALSE)
        
        # 4 - valor estimado jackknife
        d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
        
        # 5 - limite superior jackknife
        d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
        
        # 6 - limite inferior jackknife
        d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
        
        boot_jack <- real %>% 
          bind_rows(d1) %>% 
          bind_rows(d2) %>%
          bind_rows(d3) %>%
          bind_rows(d4) %>%
          bind_rows(d5) %>%
          bind_rows(d6) %>%
          rename(estimation = v)
       
        demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the shape") + theme(plot.title = element_text(lineheight=.8, face="bold"))
        return(demo_plot)
        
        
        }else{
        if(input$dist == "lnorm"){
          req(input$mean_log,sdlog=input$sd_log)
          vetor <-dist(n,meanlog=input$mean_log,sdlog=input$sd_log)
          
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
              
              
              R[b] <- mean(log(vetor_boot))  
              
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
            jack[i] <- mean(log(vetor_jack)) #estimando log mean 
              
          }
          mean_jack <- mean(jack) # para criação do IC da média
          upper_jack <-mean_jack + sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
          lower_jack <-mean_jack - sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96
          ################ Arranjando data_frame para plotar com ggplot #####################
          
          # 0 - valor real do parâmetro
          real<- data.frame(length_resample_boot = B1, v=input$mean_log, nome = 'Real value', stringsAsFactors = FALSE)
          
          # 1 - valor estimado bootstrap
          d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap', stringsAsFactors = FALSE)
          
          # 2 - limite superior bootstrap
          d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim', stringsAsFactors = FALSE)
          
          # 3 - limite inferior bootstrap
          d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim', stringsAsFactors = FALSE)
          
          # 4 - valor estimado jackknife
          d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
          
          # 5 - limite superior jackknife
          d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
          
          # 6 - limite inferior jackknife
          d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
          
          boot_jack <- real %>% 
            bind_rows(d1) %>% 
            bind_rows(d2) %>%
            bind_rows(d3) %>%
            bind_rows(d4) %>%
            bind_rows(d5) %>%
            bind_rows(d6) %>%
            rename(estimation = v)
          
          demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the mean") + theme(plot.title = element_text(lineheight=.8, face="bold"))
          return(demo_plot)
          
        }else{
          if(input$dist == "exp"){
            req(input$rate_exp)
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
            
            ################ Arranjando data_frame para plotar com ggplot #####################
            
            # 0 - valor real do parâmetro
            real<- data.frame(length_resample_boot = B1, v=input$rate_exp, nome = 'Real value', stringsAsFactors = FALSE)
            
            # 1 - valor estimado bootstrap
            d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap', stringsAsFactors = FALSE)
            
            # 2 - limite superior bootstrap
            d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim', stringsAsFactors = FALSE)
            
            # 3 - limite inferior bootstrap
            d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim', stringsAsFactors = FALSE)
            
            # 4 - valor estimado jackknife
            d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
            
            # 5 - limite superior jackknife
            d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
            
            # 6 - limite inferior jackknife
            d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
            
            boot_jack <- real %>% 
              bind_rows(d1) %>% 
              bind_rows(d2) %>%
              bind_rows(d3) %>%
              bind_rows(d4) %>%
              bind_rows(d5) %>%
              bind_rows(d6) %>%
              rename(estimation = v)
            
            demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the rate") + theme(plot.title = element_text(lineheight=.8, face="bold"))
            return(demo_plot)
                  }
          else{
            req(input$shape_wei,input$scale_wei)
            
            vetor <-dist(n,shape=input$shape_wei,scale=input$scale_wei)
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
                
                
                R[b] <-fitdistr(vetor_boot, "weibull")$estimate[1] #estimando shape
                while(is.na(R[b])==TRUE){R[b] <- fitdistr(vetor_boot, "weibull")$estimate[1] #estimando shape 
                }
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
              jack[i] <-  fitdistr(vetor_jack, "weibull")$estimate[1] #estimando shape#estimando log mean 
              while(is.na(jack[i])==TRUE){R[b] <- fitdistr(vetor_jack, "weibull")$estimate[1] #estimando shape 
              }
            }
            mean_jack <- mean(jack) # para criação do IC da média
            upper_jack <-mean_jack + sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
            lower_jack <-mean_jack - sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96
            
            ################ Arranjando data_frame para plotar com ggplot #####################
            
            # 0 - valor real do parâmetro
            real<- data.frame(length_resample_boot = B1, v=input$shape_wei, nome = 'Real value', stringsAsFactors = FALSE)
            
            # 1 - valor estimado bootstrap
            d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap', stringsAsFactors = FALSE)
            
            # 2 - limite superior bootstrap
            d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim', stringsAsFactors = FALSE)
            
            # 3 - limite inferior bootstrap
            d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim', stringsAsFactors = FALSE)
            
            # 4 - valor estimado jackknife
            d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
            
            # 5 - limite superior jackknife
            d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
            
            # 6 - limite inferior jackknife
            d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
            
            boot_jack <- real %>% 
              bind_rows(d1) %>% 
              bind_rows(d2) %>%
              bind_rows(d3) %>%
              bind_rows(d4) %>%
              bind_rows(d5) %>%
              bind_rows(d6) %>%
              rename(estimation = v)
            
            demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the shape") + theme(plot.title = element_text(lineheight=.8, face="bold"))
            return(demo_plot)
            
          } } } }  #weibull
    })
  
  #PLOT 2 
  # PARA NORMAL ESTE PLOT MOSTRA O DESVIO PADRÃO
  # PARA A GAMMA ESTE PLOT MOSTRA O RATE
  # PARA A LNORM ESTE PLOT MOSTRA O LOG DESVIO PADRÃO
  # PARA A WEIBULL ESTE PLOT MOSTRA O RATE
  
  output$plot2 <- renderPlot({ 
    req(input$amostra, input$mean_norm,input$sd_norm)
    set.seed(1)
    dist <- data()
    
    # setando bootstrap
    
    B1 <- seq(50,500,50) #Numero de replicas
    n <- input$amostra #tamanho da amostra (5 a 100)
    #print(n)
    #Setar parametros de acordo com a escolha do usuario
    if(input$dist == "norm"){
      req(input$sd_norm)
      mean_by_resamplesd <- numeric(length(B1))
      upperval_by_resamplesd <- numeric(length(B1))
      lowerval_by_resamplesd <- numeric(length(B1))
      vetor <-dist(n,input$mean_norm,input$sd_norm)
      
      # iniciando bootstrap
      for (k in 1:length(B1)){
        B <- B1[k]   
        R_sd <- numeric(B)
        #estimativa da media e IC pelo metodo bootstrap
        for (b in 1:B){
          
          #coleta aleatoria de indices
          i <- sample(1:n, size = n, replace = TRUE)
          vetor_boot <- vetor[i] #i vetor de indices
          R_sd[b] <- sd(vetor_boot) #estimando o desvio padrão da normal
        }
        # IC por reamostra
        mean_by_resamplesd[k] <- mean(R_sd) 
        upperval_by_resamplesd[k] <- mean(R_sd)+1.96*sd(R_sd)
        lowerval_by_resamplesd[k] <- mean(R_sd)-1.96*sd(R_sd)
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
      
      ################ Arranjando data_frame para plotar com ggplot #####################
      
      # 0 - valor real do parâmetro
      real<- data.frame(length_resample_boot = B1, v=input$sd_norm, nome = 'Real value', stringsAsFactors = FALSE)
      
      # 1 - valor estimado bootstrap
      d1<- data.frame(length_resample_boot = B1, v=mean_by_resamplesd, nome = 'bootstrap', stringsAsFactors = FALSE)
      
      # 2 - limite superior bootstrap
      d2<- data.frame(length_resample_boot = B1, v= upperval_by_resamplesd, nome = 'boot upper lim', stringsAsFactors = FALSE)
      
      # 3 - limite inferior bootstrap
      d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resamplesd, nome = 'boot lower lim', stringsAsFactors = FALSE)
      
      # 4 - valor estimado jackknife
      d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
      
      # 5 - limite superior jackknife
      d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
      
      # 6 - limite inferior jackknife
      d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
      
      boot_jack <- real %>% 
        bind_rows(d1) %>% 
        bind_rows(d2) %>%
        bind_rows(d3) %>%
        bind_rows(d4) %>%
        bind_rows(d5) %>%
        bind_rows(d6) %>%
        rename(estimation = v)
      demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the standart deviation") + theme(plot.title = element_text(lineheight=.8, face="bold"))
      return(demo_plot)
      
    }else{
      if(input$dist == "gam"){
      req(input$gam_shape,input$gam_rate)
      
      # >> declaração de variaveis para o ggplot
      mean_by_resample2 <- numeric(length(B1))    
      upperval_by_resample2 <- numeric(length(B1))
      lowerval_by_resample2<- numeric(length(B1))
      
       vetor2 <-dist(n,shape=input$gam_shape,rate=input$gam_rate)
      # >> fim de declaração de variáveis para o ggplot
      
      # iniciando bootstrap
      for (k in 1:length(B1)){
        B <- B1[k]   
        R_2 <- numeric(B) # estoque de rep
        #estimativa da media e IC pelo metodo bootstrap
        for (b in 1:B){
          
          #coleta aleatoria de indices
          i <- sample(1:n, size = n, replace = TRUE)
          vetor_boot2 <- vetor2[i] #i vetor de indices
          
          
          R_2[b] <- fitdistr(vetor_boot2, "gamma")$estimate[2] #estimando shape 
          
          while(is.na(R_2[b])==TRUE){R_2[b] <- fitdistr(vetor_boot2, "gamma")$estimate[2] #estimando rate 
          }
        }
        # IC por reamostra
        mean_by_resample2[k] <- mean(R_2) 
        upperval_by_resample2[k] <- mean(R_2)+1.96*sd(R_2)
        lowerval_by_resample2[k] <- mean(R_2)-1.96*sd(R_2)
        
      }
      #estimativa da media e IC pelo metodo jacknife
      jack2<-numeric(n)
      for(i in 1:n){
        vetor_jack2 <- vetor2[-i]
        jack2[i] <- fitdistr(vetor_jack2, "gamma")$estimate[2] #estimando shape 
        
        while(is.na(jack2[i])==TRUE){jack2[i] <- fitdistr(vetor_jack2, "gamma")$estimate[2] #estimando shape 
        }
      }
      mean_jack2 <- mean(jack2) # para criação do IC da média
      upper_jack2 <-mean_jack2 + sqrt(((n-1)/n)*mean((jack2-mean_jack2)^2))*1.96 # para criação do IC da média
      lower_jack2 <-mean_jack2 - sqrt(((n-1)/n)*mean((jack2-mean_jack2)^2))*1.96
      
      ################ Arranjando data_frame para plotar com ggplot #####################
      
      # 0 - valor real do parâmetro
      real<- data.frame(length_resample_boot = B1, v=input$gam_rate, nome = 'Real value', stringsAsFactors = FALSE)
      
      # 1 - valor estimado bootstrap
      d1<- data.frame(length_resample_boot = B1, v=mean_by_resample2, nome = 'bootstrap', stringsAsFactors = FALSE)
      
      # 2 - limite superior bootstrap
      d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample2, nome = 'boot upper lim', stringsAsFactors = FALSE)
      
      # 3 - limite inferior bootstrap
      d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample2, nome = 'boot lower lim', stringsAsFactors = FALSE)
      
      # 4 - valor estimado jackknife
      d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack2,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
      
      # 5 - limite superior jackknife
      d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack2,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
      
      # 6 - limite inferior jackknife
      d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack2,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
      
      boot_jack <- real %>% 
        bind_rows(d1) %>% 
        bind_rows(d2) %>%
        bind_rows(d3) %>%
        bind_rows(d4) %>%
        bind_rows(d5) %>%
        bind_rows(d6) %>%
        rename(estimation = v)
      
      demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the rate") + theme(plot.title = element_text(lineheight=.8, face="bold"))
      return(demo_plot)
      
      
    }else{
      if(input$dist == "lnorm"){
        req(input$mean_log,sdlog=input$sd_log)
        vetor <-dist(n,meanlog=input$mean_log,sdlog=input$sd_log)
        
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
            
            
            R[b] <- sd(log(vetor_boot))  
            
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
          jack[i] <- sd(log(vetor_jack)) #estimando log mean 
          
        }
        mean_jack <- mean(jack) # para criação do IC da média
        upper_jack <-mean_jack + sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
        lower_jack <-mean_jack - sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96
        ################ Arranjando data_frame para plotar com ggplot #####################
        
        # 0 - valor real do parâmetro
        real<- data.frame(length_resample_boot = B1, v=input$sd_log, nome = 'Real value', stringsAsFactors = FALSE)
        
        # 1 - valor estimado bootstrap
        d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap', stringsAsFactors = FALSE)
        
        # 2 - limite superior bootstrap
        d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim', stringsAsFactors = FALSE)
        
        # 3 - limite inferior bootstrap
        d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim', stringsAsFactors = FALSE)
        
        # 4 - valor estimado jackknife
        d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
        
        # 5 - limite superior jackknife
        d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
        
        # 6 - limite inferior jackknife
        d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
        
        boot_jack <- real %>% 
          bind_rows(d1) %>% 
          bind_rows(d2) %>%
          bind_rows(d3) %>%
          bind_rows(d4) %>%
          bind_rows(d5) %>%
          bind_rows(d6) %>%
          rename(estimation = v)
        
        demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the log standart deviation") + theme(plot.title = element_text(lineheight=.8, face="bold"))
        return(demo_plot)
        
      }else{
       if(input$dist == "wei"){
         
         req(input$shape_wei,input$scale_wei)
         vetor <-dist(n,shape=input$shape_wei,scale=input$scale_wei)
         
         req(input$shape_wei,input$scale_wei)
         
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
             
             
             R[b] <-fitdistr(vetor_boot, "weibull")$estimate[2] #estimando shape
             while(is.na(R[b])==TRUE){R[b] <- fitdistr(vetor_boot, "weibull")$estimate[2] #estimando shape 
             }
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
           jack[i] <-  fitdistr(vetor_jack, "weibull")$estimate[2] #estimando shape#estimando log mean 
           while(is.na(jack[i])==TRUE){R[b] <- fitdistr(vetor_jack, "weibull")$estimate[2] #estimando shape 
           }
         }
         mean_jack <- mean(jack) # para criação do IC da média
         upper_jack <-mean_jack + sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96 # para criação do IC da média
         lower_jack <-mean_jack - sqrt(((n-1))*mean((jack-mean_jack)^2))*1.96
         
         ################ Arranjando data_frame para plotar com ggplot #####################
         
         # 0 - valor real do parâmetro
         real<- data.frame(length_resample_boot = B1, v=input$scale_wei, nome = 'Real value', stringsAsFactors = FALSE)
         
         # 1 - valor estimado bootstrap
         d1<- data.frame(length_resample_boot = B1, v=mean_by_resample, nome = 'bootstrap', stringsAsFactors = FALSE)
         
         # 2 - limite superior bootstrap
         d2<- data.frame(length_resample_boot = B1, v= upperval_by_resample, nome = 'boot upper lim', stringsAsFactors = FALSE)
         
         # 3 - limite inferior bootstrap
         d3<- data.frame(length_resample_boot = B1, v= lowerval_by_resample, nome = 'boot lower lim', stringsAsFactors = FALSE)
         
         # 4 - valor estimado jackknife
         d4<- data.frame(length_resample_boot = B1, v=rep(mean_jack,length(B1)), nome = 'jack estimation', stringsAsFactors = FALSE)
         
         # 5 - limite superior jackknife
         d5<- data.frame(length_resample_boot = B1, v= rep(upper_jack,length(B1)), nome = 'jack upper lim', stringsAsFactors = FALSE)
         
         # 6 - limite inferior jackknife
         d6<- data.frame(length_resample_boot = B1,v = rep(lower_jack,length(B1)), nome = 'jack lower lim', stringsAsFactors = FALSE)
         
         boot_jack <- real %>% 
           bind_rows(d1) %>% 
           bind_rows(d2) %>%
           bind_rows(d3) %>%
           bind_rows(d4) %>%
           bind_rows(d5) %>%
           bind_rows(d6) %>%
           rename(estimation = v)
         
         demo_plot<-ggplot(data=boot_jack, aes(x=length_resample_boot, y=estimation, color= nome)) + geom_point() + geom_line() + ggtitle("Estimation of the scale") + theme(plot.title = element_text(lineheight=.8, face="bold"))
         return(demo_plot)
         
         
       } 
        
      }
      }
      }
      })
  
#  output$table <- renderDataTable({table(rnorm(input$amostra))})
})
