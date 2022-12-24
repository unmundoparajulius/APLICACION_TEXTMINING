

# Definimos la logica del servidor
shinyServer(function(input, output) {
    
  ###################### CARGA DE DATOS ##########################  
    output$contenido <- renderPlot({
      req(input$file)
      
      tryCatch({
        df <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep)
        ####################### CORPUS #######################
        #corpus <- Corpus(VectorSource(df))
        
        #d <- tm_map(corpus, tolower)
      
        #d <- tm_map(d, stripWhitespace)
        
        #d <- tm_map(d, removePunctuation)
        
        #d <- tm_map(d, removeNumbers)
        
        #d <- tm_map(d, removeWords, stopwords("spanish"))
        
        #tdm <- TermDocumentMatrix(d)
        #findFreqTerms(tdm, lowfreq = 20)
        
        #Matriz
        #m <- as.matrix(tdm)
        #v <- sort(rowSums(m),decreasing = TRUE)
        #df1 <- data.frame(word = names(v), n=v)
        ##################### LIMPIEZA #########################
        
        stop_words_es <- stopwords(language = "es", source = "snowball")
        
        df_token <- unnest_tokens(tbl=df,
                                         output = "word",
                                         input = "V1",
                                         token = "words") %>%
          filter(!(word %in% stop_words_es)) 
        
        ########################## DICCIONARIO DE SENTIMIENTOS ###################
        sentimientosyemociones <- read.csv("dataset/nrc_03.csv", sep = ";",
                                           fileEncoding = "latin1")
        dic_nrc <- df_token %>% inner_join(sentimientosyemociones)
        
        dic_nrc %>% count(word,Sentimientos, sort = TRUE)
        
        
        ######################### SENTIMIENTOS ###############################
        output$sent <- renderPlot({
          grafica_sent<-dic_nrc %>%
            filter(Sentimientos!="alegría" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            #ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = TRUE) +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "cantidad de Sentimientos")
          return(grafica_sent)
        })
        
        ############################## EMOCIONES ######################
        ###### Grafica de Alegría
        output$alegria <- renderPlot({
          grafica_alegria<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!="negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            #ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "orange") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Alegría")
          return(grafica_alegria)
        })
        
        ###### Gráfica de Anticipación
        output$anticipacion <- renderPlot({
          grafica_anticipacion<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="alegría" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "orange") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Anticipación")
          return(grafica_anticipacion)
        })
        
        ###### Gráfica de Confianza
        output$confianza <- renderPlot({
          grafica_confianza<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="alegría" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "dark green") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Confianza")
          return(grafica_confianza)
        })
        
        ##### Gráfica de desagrado
        output$desagrado <- renderPlot({
          grafica_desagrado<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="alegría" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "purple") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+ 
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Desagrado")
          return(grafica_desagrado)
        })
        
        ##### Gráfica de ira
        output$ira <- renderPlot({
          grafica_ira<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="alegría" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "red") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Ira")
          return(grafica_ira)
        })
        
        ##### Gráfica de miedo
        output$miedo <- renderPlot({
          grafica_miedo<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="alegría" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "dark green") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Miedo")
          return(grafica_miedo)
        })
        
        ##### Gráfica de Sorpresa
        output$sorpresa <- renderPlot({
          grafica_sorpresa<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="alegría" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "sky blue") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Sorpresa")
          return(grafica_sorpresa)
        })
        
        ##### Gráfica de Tristeza
        output$tristeza <- renderPlot({
          grafica_tristeza<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="alegría"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "blue") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de TRisteza")
          return(grafica_tristeza)
        })
        
        ####################### NUBE DE PALABRAS #########################
        ####### Gráfica en vista general
        output$cloud <- renderPlot({
          grafica_cloud<- dic_nrc %>%
            count(word) %>%
            with(wordcloud(words = word, freq = n, scale = c(2.5,0.5),
                           max.words=50, random.order=FALSE, rot.per=0.3, 
                           colors=brewer.pal(8, "Dark2")))
          return(grafica_cloud)
        })
        
        ####### NP con sentimientos positivos
        output$npp <- renderPlot({
          grafica_sp<-dic_nrc %>%
            count(word,Sentimientos) %>%            
            filter(Sentimientos =="positivo") %>%   
            with(wordcloud(words=word,             
                           freq = n,                 
                           max.words = 100,        
                           scale = c(2.8,1),        
                           rot.per = 0.3,         
                           random.order = FALSE,   
                           colors=brewer.pal(6,"Dark2")))
          
          return(grafica_sp)
        })
        
        ####### NP con sentimientos negativos
        output$npn <- renderPlot({
          grafica_npn<-dic_nrc%>%
            count(word,Sentimientos) %>%            
            filter(Sentimientos =="negativo") %>%   
            with(wordcloud(words=word,             
                           freq=n,                 
                           max.words = 100,        
                           scale = c(2.8,0.5),        
                           rot.per = 0.3,         
                           random.order = FALSE,   
                           colors=brewer.pal(8,"Dark2")))
          return(grafica_npn)
        })
        
        #################### INDICADORES AUTOREGULACION ############
        output$tabla_01 <- renderTable({
          tabla_sent <- count(dic_nrc,Sentimientos)
          return(tabla_sent)
        })
        #################### CANTIDAD DE SENTIMIENTOS ################
        output$salida_01 <- renderText({
          cant_sent <- length(dic_nrc$Sentimientos)
          return(cant_sent)
        })
        ######## CANTIDAD DE PALABRAS A TRABAJAR AUTORREGULACIÓN ##############
        output$salida_02 <- renderText({
          cant_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          cant_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          cant_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          cant_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_regulacion <- nrow(cant_alegria)+ nrow(cant_confianza)+nrow(cant_miedo)+nrow(cant_tristeza)
          return(suma_regulacion)
        })
        ################## INDICADOR ALEGRIA ###################
        output$hap <- renderPlot({
          graf_ale<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!="negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            #ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "orange") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Alegría")
          return(graf_ale)
        })
        ################## INDICADOR CONFIANZA ######################
        output$anza <- renderPlot({
          graf_conf<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="alegría" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "dark green") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Confianza")
          return(graf_conf)
        })
        
        ####################### EMOCIÓN POSITIVA ######################
        #------------------ AMOR --------------#
        output$sal_alegria <- renderText({
          #----------- Filtrado --------------
          alexis <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          return(nrow(alexis))
        })
        output$sal_confianza <- renderText({
          #--------------- Filtrado ------------#
          confa <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          return(nrow(confa))
        })
        output$sumar_ac <- renderText({
          #----------- Suma de emociones Alegría y Confianza -------#
          alexis <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          
          confa <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          
          suma_01 <- nrow(alexis) + nrow(confa) 
          return(suma_01)
        })
        output$total_amor <- renderText({
          tabla_sent <- count(dic_nrc,Sentimientos)
          #cant_sent <- length(dic_nrc$Sentimientos)
          cant_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          cant_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          cant_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          cant_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_regulacion <- nrow(cant_alegria)+ nrow(cant_confianza)+nrow(cant_miedo)+nrow(cant_tristeza)
          
          alexis <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          
          confa <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          
          suma_01 <- nrow(alexis) + nrow(confa)
          
          ver_resul <- round((suma_01/suma_regulacion),4)*100
          return(ver_resul)
        })
        
        ##################### EMOCIONES NEGATIVAS #################
        ######################### INDICADOR MIEDO #################
        output$ind_mied <- renderPlot({
          graf_miedo<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="alegría" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "dark green") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Miedo")
          return(graf_miedo)
        })
        ########################## INDICADOR TRISTEZA ###############
        output$ind_tristeza <- renderPlot({
          graf_tristeza<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="alegría"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "blue") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de TRisteza")
          return(graf_tristeza)
        })
        #################### DESESPERACION ########################
        output$salida_mied <- renderText({
          #----------- Filtrado --------------
          emo_mied <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          return(nrow(emo_mied))
        })
        output$salida_tristeza <- renderText({
          #--------------- Filtrado ------------#
          emo_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          return(nrow(emo_tristeza))
        })
        output$sum_mt <- renderText({
          emo_mied <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          #----------- Suma de emociones Miedo y Anticipacion -------#
          suma_desesperacion <- nrow(emo_mied) + nrow(emo_tristeza)
          return(suma_desesperacion)
        })
        output$result_desesperacion <- renderText({
          tabla_sent <- count(dic_nrc,Sentimientos)
          #cant_sent <- length(dic_nrc$Sentimientos)
          cant_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          cant_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          cant_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          cant_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_regulacion <- nrow(cant_alegria)+ nrow(cant_confianza)+nrow(cant_miedo)+nrow(cant_tristeza)
          
          emo_mied <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_desesperacion <- nrow(emo_mied) + nrow(emo_tristeza)
          
          res_desesperacion <- round((suma_desesperacion/suma_regulacion),4)*100
          return(res_desesperacion)
        })
        ######################## CUADRO RESUMEN ######################
        output$tbl_res_01 <- renderTable({
          #RESUMENES
          tabla_sent <- count(dic_nrc,Sentimientos)
          #cant_sent <- length(dic_nrc$Sentimientos)
          cant_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          cant_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          cant_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          cant_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_regulacion <- nrow(cant_alegria)+ nrow(cant_confianza)+nrow(cant_miedo)+nrow(cant_tristeza)
          ########################## AMOR ########################
          alexis <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          confa <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_01 <- nrow(alexis) + nrow(confa)
          ver_resul <- round((suma_01/suma_regulacion),4)*100
          ####################### DESESPERACION ############################
          emo_mied <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_desesperacion <- nrow(emo_mied) + nrow(emo_tristeza)
          res_desesperacion <- round((suma_desesperacion/suma_regulacion),4)*100
          ############################# OTRO #######################
          Emociones <- c("Amor","Desesperación")
          Tipo <- c("Positiva", "Negativa")
          Cantidad_01 <- c(suma_01, suma_desesperacion)
          Porcentaje <- c(ver_resul, res_desesperacion)
          Autoconocimiento <- as.data.frame(cbind(Emociones, Tipo, Cantidad_01, Porcentaje))
          return(Autoconocimiento)
        })
        ################################ CANT POS - EMOC (AUTOREG) #####################
        output$regulacion_emopos <- renderText({
          #RESUMENES
          tabla_sent <- count(dic_nrc,Sentimientos)
          cant_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          cant_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          cant_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          cant_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_regulacion <- nrow(cant_alegria)+ nrow(cant_confianza)+nrow(cant_miedo)+nrow(cant_tristeza)
          ########################## AMOR ########################
          alexis <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          confa <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_01 <- nrow(alexis) + nrow(confa)
          ver_resul <- round((suma_01/suma_regulacion),4)*100
          ####################### DESESPERACION ############################
          emo_mied <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_desesperacion <- nrow(emo_mied) + nrow(emo_tristeza)
          res_desesperacion <- round((suma_desesperacion/suma_regulacion),4)*100
          
          total_autoreg_pos <- suma_01
          return(total_autoreg_pos)
        })
        ################################### CANT NEG - EMOC (AUTOREG) #################
        output$regulacion_emoneg <- renderText({
          #RESUMENES
          tabla_sent <- count(dic_nrc,Sentimientos)
          cant_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          cant_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          cant_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          cant_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_regulacion <- nrow(cant_alegria)+ nrow(cant_confianza)+nrow(cant_miedo)+nrow(cant_tristeza)
          ########################## AMOR ########################
          alexis <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          confa <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_01 <- nrow(alexis) + nrow(confa)
          ver_resul <- round((suma_01/suma_regulacion),4)*100
          ####################### DESESPERACION ############################
          emo_mied <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_desesperacion <- nrow(emo_mied) + nrow(emo_tristeza)
          res_desesperacion <- round((suma_desesperacion/suma_regulacion),4)*100
          
          total_autorep_neg <- suma_desesperacion
          return(total_autorep_neg)
        })
        ################################# GRAFICA EMOCIONES AUTOREG ###################
        output$gra_reg <- renderPlot({
          #RESUMENES
          tabla_sent <- count(dic_nrc,Sentimientos)
          cant_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          cant_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          cant_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          cant_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_regulacion <- nrow(cant_alegria)+ nrow(cant_confianza)+nrow(cant_miedo)+nrow(cant_tristeza)
          ########################## AMOR ########################
          alexis <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          confa <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_01 <- nrow(alexis) + nrow(confa)
          ver_resul <- round((suma_01/suma_regulacion),4)*100
          ####################### DESESPERACION ############################
          emo_mied <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_tristeza <- dplyr::filter(dic_nrc, Sentimientos == "tristeza")
          suma_desesperacion <- nrow(emo_mied) + nrow(emo_tristeza)
          res_desesperacion <- round((suma_desesperacion/suma_regulacion),4)*100
          
          Emociones <- c("Amor","Desesperación")
          contador_reg <- c(suma_01, suma_desesperacion)
          etiquetas <- paste0(Emociones, " = ", round(100*contador_reg/sum(contador_reg), 2), "%")
          resultado_reg <- pie(contador_reg, labels = etiquetas)
        
          return(resultado_reg)
        })
        
        ################################ HABILIDAD SOCIAL #####################
        ############## INDICADORES HABILIDAD SOCIAL ############
        output$tabla_02 <- renderTable({
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          return(tabla_sentimiento)
        })
        ########## CANTIDAD DE SENTIMIENTOS ################
        output$salida_03 <- renderText({
          cant_sentimiento <- length(dic_nrc$Sentimientos)
          return(cant_sentimiento)
        })
        ########### CANTIDAD DE PALABRAS A TRABAJAR SOCIAL#############
        output$salida_04 <- renderText({
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          return(suma_social)
        })
        ################## INDICADOR ALEGRIA 2 ###################
        output$happy <- renderPlot({
          graf_ale<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!="negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            #ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "orange") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Alegría")
          return(graf_ale)
        })
        ################## INDICADOR CONFIANZA 2######################
        output$fianza <- renderPlot({
          graf_conf<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="alegría" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "dark green") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Confianza")
          return(graf_conf)
        })
        ####################### EMOCIÓN POSITIVA 2######################
        #-------------------------------- AMOR ------------------------#
        output$sal_gria <- renderText({
          #----------- Filtrado --------------
          fel <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          return(nrow(fel))
        })
        output$sal_fi <- renderText({
          #--------------- Filtrado ------------#
          co <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          return(nrow(co))
        })
        output$sumar_amor <- renderText({
          #----------- Suma de emociones Alegría y Confianza -------#
          fel <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          
          co <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          
          suma_02 <- nrow(fel) + nrow(co) 
          return(suma_02)
        })
        output$result_am <- renderText({
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          
          #cant_sentimiento <- length(dic_nrc$Sentimientos)
          
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          
          
          fel <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          
          co <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          
          suma_02 <- nrow(fel) + nrow(co)
          
          ver_sol <- round((suma_02/suma_social),4)*100
          return(ver_sol)
        })
        ########################### INDICADOR DE CONFIANZA ###################
        output$ind_fianza <- renderPlot({
          graf_fia<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="alegría" & Sentimientos!="tristeza"
                   & Sentimientos!="ira" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "dark green") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Confianza")
          return(graf_fia)
        })
        ############### Indicador de Sorpresa #################
        output$ind_sorpresa <- renderPlot({
          graf_sorp<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="anticipacion" 
                   & Sentimientos!="alegría" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "sky blue") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Sorpresa")
          return(graf_sorp)
        })
        ############################ CURIOSIDAD ################
        output$salida_fianza <- renderText({
          #----------- Filtrado --------------
          emo_fianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          return(nrow(emo_fianza))
        })
        output$salida_sorpresa <- renderText({
          #--------------- Filtrado ------------#
          emo_sorp <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          return(nrow(emo_sorp))
        })
        output$sumar_cs <- renderText({
          emo_fianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          emo_sorp <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          #----------- Suma de emociones Confianza y Sorpresa -------#
          suma_curiosidad <- nrow(emo_fianza) + nrow(emo_sorp)
          return(suma_curiosidad)
        })
        output$result_curiosidad <- renderText({
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          #cant_sentimiento <- length(dic_nrc$Sentimientos)
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          
          emo_fianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          emo_sorp <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          suma_curiosidad <- nrow(emo_fianza) + nrow(emo_sorp)
          
          res_curiosidad <- round((suma_curiosidad/suma_social),4)*100
          return(res_curiosidad)
        })
        #################### EMOCIONES NEGATIVAS ####################
        #################### INDICADOR DE DESAGRADO #####################
        output$ind_desagrado <- renderPlot({
          graf_desag<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="miedo" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="alegría" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "purple") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de desagrado")
          return(graf_desag)
        })
        ###########################  INDICADOR DE IRA ###########################
        output$ind_irasa <- renderPlot({
          graf_irasa<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="alegría" & Sentimientos!="anticipacion" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "red") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Ira")
          return(graf_irasa)
        })
        ########################## DESPRECIO ################
        output$salida_desa <- renderText({
          #----------- Filtrado --------------
          emo_desa <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          return(nrow(emo_desa))
        })
        output$salida_iras <- renderText({
          #--------------- Filtrado ------------#
          emo_irases <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          return(nrow(emo_irases))
        })
        output$sumar_di <- renderText({
          emo_desa <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          emo_irases <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          #----------- Suma de emociones Miedo y Anticipacion -------#
          suma_desprecio <- nrow(emo_desa) + nrow(emo_irases)
          return(suma_desprecio)
        })
        output$result_desprecio <- renderText({
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          #cant_sentimiento <- length(dic_nrc$Sentimientos)
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          
          emo_desa <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          emo_irases <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          suma_desprecio <- nrow(emo_desa) + nrow(emo_irases)
          
          res_desprecio <- round((suma_desprecio/suma_social),4)*100
          return(res_desprecio)
        })
        
        #################### INDICADOR DE MIEDO #####################
        output$ind_mdo <- renderPlot({
          graf_mia<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="anticipacion" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="alegría" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "dark green") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Frecuencia", x = "Emociones de Miedo")
          return(graf_mia)
        })
        ###################  INDICADOR DE ANTICIPACION ##############
        output$ind_pacion <- renderPlot({
          graf_anticipa<-dic_nrc %>%
            filter(Sentimientos!="positivo" & Sentimientos!="desagrado" & Sentimientos!="sorpresa" 
                   & Sentimientos!="alegría" & Sentimientos!="ira" & Sentimientos!="tristeza"
                   & Sentimientos!="confianza" & Sentimientos!="miedo" & Sentimientos!= "negativo") %>%
            count(word,Sentimientos,sort=TRUE) %>%             
            group_by(Sentimientos) %>%                        
            top_n(5) %>%                                                     
            ungroup() %>%                                   
            mutate(word=reorder(word,n)) %>%                
            ggplot(aes(word,n,fill=Sentimientos))+           
            geom_col(show.legend = FALSE, fill = "orange") +
            geom_text(aes(label=n), hjust= 0) +
            facet_wrap(~Sentimientos,scales = "free_y")+  
            coord_flip() +
            labs(y = "Cantidad", x = "Emociones de Anticipación")
          return(graf_anticipa)
        })
        ########################## ANSIEDAD ################
        output$salida_mie <- renderText({
          #----------- Filtrado --------------
          emo_miedi <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          return(nrow(emo_miedi))
        })
        output$salida_pacion <- renderText({
          #--------------- Filtrado ------------#
          emo_anti <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          return(nrow(emo_anti))
        })
        output$sumar_ma <- renderText({
          emo_miedi <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_anti <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          #----------- Suma de emociones Miedo y Anticipacion -------#
          suma_ansied <- nrow(emo_miedi) + nrow(emo_anti)
          return(suma_ansied)
        })
        output$result_ansi <- renderText({
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          #cant_sentimiento <- length(dic_nrc$Sentimientos)
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          
          emo_miedi <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_anti <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_ansied <- nrow(emo_miedi) + nrow(emo_anti)
          
          res_ansied <- round((suma_ansied/suma_social),4)*100
          return(res_ansied)
        })
        ########################### CUADRO RESUMEN 02 ######################
        output$tbl_res_02 <- renderTable({
          #RESUMENES
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
        ################################ AMOR #############################
          fel <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          co <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_02 <- nrow(fel) + nrow(co)
          ver_sol <- round((suma_02/suma_social),4)*100 
        ############################### CURIOSIDAD #########################
          emo_fianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          emo_sorp <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          suma_curiosidad <- nrow(emo_fianza) + nrow(emo_sorp)
          res_curiosidad <- round((suma_curiosidad/suma_social),4)*100
        ############################## DESPRECIO ##########################
          emo_desa <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          emo_irases <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          suma_desprecio <- nrow(emo_desa) + nrow(emo_irases)
          res_desprecio <- round((suma_desprecio/suma_social),4)*100
        ############################### ANSIEDAD #########################
          emo_miedi <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_anti <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_ansied <- nrow(emo_miedi) + nrow(emo_anti)
          res_ansied <- round((suma_ansied/suma_social),4)*100
        ############################## OTRO 02 #############################
          Emociones_02 <- c("Amor", "Curiosidad", "Desprecio",
                         "Ansiedad")
          Tipo_02 <- c("Positiva", "Positiva", "Negativa",
                    "Negativa")
          Cantidad_02 <- c(suma_02, suma_curiosidad, suma_desprecio, suma_ansied)
          Porcentaje_02 <- c(ver_sol, res_curiosidad,
                          res_desprecio, res_ansied)
          HabilidadSocial <- as.data.frame(cbind(Emociones_02, Tipo_02, Cantidad_02, Porcentaje_02))
          return(HabilidadSocial)
        })
        ########################## CANT POS - EMOC ###################
        output$social_emopos <- renderText({
          #RESUMENES
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          ################################ AMOR #############################
          fel <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          co <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_02 <- nrow(fel) + nrow(co)
          ver_sol <- round((suma_02/suma_social),4)*100 
          ############################### CURIOSIDAD #########################
          emo_fianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          emo_sorp <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          suma_curiosidad <- nrow(emo_fianza) + nrow(emo_sorp)
          res_curiosidad <- round((suma_curiosidad/suma_social),4)*100
          ############################## DESPRECIO ##########################
          emo_desa <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          emo_irases <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          suma_desprecio <- nrow(emo_desa) + nrow(emo_irases)
          res_desprecio <- round((suma_desprecio/suma_social),4)*100
          ############################### ANSIEDAD #########################
          emo_miedi <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_anti <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_ansied <- nrow(emo_miedi) + nrow(emo_anti)
          res_ansied <- round((suma_ansied/suma_social),4)*100
          total_emopos <- suma_02 + suma_curiosidad
          return(total_emopos)
        })
        ########################### CANT NEG - EMOC ###################
        output$social_emoneg <- renderText({
          #RESUMENES
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          ################################ AMOR #############################
          fel <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          co <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_02 <- nrow(fel) + nrow(co)
          ver_sol <- round((suma_02/suma_social),4)*100 
          ############################### CURIOSIDAD #########################
          emo_fianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          emo_sorp <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          suma_curiosidad <- nrow(emo_fianza) + nrow(emo_sorp)
          res_curiosidad <- round((suma_curiosidad/suma_social),4)*100
          ############################## DESPRECIO ##########################
          emo_desa <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          emo_irases <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          suma_desprecio <- nrow(emo_desa) + nrow(emo_irases)
          res_desprecio <- round((suma_desprecio/suma_social),4)*100
          ############################### ANSIEDAD #########################
          emo_miedi <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_anti <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_ansied <- nrow(emo_miedi) + nrow(emo_anti)
          res_ansied <- round((suma_ansied/suma_social),4)*100
          total_emoneg <- suma_desprecio + suma_ansied
          return(total_emoneg)
        })
        ########################## GRAFICA EMOCIONES HABILIDAD SOCIAL #############
        output$gra_hab <- renderPlot({
          #RESUMENES
          tabla_sentimiento <- count(dic_nrc,Sentimientos)
          can_alegria <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          can_confianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          can_sorpresa <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          can_desagrado <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          can_ira <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          can_miedo <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          can_anticipacion <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_social <- nrow(can_alegria)+ nrow(can_confianza)+nrow(can_sorpresa)+
            nrow(can_desagrado)+ nrow(can_ira)+nrow(can_miedo)+nrow(can_anticipacion)
          ################################ AMOR #############################
          fel <- dplyr::filter(dic_nrc, Sentimientos == "alegría")
          co <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          suma_02 <- nrow(fel) + nrow(co)
          ver_sol <- round((suma_02/suma_social),4)*100 
          ############################### CURIOSIDAD #########################
          emo_fianza <- dplyr::filter(dic_nrc, Sentimientos == "confianza")
          emo_sorp <- dplyr::filter(dic_nrc, Sentimientos == "sorpresa")
          suma_curiosidad <- nrow(emo_fianza) + nrow(emo_sorp)
          res_curiosidad <- round((suma_curiosidad/suma_social),4)*100
          ############################## DESPRECIO ##########################
          emo_desa <- dplyr::filter(dic_nrc, Sentimientos == "desagrado")
          emo_irases <- dplyr::filter(dic_nrc, Sentimientos == "ira")
          suma_desprecio <- nrow(emo_desa) + nrow(emo_irases)
          res_desprecio <- round((suma_desprecio/suma_social),4)*100
          ############################### ANSIEDAD #########################
          emo_miedi <- dplyr::filter(dic_nrc, Sentimientos == "miedo")
          emo_anti <- dplyr::filter(dic_nrc, Sentimientos == "anticipacion")
          suma_ansied <- nrow(emo_miedi) + nrow(emo_anti)
          res_ansied <- round((suma_ansied/suma_social),4)*100
          
          Emociones_02 <- c("Amor", "Curiosidad", "Desprecio",
                            "Ansiedad")
          contador_hab <- c(suma_02, suma_curiosidad, suma_desprecio, suma_ansied)
          etiq <- paste0(Emociones_02, " = ", round(100*contador_hab/sum(contador_hab), 2), "%")
          resultado_hab <- pie(contador_hab, labels = etiq)
          return(resultado_hab)
        })
        ########################### REPORTES #####################
        
        
        ###################### VISUALIZACION PALABRAS FRECUENTES ############################
        gr1<-dic_nrc %>%
          count(word, sort = TRUE) %>%           #count(): Para contar palabras de mayor a menor
          filter(n>5) %>%                    #filter(): Filtrar las palabras que mas se repiten
          top_n(20) %>%
          mutate(word = reorder(word, n)) %>%    #mutate(): Para modificar una columna
          ggplot(aes(word, n)) +             #reorder(): Ordenar una variable.
          geom_text(aes(label=n), hjust= -0.2) + #geom_text(): Asigna una etiqueta de datos
          geom_col() +                           #xlab(): Elimina el nombre del eje x
          xlab(NULL) +                           #coord_flip(): Invierte ejes
          coord_flip()+                          #theme_minimal(): Aplica un tipo de fondo grafico
          theme_minimal()
          
        return(gr1)
        
        })
      
    })
     
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
      data <- histdata[seq_len(input$slider1)]
      hist(data)
    })
    
    output$plot2 <- renderPlot({
      data <- histdata[seq_len(input$slider2)]
      hist(data)
    })

})