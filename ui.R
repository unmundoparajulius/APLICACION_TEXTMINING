

# Definimos la interfaz para la aplicacion
shinyUI <- dashboardPage(skin = "blue",
  dashboardHeader(title = "UPAO"
                  ),
  
  # Agregamos los contenidos de la barra lateral
  dashboardSidebar(
    sidebarMenu(
      menuItem("Archivos", tabName = "archivos", icon = icon("archive")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
               menuSubItem("Sentimientos", tabName = "sentimientos"),
               menuSubItem("Emociones", tabName = "emociones"),
               menuSubItem("Nube de palabras", tabName = "nube")
               ),
      menuItem("H-Inteligencia Emocional", tabName = "inteligencia", icon = icon("dashboard"),
               menuSubItem("Autorregulacion", tabName = "autorregulacion")
               ),
      menuItem("H-Liderazgo", tabName = "liderazgo", icon = icon("dashboard"),
               menuSubItem("Habilidad Social", tabName = "social")
      )
    )
  ),
  
  # Contenido del cuerpo
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type = "text/css", href= "custom.css")
    ),
    tabItems(
      # Segundo indice de contenidos
      tabItem(tabName = "archivos",
              
          #Formato para la subida de archivos
          fluidRow(
            box(title = "IMPORTACIÓN DE ARCHIVOS", width = 4,
                solidHeader = TRUE, status = "info",
                h4("¡Es posible que la carga de la data demore por el volumen extenso de la información!"),
              fileInput("file", " ---------------------> Arrastra o sube un archivo CSV o txt <------------------------",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              tags$hr(),
              
              checkboxInput("header", "Cabezera", FALSE),
              
              radioButtons("sep", "Separado por ",
                           choices = c(Comas = ",",
                                       Semicomas = ";",
                                       Tabular = "\t"),
                           selected = ",")
               ),
            box(title = "RESULTADOS DE ARCHIVOS", status = "info", width = 8,
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("contenido")
               )
          )
          #fluidRow(
          #  box(title = "IMPORTACIÓN DE VIDEOS", width = 4,
           #     solidHeader = TRUE, status = "success",
            #    h4("¡Es posible que la carga del audio demore por el volumen extenso de la duración!"),
             #   fileInput("file1", " ---------------------> Arrastra o sube un archivo de audio <------------------------",
              #            multiple = FALSE,
               #           accept = c("text/csv",
                #                     "text/comma-separated-values,text/plain",
                 #                    ".csv"))
            #   ),
          #  box(title = "RESULTADOS DE VIDEO", status = "success", width = 8)
           #       )
           ),#Acaba segundo indice de contenidos

      # Tercera indice de contenidos
      tabItem(tabName = "sentimientos",
              # Colocamos los cuadros en una fila o columna (Primera grafica)
              fluidRow(
                box(title = "Gráfica de sentimientos", status = "danger", 
                    #height = 300,
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("sent", height = 500))
                      )
              
      ),#Acaba tercer indice de contenidos
      tabItem(tabName = "emociones",
              h2("GRÁFICA DE EMOCIONES"),
              fluidRow(
                box(title = "GRÁFICA DE LA EMOCIÓN ALEGRÍA", status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("alegria", height = 300)),
                
                box(title = "GRÁFICA DE LA EMOCIÓN ANTICIPACIÓN", status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("anticipacion", height = 300)),
                
                box(title = "GRÁFICA DE LA EMOCIÓN CONFIANZA", status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("confianza", height = 300)),
                
                box(title = "GRÁFICA DE LA EMOCIÓN DESAGRADO", status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("desagrado", height = 300)),
                
                box(title = "GRAFICA DE IRA", status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("ira", height = 300)),
                
                box(title = "GRÁFICA DE LA EMOCIÓN MIEDO", status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("miedo", height = 300)),
                
                box(title = "GRÁFICA DE LA EMOCIÓN SORPRESA", status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("sorpresa", height = 300)),
                
                box(title = "GRÁFICA DE LA EMOCIÓN TRISTEZA", status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("tristeza", height = 300))
                      )
              ),#Acaba el cuarto indice
      tabItem(tabName = "nube",
              h2("Word Cloud"),
              fluidRow(
                box(title = "NUBE DE PALABRAS DE MANERA GENERAL", status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("cloud", height = 300)),
                box(title = "NUBE DE PALABRAS CON SENTIMIENTO POSITIVO", status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("npp", height = 300)),
                box(title = "NUBE DE PALABRAS CON SENTIMIENTO NEGATIVO", status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    plotOutput("npn", height = 300))
                      )  
              ),#Acaba el quinto indice
      
      # Sexto indice de contenidos
      tabItem(tabName = "autorregulacion",
              h2("INDICADORES DE AUTORREGULACION"),
              fluidRow(
                box(title = "INDICADORES", status = "info", 
                    h3("Emociones Positivas"),
                    h4("Amor = Alegría + Confianza"),
                    h3("Emociones Negativas"),
                    h4("Desesperación = Miedo + Tristeza"),
                    #h4("Envidia = Tristeza + Ira"),
                    solidHeader = TRUE,
                    width = 6,
                    h3("---------------------------------------------------------------------------"),
                    #downloadButton("report","Generar Reporte")
                    ),
                
                box(title = "TABLA DE RESULTADOS DE EMOCIONES", status = "danger", 
                solidHeader = TRUE,
                    width = 6,
                tableOutput("tabla_01"),
                h4("Cantidad Total de Sentimientos: ", textOutput("salida_01")),
                h4("Cantidad de sentimientos a utilizar para la habilidad Autorregulación: ", textOutput("salida_02")))
                ),
              h2("EMOCIONES POSITIVAS"),
              fluidRow(
                box(title = "AMOR (Alegria + Confianza)", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    h4("Cantidad de palabras referidas a la emoción Alegria:"),
                    textOutput("sal_alegria"),
                    h4("Cantidad de palabras referidas a la emoción Confianza:"),
                    textOutput("sal_confianza"),
                    h4("Suma de las emociones de Alegría y Confianza:"),
                    textOutput("sumar_ac"),
                    h4("% de la emoción positiva AMOR: "),
                    textOutput("total_amor")),
                box(title = "Gráfica de E-Alegría", status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("hap", height = 300)
                    ),
                box(title = "Gráfica de E-Confianza", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("anza", height = 300)
                    )
              ),
              h2("EMOCIONES NEGATIVAS"),
              fluidRow(
                box(title = "DESESPERACION (Miedo + Tristeza)", status = "info",
                    solidHeader = TRUE,
                    width = 4,
                    h4("Cantidad de palabras referidas a la emoción miedo:"),
                    textOutput("salida_mied"),
                    h4("Cantidad de palabras referidas a la emoción tristeza:"),
                    textOutput("salida_tristeza"),
                    h4("Suma de las emociones de Miedo y Tristeza:"),
                    textOutput("sum_mt"),
                    h4("% de la emoción negativa DESESPERACION:"),
                    textOutput("result_desesperacion")),
                box(title = "Gráfica de E-Miedo", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_mied", height = 300)
                ),
                box(title = "Gráfica de E-Tristeza", status = "primary",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_tristeza", height = 300)
                )
              ),
              fluidRow(
                box(title = "EMOCIONES ANALIZADAS (AUTORREGULACION)", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    h3("--------- EMOCIONES POSITIVAS ---------"),
                    h4("- Amor (Alegría + Confianza)"),
                    h3("--------- EMOCIONES NEGATIVAS ---------"),
                    h4("- Desesperación (Miedo + Tristeza)"),
                    h3("--------- RESULTADOS FINALES ----------"),
                    h4("Cantidad de emociones positivas: "),
                    textOutput("regulacion_emopos"),
                    h4("Cantidad de emociones negativas: "),
                    textOutput("regulacion_emoneg"),
                    ),
                box(title = "TABLA RESUMEN DE AUTORREGULACION", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    tableOutput("tbl_res_01")
                ),
                box(title = "GRAFICA DE RESUMEN AUTOREGULACION", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("gra_reg"))
              )
      ),#Acaba el cuarto indice de contenidos
      tabItem(tabName = "social",
              h2("INDICADORES DE HABILIDAD SOCIAL"),
              fluidRow(
                box(title = "INDICADORES", status = "info",
                    h3("Emociones Positivas"),
                    h4("Amor = Alegría + Confianza"),
                    h4("Curiosidad = Confianza + Sorpresa"),
                    h3("Emociones negativas"),
                    h4("Desprecio = Desagrado + Ira"),
                    h4("Ansiedad = Miedo + Anticipación"),
                    solidHeader = TRUE,
                    width = 6),
                box(title = "TABLA DE RESULTADOS DE EMOCIONES", status = "danger",
                    solidHeader = TRUE,
                    width = 6,
                    tableOutput("tabla_02"),
                    h4("Cantidad total Sentimientos:", textOutput("salida_03")),
                    h4("Cantidad de Sentimientos a usar para la habilidad social:", 
                       textOutput("salida_04") ))
              ),
              h2("EMOCIONES POSITIVAS"),
              fluidRow(
                box(title = "AMOR (Alegría + Confianza)", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    h4("Cantidad de palabras referidas a la emoción Alegría:"),
                    textOutput("sal_gria"),
                    h4("Cantidad de palabras referidas a la emoción Confianza:"),
                    textOutput("sal_fi"),
                    h4("Suma de las emociones de Alegría y Confianza:"),
                    textOutput("sumar_amor"),
                    h4("% de la emoción positiva AMOR:"),
                    textOutput("result_am")),
                box(title = "Gráfica de E-Alegria", status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("happy", height = 300)),
                box(title = "Gráfica de la E-Confianza", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("fianza", height = 300))
              ),
              fluidRow(
                box(title = "CURIOSIDAD (Confianza + Sorpresa)", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    h4("Cantidad de palabras referidas a la emoción confianza:"),
                    textOutput("salida_fianza"),
                    h4("Cantidad de palabras referidas a la emocion sorpresa:"),
                    textOutput("salida_sorpresa"),
                    h4("Suma de las emociones de Confianza y Sorpresa:"),
                    textOutput("sumar_cs"),
                    h4("% de la emoción de Confianza y Sorpresa"),
                    textOutput("result_curiosidad")),
                box(title = "Gráfica de la E-Confianza", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_fianza", height = 300)
                    ),
                box(title = "Gráfica de E-Sorpresa", status = "info",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_sorpresa", height = 300)
                    )
              ),
              h2("EMOCIONES NEGATIVAS"),
              fluidRow(
                box(title = "DESPRECIO (Desagrado + Ira)", status = "danger",
                    solidHeader = TRUE,
                    width = 4,
                    h4("Cantidad de palabras referidas a la emoción desagrado:"),
                    textOutput("salida_desa"),
                    h4("Cantidad de palabras referidas a la emoción Ira:"),
                    textOutput("salida_iras"),
                    h4("Suma de las emociones de Desagrado e Ira:"),
                    textOutput("sumar_di"),
                    h4("% de la emoción negativa DESPRECIO:"),
                    textOutput("result_desprecio")),
                box(title = "Gráfica de E-Desagrado", status = "primary",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_desagrado", height = 300)),
                box(title = "Gráfica de E-Ira", status = "danger",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_irasa", height = 300))
              ),
              fluidRow(
                box(title = "ANSIEDAD (Miedo + Anticipación)", status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    h4("Cantidad de palabras referidas a la emoción miedo:"),
                    textOutput("salida_mie"),
                    h4("Cantidad de palabras referidas a la emoción anticipacion:"),
                    textOutput("salida_pacion"),
                    h4("Suma de las emociones de Miedo y Anticipación:"),
                    textOutput("sumar_ma"),
                    h4("% de la emoción negativa ANSIEDAD:"),
                    textOutput("result_ansi")),
                box(title = "Gráfica de E-Miedo", status = "success",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_mdo", height = 300)),
                box(title = "Gráfica de E-Anticipación", status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("ind_pacion", height = 300))
              ),
              fluidRow(
                box(title = "EMOCIONES ANALIZADAS (HABILIDAD SOCIAL)", status = "danger",
                    solidHeader = TRUE,
                    width = 4,
                    h3("--------- EMOCIONES POSITIVAS ---------"),
                    h4("- Amor (Alegría + Confianza)"),
                    h4("- Curiosidad (Confianza + Sorpresa)"),
                    h3("--------- EMOCIONES NEGATIVAS ---------"),
                    h4("- Desprecio (Desagrado + Ira)"),
                    h4("- Ansiedad (Miedo + Anticipación)"),
                    h3("--------- RESULTADOS FINALES ----------"),
                    h4("Cantidad de Emociones positivas: "),
                    textOutput("social_emopos"),
                    h4("Cantidad de Emociones negativas: "),
                    textOutput("social_emoneg")
                ),
                box(title = "TABLA RESUMEN DE HABILIDAD SOCIAL", status = "danger",
                    solidHeader = TRUE,
                    width = 4,
                    tableOutput("tbl_res_02")
                ),
                box(title = "GRAFICA DE RESUMEN HABILIDAD SOCIAL", status = "info",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput("gra_hab"))
              )
              )#Acaba el flujo
      
        )#Acaba tabitems1
    )
)
