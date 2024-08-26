library(tcltk)
library(readxl)
library(vegan)
library(iNEXT)
library(ggplot2)
library(dplyr)

# Pedir la ruta del archivo Excel
ruta_archivo <- tclvalue(tkgetOpenFile(filetypes = "{{Excel Files} {.xlsx}} {{All files} *}"))

if (ruta_archivo != "") {
  print(paste("Ruta seleccionada:", ruta_archivo))
} else {
  print("No se seleccionó ningún archivo")
}
library(readxl)

# Leer el archivo Excel
if (ruta_archivo != ""){
  data <- read_excel(ruta_archivo)
  print(head(data))  # Muestra las primeras filas del conjunto de datos
  
}
#-----------------------------------------------------------------
str(data)
datos<-as.data.frame(data)
#Establecer la primera columna como nombresdefila
rownames(datos) <-datos[, 1]
#Eliminar la primera columna, ya que ahora es el índice de fila
datos<-datos[,-1];
datos%>% as_tibble()
#:::::::::::::::::::::...
#distancias<-vegdist(datos, method="jaccard", binary=TRUE);
#distancias
#cluster <- hclust(distancias)
#plot(cluster)
#::::::::::::::::::::::::::.

#str(datos)
#-----------------------------------------------------------------
library(vegan)
Sobs<-specnumber(datos)  
#Sobs 
Hsw<-diversity(datos)
#Hsw
simp<-diversity(datos, "simpson") 
#simp
Ep<-Hsw/log(Sobs)
#Ep
#para ver si la especie x es dominante en cada PARCELA

abd<-(data)

# Verifica los nombres de las columnas
print(colnames(abd))
row_sums <- rowSums(abd[, -1])

# Abundancia relativa
abd_r <- abd[, -1] / row_sums

# Agregar la columna de parcelas
abd_r <- cbind(abd$parcela, abd_r)

colnames(abd_r)[1] <- "parcela"
# Mostrar resultados
#print(abd_r)

abd_r$Sobs<-c(Sobs)
str(abd_r)
#-----------------------------------------------------------------
# Cargar la biblioteca shiny
library(shiny)

# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel("Resultados de Operaciones"),
  
  # Botón para generar el dataframe
  actionButton("tabla", "Generar Dataframe-base"),
  
  # Espacio para mostrar el dataframe
  tableOutput("table_result"),
  
  # Botón para mostrar los resultados
  actionButton("show_results", "Índices generales"),
  
  # Menú desplegable para elegir la especie
  selectInput("especie", "Dominancia-Selecciona la especie:",
              choices = colnames(abd_r)[!colnames(abd_r) %in% "Sobs"]),
  
  # Botón para aplicar el filtro
  actionButton("filtrar", "Aplicar Filtro"),
  
  # Espacio para mostrar los resultados
  tableOutput("resultados"),
  textOutput("mensaje"),
  
  # Espacio para mostrar la tabla de distancias
  div(
    h3("Distancias"),
    tableOutput("distancias")
  ),
  
  # Botón para generar el dendrograma
  actionButton("generar_dendo", "Generar Dendrograma"),
  
  # Espacio para mostrar el gráfico
  plotOutput("dendrograma"),
  
  # Botón para generar las curvas
  actionButton("generar", "Generar Curvas de Rarefacción"),
  
  # Mostrar el gráfico
  plotOutput("rare_plot"),
  
  # Input para que el usuario elija la fila
  numericInput("fila", "Seleccione la fila para analizar c. rarefacción de mayor esfuerzo:", min = 1, max = nrow(datos), value = 1),
  
  # Botón para generar la curva
  actionButton("generar_2", "Generar Curva de mayor esfuerzo"),
  
  # Mostrar el gráfico
  plotOutput("rare_plot_2"),
  
  # Espacio para mostrar el mensaje de error
  textOutput("error_message")

)

# Definir la lógica del servidor
server <- function(input, output, session) {
  
  
  # Evento al presionar el botón "tabla"
  observeEvent(input$tabla, {
    # Crear un dataframe de ejemplo
    tb_datos <- data.frame(datos)
    
    # Mostrar el dataframe en la aplicación
    output$table_result <- renderTable({
      tb_datos
    })
  })
  
  # Crear algunos datos simulados (pueden ser resultados de operaciones)
  resultado1 <- c(Sobs)
  resultado2 <- c(Hsw)
  resultado3 <- c(simp)
  resultado4 <- c(Ep)
  
  # Evento al presionar el botón "show_results"
  observeEvent(input$show_results, {
    resultado1_txt <- paste("<ul>", paste("<li>", resultado1, "</li>", collapse = ""), "</ul>")
    resultado2_txt <- paste("<ul>", paste("<li>", resultado2, "</li>", collapse = ""), "</ul>")
    resultado3_txt <- paste("<ul>", paste("<li>", resultado3, "</li>", collapse = ""), "</ul>")
    resultado4_txt <- paste("<ul>", paste("<li>", resultado4, "</li>", collapse = ""), "</ul>")
    
    showModal(modalDialog(
      title = "Resultados",
      # Mostrar los resultados en formato HTML
      HTML(paste("Riqueza observada: ", resultado1_txt, "<br>",
                 "Ind. Shanon-Weaver: ", resultado2_txt, "<br>",
                 "Ind. Simpson: ", resultado3_txt, "<br>",
                 "Ind. Pielou: ", resultado4_txt)),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$filtrar, {
    # Convertir el nombre de la especie ingresado a minúsculas
    especie_usuario <- tolower(input$especie)
    
    # Convertir los nombres de las columnas a minúsculas
    nombres_columnas <- tolower(names(abd_r))
    
    # Verificar si la columna existe
    if (especie_usuario %in% nombres_columnas) {
      # Obtener el nombre real de la columna
      nombre_columna_real <- colnames(abd_r)[which(nombres_columnas == especie_usuario)]
      
      # Aplicar la fórmula basada en la columna seleccionada
      dominancia <- with(abd_r, abd_r[[nombre_columna_real]] > (1 / abd_r$Sobs))
      
      # Mostrar los resultados
      output$resultados <- renderTable({
        data.frame(Parcela = 1:nrow(abd_r), Dominante = dominancia)
      })
      
      # Ocultar mensaje de error si la columna existe
      output$mensaje <- renderText({
        NULL
      })
    } else {
      output$resultados <- renderTable({
        data.frame()
      })
      
      # Mostrar mensaje de error si la columna no existe
      output$mensaje <- renderText({
        "Nombre de la especie no encontrado. Por favor, intenta otro nombre."
      })
    }
  })
  
  observeEvent(input$generar_dendo, {
    # Calcular las distancias usando el método de Jaccard
    distancias <- vegdist(datos, method = "jaccard", binary = TRUE)
    
    
    # Convertir el objeto dist a una matriz y dar formato adecuado
    distancias_matrix <- as.matrix(distancias)
    rownames(distancias_matrix) <- colnames(distancias_matrix) <- rownames(datos)[1:nrow(distancias_matrix)]
    
    # Realizar el clustering jerárquico
    cluster <- hclust(distancias)
    
    # Mostrar el dendrograma
    output$dendrograma <- renderPlot({
      plot(cluster, main = "Dendrograma del Clustering Jerárquico",
           xlab = "", ylab = "", sub = "", cex = 0.8)
    })
    
    # Mostrar la tabla de distancias
    output$distancias <- renderTable({
      distancias_matrix
    }, rownames = TRUE)
    
  })

#rarefacción
# Observar cuando el usuario presiona el botón
observeEvent(input$generar, {
  output$rare_plot <- renderPlot({
    # Asegurarse de que solo se usen columnas numéricas de DATA1
    datos_seleccionados <- datos %>% select_if(is.numeric)
    
    # Calcular el número mínimo de individuos por fila
    minSp <- min(rowSums(datos_seleccionados))
    
    # Generar la curva de rarefacción para todas las muestras numéricas
    rarecurve(datos_seleccionados, step = 1, sample = minSp, col = "darkblue", 
              lwd = 2, cex = 1.2, xlab = "Individuos muestreados", 
              ylab = "Riqueza de especies", main = "Curvas de Rarefacción (Todas las Muestras)")
  })
})


observeEvent(input$generar_2, {
  # Verificar si el número de fila es válido
  if (input$fila > nrow(datos) || input$fila < 1) {
    output$error_message <- renderText({
      "Error: El número de fila no es válido. Por favor, seleccione un número entre 1 y el total de filas."
    })
  } else {
    output$error_message <- renderText({ "" })  # Limpiar mensaje de error
    
    # Seleccionar los datos de la fila elegida
    Test <- unlist(datos[input$fila,])
    
    # Verificar la estructura de Test
    print("Valores de Test antes de filtrar ceros:")
    print(Test)
    
    # (Omitimos el filtrado de ceros ya que mencionas que no hay ceros)
    
    # Verificar si Test está vacío o tiene solo valores no numéricos
    if (length(Test) == 0 || !all(is.numeric(Test))) {
      output$error_message <- renderText({
        "Error: Datos inválidos. Asegúrese de que los valores seleccionados sean numéricos y no estén vacíos."
      })
    } else {
      # Calcular el estimador con iNEXT, controlando posibles errores
      tryCatch({
        estimador <- iNEXT(Test, q = c(0), datatype = "abundance", endpoint = 200)
        
        # Generar el gráfico con ggiNEXT
        output$rare_plot_2 <- renderPlot({
          ggiNEXT(estimador, se = TRUE) + 
            labs(x = "Individuos muestreados", y = "Riqueza de especies")
        })
      }, error = function(e) {
        output$error_message <- renderText({
          paste("Error al calcular el estimador:", e$message)
        })
      })
    }
  }
})


}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
