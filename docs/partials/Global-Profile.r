# FUnction used to get the value for the comparison
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Data = AFTER

# Function to create the Grafic for Climatelabs
Fig.Global <- function(Data, ancho=750, alto=750, visible = TRUE, ...){


# Selecting the data
Test =  Data %>% filter( Variable != "Comments")

#Questions = Test %>% select(Variable, Descripcion) %>% group_by(Variable, Descripcion) %>% tally()
Test$Value = as.numeric(Test$Value)  # Puttin 'Value'as numeric


# Evaluacion del indice
Test = Test %>% group_by(Variable, University)%>% summarise(Level = getmode(Value))



if (Test$University %>% as.factor() %>% levels() %>% length() > 1 ) {

   # Add variable Global for Consortium
   Climatelabs = Test %>% group_by(Variable) %>% summarise(Level = mean(Level)) %>% mutate( University= "Climate Labs Global" ) %>%
      select(Variable, University, Level)

   Test = rbind(Test, Climatelabs)

}


# PUtting the Description of the variables
Test = merge(Test, Questions, by="Variable") %>% as_tibble()

Test = Test %>% arrange(Role.Climatelabs)

# Identificando la longitud para la division
var = length( as.factor(Test$Variable) %>% levels())



# Organizacion de los factores por competencia
Test$Variable = as.factor(Test$Variable)

Test$Variable <- factor(Test$Variable, levels = Questions$Variable)


# Grafica
Test = Test %>% group_by(University) %>% mutate(degree =seq(from=0, to=360-(360/var), length.out= var ))




Test$o <- Test$Level * sin(Test$degree * pi / 180) # SOH
Test$a <- Test$Level * cos(Test$degree * pi / 180) # CAH
Test$X.exterior <- 5.8 * sin(Test$degree * pi / 180) # Outer ring x
Test$Y.exterior <- 5.8 * cos(Test$degree * pi / 180) # Outer ring y

Test$X10 <- 1 * sin(Test$degree * pi / 180) # 10% ring x
Test$Y10 <- 1 * cos(Test$degree * pi / 180) # 10% ring y
Test$X20 <- 2 * sin(Test$degree * pi / 180) # 20% ring x
Test$Y20 <- 2 * cos(Test$degree * pi / 180) # 20% ring y
Test$X30 <- 3 * sin(Test$degree * pi / 180) # 30% ring x
Test$Y30 <- 3 * cos(Test$degree * pi / 180) # 30% ring y
Test$X40 <- 4 * sin(Test$degree * pi / 180) # 40% ring x
Test$Y40 <- 4 * cos(Test$degree * pi / 180) # 40% ring y
Test$X50 <- 5 * sin(Test$degree * pi / 180) # 50% ring x
Test$Y50 <- 5 * cos(Test$degree * pi / 180) # 50% ring y


# Creacion de Grafica
Test = Test %>% arrange(University)

p = plot_ly( )

p = p %>%
   add_trace(
      x = c(0, Test$X.exterior[1:7], 0), y = c(0, Test$Y.exterior[1:7],0),
      color =  'rgba(255, 212, 96, 0.5)',  fillcolor = 'rgba(230, 233, 255, 0.5)',
      type = 'scatter',
      visible = TRUE,
      showlegend = FALSE,
      mode = "lines",
      line = list(color = "#d3d3d3", dash = "2px"),
      showlegend = FALSE,
      fill = 'toself',
      hoverinfo = "text",
      name="Facilitador",
      text = "Facilitador" ) %>%
   add_trace(
      x = c(0, Test$X.exterior[8:12], 0), y = c(0, Test$Y.exterior[8:12],0),
      color =  'rgba(255, 212, 96, 0.5)',  fillcolor = 'rgba(255, 230, 232, 0.5)',
      type = 'scatter',
      visible = TRUE,
      showlegend = FALSE,
      mode = "lines",
      line = list(color = "#d3d3d3", dash = "2px"),
      showlegend = FALSE,
      fill = 'toself',
      text = "Maker",
      hoverinfo = "text",
      name="Maker" ) %>%
   add_trace(
      x = c(0, Test$X.exterior[13:17], 0), y = c(0, Test$Y.exterior[13:17],0),
      color =  'rgba(255, 212, 96, 0.5)',  fillcolor = 'rgba(248, 248, 248, 0.5)',
      type = 'scatter',
      visible = TRUE,
      showlegend = FALSE,
      mode = "lines",
      line = list(color = "#d3d3d3", dash = "2px"),
      showlegend = FALSE,
      fill = 'toself',
      text = "Manager",
      hoverinfo = "text",
      name="Manager" ) %>%
   add_trace(
      x = c(0, Test$X.exterior[18:23], 0), y = c(0, Test$Y.exterior[18:23],0),
      color =  'rgba(255, 212, 96, 0.5)',  fillcolor = 'rgba(233, 255, 234, 0.6)',
      type = 'scatter',
      visible = TRUE,
      showlegend = FALSE,
      mode = "lines",
      line = list(color = "#d3d3d3", dash = "2px"),
      showlegend = FALSE,
      fill = 'toself',
      text = "Visionario",
      hoverinfo = "text",
      name="Visionario" )

p = p %>%
      add_annotations(
                       x = c(6,6,-6,-6),
                    y = c(6,-6,-6,6),
                    text = c("Facilitateur", "Maker", "Manager", "Visionnaire"),
                    xref = "x",
                    yref = "y",
                    showarrow = F,
                    font = list(color = '#264E86',
                                family = 'Palatino',
                                size = 20)
                    )



for(i in 1:23) {
   p <- add_trace(
      p,
      x = c(Test$X.exterior[i],0),
      y = c(Test$Y.exterior[i],0),
      type="scatter",
     # evaluate = TRUE,
      line = list(color = "#d3d3d3", dash = "2px"),
      showlegend = FALSE,
      hoverinfo = "text",
      text = Test$Descripcion[i],
      hoverlabel=list(font =list(family="Times New Roman"))
   )
}

# Conecting the lines
line.final = Test[1,]
Test=rbind(Test,line.final)
rm(line.final)


Perfil =
   p %>%
   add_trace(x = Test$X.exterior, y = Test$Y.exterior,
             text = Test$Variable,
             hoverinfo = "text",
             textposition = "top middle", mode = "text",
             textfont = list(family= "Helvetica" ,color = '#8e8e8e', size = 13) ,
             #line = list(color = "#d3d3d3", dash = "2px", shape = "spline"),
             text = paste(Test$Descripcion ),
             showlegend = FALSE) %>%

   add_trace(x = rep(0,6), y = c(0:5),
             text = c("", "Novato", "Novato Avanzado", "Competente", "Performante", "Experto"),
             textposition = "top middle", mode = "text",
             textfont = list(family= "Helvetica" ,color = '#8e8e8e', size = 13),
             showlegend = FALSE)   %>%

   add_trace(x = Test$o, y = Test$a, color = Test$University,
             mode = "lines+markers",
             hoverinfo = "text",
             fill = 'toself',
             visible = visible,
             #visible = "legendonly",
             showlegend = TRUE,
             text = paste(Test$University, Test$Level,  Test$Descripcion, sep="\n")) %>%

   add_trace(x = Test$X10, y = Test$Y10, mode = "lines",
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"),
             hoverinfo = "none",
             showlegend = FALSE) %>%

   add_trace(x = Test$X20, y = Test$Y20, mode = "lines",
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"),
             hoverinfo = "none",
             showlegend = FALSE) %>%

   add_trace(x = Test$X30, y = Test$Y30, mode = "lines",
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"),
             hoverinfo = "none",
             showlegend = FALSE) %>%

   add_trace(x = Test$X40, y = Test$Y40, mode = "lines",
             line = list(color = "#d3d3d3", dash = "1px", shape = "spline"),
             hoverinfo = "none",
             showlegend = FALSE) %>%

   add_trace(x = Test$X50, y = Test$Y50, mode = "lines",
             line = list(color = "#d3d3d3", dash = "3px", shape = "spline"),
             hoverinfo = "none",
             showlegend = FALSE) %>%
   layout(
      legend = list(orientation = 'h'),
      #plot_bgcolor  = "rgba(0, 0, 0, 0)",
      #paper_bgcolor = "rgba(0, 0, 0, 0)",
      #fig_bgcolor   = "rgba(0, 0, 0, 0)",
      #autosize = FALSE,
      hovermode = "closest",
      width = ancho,
      #height = 900,
      height = alto,
      autosize = TRUE,
      #autoscale = TRUE,
      xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
      yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE)
   )


return(Perfil)

}

