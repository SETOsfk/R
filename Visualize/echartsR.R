#bar plot
library(ggplot2)
library(plotly)
library(dplyr)
library(echarts4r)

data <- iris |> 
  group_by(Species) |> 
  summarise(
    mean.sepal.lenght= mean(Sepal.Length),
    .groups = "drop"
  )

data |> ggplot(
  aes(x = Species, y = mean.sepal.lenght
  ))+ 
    geom_bar(stat = "identity")

data |> 
  plot_ly(
    x = ~Species ,
    y = ~mean.sepal.lenght,
    type = "bar"
  )


data |> 
  e_charts(x= Species) |> 
  e_bar(serie = mean.sepal.lenght) |> 
  e_tooltip(trigger = "item")#bu baya iyi item dersen onu kullandırıyor,
#axis dersen ayrı yapır

#linechart

dates<- seq.Date(
  from= as.Date("2021-01-01"),
  by = "day",
  length.out = nrow(iris)/3
)

data <- iris |> 
  mutate(
    Date = rep(dates,3)
  ) |> 
  select(
    Date,
    Petal.Width,
    Species
  )
#Simple line chart
data |> 
  group_by(Species) |>  #bunu echartsla kullanınca direkt
  #farklı gruplarla çalıştığımızı aktarıcak e chartsa ve labelda yazıack
  e_charts(x = Date) |> 
  e_line (serie = Petal.Width) |> 
  e_tooltip(trigger ="axis")

#using non-native formatting
?e_line #click the "see additional arguments"

#fill the points on the chart
#solid bir line yerine noktalarla dolmus vb istioz
data |> 
  group_by(Species) |> 
  e_charts(x = Date) |> 
  e_line(
    serie = Petal.Width,
    symbol = "circle" #fill in the circle
  )

#datazoom x ve y axise ekleyip
#grafikte x ya da y de zoomlama şansı veriyor

install.packages(c(
  "shiny","htmlwidgets","reactable"
))

set.seed(321)
heatmap_data <-iris |> 
  dplyr::mutate(
    Group = sample(
      x = LETTERS[1:8],
      size = nrow(iris),
      replace = TRUE
    )
  )
ui <- shiny:: fluidPage(
  title = "Intro to {echarts4r}",
  shiny:: fluidRow(
    shiny:: column(
      width = 12,
      shiny::h3("Intro to {ehcarts4r}"),
      shiny::h6("An R Interfrace to the Apache eCharts Project"),


    )
  ),
  shiny::hr(),
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::tabsetPanel(
        shiny::tabPanel(
          title = "Bar & Line Charts",
          shiny::br(),

          #"Choose which species to display in the bar chart"
          shiny::selectInput(
            inputId = "select_species",
            label = "Select Species:",
            choices = unique(iris$Species),
            selected = c("setosa","versicolor"),
            multiple = TRUE

          ),
          shiny::br(),
          shiny:: fluidRow(
            shiny::column(
              width = 6,

              #bar chart
              echarts4r::echarts4rOutput(
                outputId = "line_chart"
              )
            )

          )


        ),
        shiny::tabPanel(
          title = "Heatmap",
          shiny::br(),
          reactable::reactableOutput(
            outputId = "table",
            height = "251px"
          )
        )
      )
    )
  )
)

server <- function(input,output,session) {
  #filtered data based upon user-selected "species" type
  chart_data<- shiny::reactive({

    iris |> 
      dplyr::filter(Species %in% input$select_species) |> 
      dplyr::select(Sepal.Length,Species) |> 
      dplyr::group_by(Species) |> 
      dplyr::summarise(
        mean_Sepal.Length = mean(Sepal.Length),
        .groups = "drop"
      )
  })
  #render barchart
  output$bar_chart <- echarts4r::renderEcharts4r({

    chart_data() |> 
      echarts4r::e_chart(
        x= Species,
      ) |> 
      echarts4r::e_line(
        serie = mean.Sepal.Length,
        name = "Mean Sepal Length"
      )
  })


  #render the heatmap
  output$heatmap <- echarts4r::renderEcharts4r({
    heatmap_data |> 
      dplyr::group_by(Group, Species) |> 
      dplyr::summarise(
        sum_Petal.Width = sum(Petal.Width),
        .groups = "drop"
      ) |> 
      echarts4r::e_charts(x = Group) |> 
      echarts4r::e_heatmap(
        y= Species,
        z= sum_Petal.Width
      ) |> 
      echarts4r::e_visual_map(
        serie = sum_Petal.Width,
        show = FALSE #hide the big gradient legend thing

      ) |> 
      echarts4r::e_title(
        text = "Sum of Petal width",
        subtext = "by Group & Species"
      ) |> 
      echarts4r::e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS(
          "
          function(params){
          return(
          'Group: ' + params.value[0] + '<br />' +
            'Species: ' + params.value[1] + '<br />' +
            params.marker + 'Sum of Petal width: <strong>' + params.value[2]+'<strong/>'
          )
          }
          "
        )
      )
  })

  #grab the data in the cell that user clicked on

  selected<- shiny::eventReactive(input$heatmap_clicked_data, {
    list (
      Group = input$heatmap_clicked_data$value[1],
      Species = input$heatmap_clicked_data$value[2]
    )
  })

  #build the reactive table

  output$table<- reactable::renderReactable({

    shiny::req(selected())

    heatmap_data |> 
      dplyr::filter(
        Group == selected()$Group,
        Species == selected()$Species
      ) |> 
      reactable::reactable(defaultPageSize = 5)
  })






}












