library('shiny')
library('RSQLite')
library('xtable')
library('DBI')
library('ggplot2')
library('rjson')
#library('reshape2')
#library('data.table')
library('sqldf')
library('tidyr')
library('dplyr')
library('sfsmisc')# arrumar ^10 dos graficos

con <- dbConnect(SQLite(), dbname="tcc_statistics.sqlite")
#con <- dbConnect(SQLite(), dbname="all.sqlite")

f1 <- function(input, output) {

  #pg 71 distribuicao empirica de requicoes por ip
  consultaplot <-dbSendQuery(con, "select questions from DNS_CLIENT;")
  consultaplot1 <- fetch(consultaplot)
  consultaplot1 <- na.omit(consultaplot1)
  consultaplot1 <- consultaplot1[[1]]

  tableplot2 <- data.frame(consultaplot1)

  output$plot <- renderPlot({
    plot(ecdf(consultaplot1))
  })

  output$summary <- renderPrint({
    summary(ecdf(consultaplot1))
  })

   output$table <- renderTable({
     data.frame(x=consultaplot1)
   })

}

f2 <- function(input, output) {

  #pg 71 distribuicao empirica de requicoes por ip
  consultaplot <-dbSendQuery(con, "select questions from DNS_CLIENT;")
  consultaplot1 <- fetch(consultaplot)
  consultaplot1 <- na.omit(consultaplot1)
  consultaplot1 <- consultaplot1[[1]]

  tableplot2 <- data.frame(consultaplot1)

  output$plot <- renderPlot({
    plot(ecdf(consultaplot1))
  })

  output$summary <- renderPrint({
    summary(ecdf(consultaplot1))
  })

   output$table <- renderTable({
     data.frame(x=consultaplot1)
   })
}



# ui <- fluidPage()
#
# server <- function(input, output) {
#
#   output$coolplot <- renderPlot({
#     if (is.null(filtered())) {
#       return()
#     }
#     ggplot(filtered(), aes(Alcohol_Content)) +
#       geom_histogram()
#   })
#
#   output$results <- renderTable({
#     filtered()
#   })
# }
#
# shinyApp(ui = ui, server = server)
