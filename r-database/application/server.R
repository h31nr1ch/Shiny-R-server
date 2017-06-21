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

function(input, output) {

  ####################################################################
  consultaplot <-dbSendQuery(con, "select (count) from  DNS_DOMAIN_SEARCHED;")
  consultaplot2 <- fetch(consultaplot)
  consultaplot2 <- na.omit(consultaplot2)
  consultaplot2 <- consultaplot2[[1]]

  output$plot1 <- renderPlot({
    plot.ecdf(log(consultaplot2),xaxt="n", main="Distribuição empírica de requisições por RR",xlab="Número de requisições",ylab="Fn(x)")
    axis(1, at=c(0,3,6,9,12,15))
  })
  ####################################################################

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
     data.frame(consultaplot1)
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
