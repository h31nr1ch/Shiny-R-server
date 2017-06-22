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

server <- function(input, output,session) {

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
    plot.ecdf(log(consultaplot1),xaxt="n", xlab="Número de requisições",ylab="Fn(x)", main="Distribuição empírica de requisições por IP")
    axis(1,at=c(0,3,6,9,12))
  })

  output$summary <- renderPrint({
    summary(ecdf(consultaplot1))
  })

   output$table <- renderTable({
     data.frame(consultaplot1)
   })

   ####################################################################
   simpleQuery<- paste("select domain from DNS_DOMAIN_SEARCHED;")
   domain <- dbGetQuery(con, simpleQuery)

  #  output$summary <- renderPrint({
  #    summary(ecdf(consultaplot1))
  #  })
    # i=1
    # newdomain <- c()
    # while(i<length(domain[[1]])){
    #     newdomain <- c(newdomain, domain[[i]])
    #     newdomain <- c(newdomain,"\n")
    #     i = i +1
    # }

    output$tabled <-  renderText(
        #print(newdomain[[1])
        print(domain[[1]])
    )

    ####################################################################
    # Using a custom container and class
    tags$ul(
        htmlOutput("summary", container = tags$li, class = "custom-li-output")
    )
    ####################################################################
    renderTimeSeries <- function(expr, env=parent.frame(), quoted=FALSE) {
    # Convert the expression + environment into a function
    func <- exprToFunction(expr, env, quoted)

    function() {
      val <- func()
      list(start = tsp(val)[1],
           end = tsp(val)[2],
           freq = tsp(val)[3],
           data = as.vector(val))
    }
    }


    output$timeSeries1 <- renderTimeSeries({
    ts(matrix(rnorm(300), 100, 3), start=c(1961, 1), frequency=12)
    })

    ####################################################################

    tags$code("This text will be displayed as computer code.")

    ####################################################################

    output$txt2Test <- renderText({ input$txt2 })
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
