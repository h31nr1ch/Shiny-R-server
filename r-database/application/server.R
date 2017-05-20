library('shiny')
library('RSQLite')
library('xtable')
library('DBI')
library('ggplot2')
library('rjson')

con <- dbConnect(SQLite(), dbname="tcc_statistics.sqlite")
#con <- dbConnect(SQLite(), dbname="all.sqlite")

# Define server logic for random distribution application
shinyServer(function(input, output) {

  #pg 71 distribuicao empirica de requicoes por ip

  consultaplot <-dbSendQuery(con, "select questions from DNS_CLIENT;")
  consultaplot1 <- fetch(consultaplot)
  consultaplot1 <- na.omit(consultaplot1)
  consultaplot1 <- consultaplot1[[1]]

  #cat("n = ", length(consultaplot1))
  tableplot2 <- data.frame(consultaplot1)

  #png('plot-2.png')
  #plot(ecdf(consultaplot1))
  ##ggplot(tableplot2,aes(x = consultaplot1)) + stat_ecdf(aes(colour = consultaplot1))
  #dev.off()

  # # Reactive expression to generate the requested distribution. This is
  # # called whenever the inputs change. The output renderers defined
  # # below then all used the value computed from this expression
  # data <- reactive({
  #   dist <- switch(input$dist,
  #                  norm = rnorm,
  #                  unif = runif,
  #                  lnorm = rlnorm,
  #                  exp = rexp,
  #                  rnorm)
  #
  #   dist(input$n)
  # })

  # Generate a plot of the data. Also uses the inputs to build the
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    plot(ecdf(consultaplot1))
  })


  # Generate a summary of the data

  output$summary <- renderPrint({
    summary(ecdf(consultaplot1))
  })
  #
  # # Generate an HTML table view of the data
   output$table <- renderTable({
     data.frame(x=consultaplot1)
   })

  # This would go in shinyServer()
  # output$myPlot <- renderPlot({
  # # Your plotting code here
  #     plot(ecdf(consultaplot1))
  # })

})
