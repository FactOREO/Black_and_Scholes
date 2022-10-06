#=============================================================================#
#         B l a c k & S c h o l e s   O p t i o n   P r i c i n g             #
#               D e l t a   H e d g i n g   S t r a t e g y                   #
# - - - - - - - - - - - - - S h i n y   A p p - - - - - - - - - - - - - - - - #
#=============================================================================#

ui <- shiny::fluidPage(
  # T i t l e
  shiny::titlePanel(
    shiny::h1(shiny::strong("Black & Scholes Option Pricing and Delta Hedge"),
              style = "font-size: 24px")
  ),
  # S i d e b a r
  shiny::sidebarPanel(
    width = 5,
    shiny::fluidRow(
      shiny::column(width = 6,
                    shiny::selectInput("type","option", choices = c("Call","Put"),selected = "Put")),
      shiny::column(width = 6,
                    shiny::numericInput("stockprice","stock prive [purchase (€)]",100))
    ),
    shiny::fluidRow(
      shiny::column(width = 6,
                    shiny::numericInput("stockprice2","stock price [current (€)]",100)),
      shiny::column(width = 6,numericInput("strikeprice","strike (€)",75))
    ),
    shiny::fluidRow(
      shiny::column(width = 4,shiny::numericInput("maturity",HTML("maturity<br/>(years)"),1,step = .05,min=0)),
      shiny::column(width = 4,shiny::numericInput("current","current (years)",0.1,step = .05,min=0)),
      shiny::column(width = 4,shiny::numericInput("purchase","purchase (years)",0,step = .05,min = 0))
    ),
    shiny::fluidRow(
      shiny::column(width = 6,shiny::numericInput("riskfree","riskfree rate of interest (%)",1)),
      shiny::column(width = 6,shiny::numericInput("vola","implicit volatility (%)", 30, min = 0))),
    shiny::fluidRow(
      shiny::column(width = 6,shiny::numericInput("ratio","ratio",0.1)),
      shiny::column(width = 6,shiny::numericInput("dividend","dividende yield (%)",2)))
  ),
  # M a i n p a n e l
  shiny::mainPanel(
    width = 7,
    shiny::tabsetPanel(type = "tabs",
                       shiny::tabPanel("option overview",shiny::tableOutput("overview")),
                       shiny::tabPanel("option value in time",shiny::plotOutput("hockeystick")),
                       shiny::tabPanel("value Delta hedge",shiny::plotOutput("deltahedge"),
                                       shiny::fluidRow(
                                         shiny::column(width = 6,shiny::textOutput("amount")),
                                         shiny::column(width = 6,shiny::textOutput("price_1_option"))),
                                       shiny::fluidRow(
                                         shiny::column(width = 6,shiny::textOutput("price_options")),
                                         shiny::column(width = 6,shiny::textOutput("initalPF"))),
                                       shiny::fluidRow(
                                         shiny::column(width = 6,shiny::textOutput("price_1_option_2")),
                                         shiny::column(width = 6,shiny::textOutput("actualPF"))),
                                       shiny::fluidRow(
                                         shiny::column(width = 6,shiny::textOutput("profitPF")))),
                       shiny::tabPanel("profit Delta-Hedge",shiny::plotOutput("deltahedge2"))
    )))

server <- function(input, output, session) {
  #====================================================================#
  #             O v e r v i e w   a t   p u r c h a s e                #
  #====================================================================#
  output$overview <- shiny::renderTable({
    BS_Option(
      Call = isTRUE(input$type),
      T = input$maturity,
      t = input$purchase,
      K = input$strikeprice,
      r = input$riskfree,
      d = input$dividend,
      sigma = input$vola,
      ratio = input$ratio)$Greeks |>
      as.data.frame() |> `colnames<-`("value")
  },
  rownames = TRUE)
  #====================================================================#
  #             V al u e   of   t h e   o p t i o n   i n   t i m e    #
  #====================================================================#
  output$hockeystick <- shiny::renderPlot({
    hockeystick_plot(
      Call = isTRUE(input$type),
      T = input$maturity,
      t = input$current,
      K = input$strikeprice,
      r = input$riskfree,
      d = input$dividend,
      sigma = input$vola)
  })
  #====================================================================#
  #             D e l t a   H e d g e   P l o t                        #
  #====================================================================#
  output$deltahedge <- shiny::renderPlot({
    DeltaHedge(
      S = input$stockprice,
      maturity = input$maturity,
      purchase_time = input$purchase,
      current_time = input$current,
      K = input$strikeprice,
      r = input$riskfree,
      d = input$dividend,
      sigma = input$vola,
      integer = TRUE,
      ratio = input$ratio)$graphic +
      ggplot2::geom_point(
        ggplot2::aes(input$stockprice2,
                     input$stockprice2 +
                       BS_Option(
                         Call = FALSE,
                         S = input$stockprice2,
                         T = input$maturity,
                         t = input$current,
                         K = input$strikeprice,
                         r = input$riskfree,
                         d = input$dividend,
                         sigma = input$vola,
                         ratio = input$ratio)$Price *
                       DeltaHedge(
                         S = input$stockprice,
                         maturity = input$maturity,
                         current_time = input$current,
                         purchase_time = input$purchase,
                         K = input$strikeprice,
                         r= input$riskfree,
                         d = input$dividend,
                         sigma = input$vola,
                         integer = TRUE,
                         ratio = input$ratio)$Put_Number),
        col = "red", size = 3)
  })
  #====================================================================#
  #             D e l t a h e d g e  i n f o r m a t i o n s           #
  #====================================================================#
  output$amount <- shiny::renderText({
    paste("Number of Put options needed:",
          DeltaHedge(
            S = input$stockprice,
            maturity = input$maturity,
            current_time = input$current,
            purchase_time = input$purchase,
            K = input$strikeprice,
            r = input$riskfree,
            d = input$dividend,
            sigma = input$vola,
            integer = TRUE,
            ratio = input$ratio)$Put_Number
    )
  })

  output$price_1_option <- shiny::renderText({
    res <- DeltaHedge(
      S = input$stockprice,
      maturity = input$maturity,
      current_time = input$current,
      purchase_time = input$purchase,
      K = input$strikeprice,
      r = input$riskfree,
      d = input$dividend,
      sigma = input$vola,
      integer = TRUE,
      ratio = input$ratio)

    price <- res$Put_Cost / res$Put_Number
    paste("price of an option [purchase]:",
          round(price,4),"€")
  })

  output$price_options <- shiny::renderText({
    paste("total costs [purchase]:",
          DeltaHedge(
            S = input$stockprice,
            maturity = input$maturity,
            current_time = input$current,
            purchase_time =  input$purchase,
            K = input$strikeprice,
            r = input$riskfree,
            d = input$dividend,
            sigma = input$vola,
            integer = TRUE,
            ratio = input$ratio)$Put_Cost |>
            round(4),
          "€")
  })

  output$initalPF <- shiny::renderText({
    paste("portfolio value [purchase]:",
          input$stockprice +
            DeltaHedge(
              S = input$stockprice,
              maturity = input$maturity,
              current_time = input$current,
              purchase_time = input$purchase,
              K = input$strikeprice,
              r = input$riskfree,
              d = input$dividend,
              sigma = input$vola,
              integer = TRUE,
              ratio = input$ratio)$Put_Cost |>
            round(4),
          "€")
  })

  output$price_1_option_2 <- shiny::renderText({
    paste("price of an option [current]:",
          BS_Option(
            Call = FALSE,
            S = input$stockprice2,
            T = input$maturity,
            t = input$current,
            K = input$strikeprice,
            r = input$riskfree,
            d = input$dividend,
            sigma = input$vola,
            ratio = input$ratio)$Price |>
            round(4),
          "€")

  })

  output$actualPF <- renderText({
    paste("portfolio value [current]:",
          (input$stockprice2 +
             BS_Option(
               Call = FALSE,
               S = input$stockprice2,
               T = input$maturity,
               t = input$current,
               K = input$strikeprice,
               r = input$riskfree,
               sigma = input$vola,
               ratio = input$ratio)$Price *
             DeltaHedge(
               S = input$stockprice,
               maturity = input$maturity,
               current_time = input$current,
               purchase_time = input$purchase,
               K = input$strikeprice,
               r = input$riskfree,
               d = input$dividend,
               sigma = input$vola,
               integer = TRUE,
               ratio = input$ratio)$Put_Number) |>
            round(4),
          "€")
  })

  output$profitPF <- shiny::renderText({
    puts <- DeltaHedge(
      S = input$stockprice,
      maturity = input$maturity,
      current_time = input$current,
      purchase_time = input$purchase,
      K = input$strikeprice,
      r = input$riskfree,
      d = input$dividend,
      sigma = input$vola,
      integer = TRUE,
      ratio = input$ratio)$Put_Number

    paste("profit:",
          (input$stockprice2 - input$stockprice +
             puts * (
               BS_Option(
                 Call = FALSE, S = input$stockprice2, T = input$maturity,
                 t = input$current, K = input$strikeprice, r = input$riskfree,
                 d = input$dividend, sigma = input$vola, ratio = input$ratio)$Price -
                 BS_Option(
                   Call = FALSE, S = input$stockprice, T = input$maturity,
                   t = input$purchase, K = input$strikeprice, r = input$riskfree,
                   d = input$dividend, sigma = input$vola, ratio = input$ratio)$Price)
          ) |> round(4),
          "€")
  })
  #====================================================================#
  #             p r o f i t  D e l t a h e d g e                       #
  #====================================================================#
  output$deltahedge2 <- shiny::renderPlot({
    # profit at current time
    puts <- DeltaHedge(
      S = input$stockprice,
      maturity = input$maturity,
      current_time = input$current,
      purchase_time = input$purchase,
      K = input$strikeprice,
      r = input$riskfree,
      d = input$dividend,
      sigma = input$vola,
      integer = TRUE,
      ratio = input$ratio)$Put_Number

    pf_profit <- input$stockprice2 - input$stockprice +
      puts * (
        BS_Option(
          Call = FALSE,
          S = input$stockprice2,
          T = input$maturity,
          t = input$current,
          K = input$strikeprice,
          r = input$riskfree,
          d = input$dividend,
          sigma = input$vola,
          ratio = input$ratio)$Price -
          BS_Option(
            Call = FALSE,
            S = input$stockprice,
            T = input$maturity,
            t = input$purchase,
            K = input$strikeprice,
            r = input$riskfree,
            d = input$dividend,
            sigma = input$vola,
            ratio = input$ratio)$Price)

    DeltaHedge_Profit(
      S = input$stockprice,
      maturity = input$maturity,
      purchase_time = input$purchase,
      current_time = input$current,
      K = input$strikeprice,
      r = input$riskfree,
      d = input$dividend,
      sigma = input$vola,
      integer = TRUE,
      ratio = input$ratio)$graphic +
      ggplot2::geom_point(
        ggplot2::aes(
          input$stockprice2,
          # current portfolio value
          input$stockprice2 +
            BS_Option(
              Call = FALSE,
              S = input$stockprice2,
              T = input$maturity,
              t = input$current,
              d = input$dividend,
              K = input$strikeprice,
              r = input$riskfree,
              sigma = input$vola,
              ratio = input$ratio)$Price *
            DeltaHedge(
              S = input$stockprice,
              maturity = input$maturity,
              current_time = input$current,
              purchase_time = input$purchase,
              K = input$strikeprice,
              r = input$riskfree,
              d = input$dividend,
              sigma = input$vola,
              integer = TRUE,
              ratio = input$ratio)$Put_Number -
            # purchase costs
            DeltaHedge(
              S = input$stockprice,
              maturity = input$maturity,
              current_time = input$current,
              purchase_time = input$purchase,
              K = input$strikeprice,
              r = input$riskfree,
              d = input$dividend,
              sigma = input$vola,
              integer = TRUE,
              ratio = input$ratio)$initPF),
        col = "red", size = 3) +
      ggplot2::annotate(geom = "text", x = input$stockprice2 - 5,
               y = pf_profit + 35, label = paste(round(pf_profit,2),"€"),
               col = "red", size = 6)
  })
}

shinyApp(ui, server)
