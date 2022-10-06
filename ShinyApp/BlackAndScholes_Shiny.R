#=============================================================================#
#         B l a c k & S c h o l e s   O p t i o n   P r i c i n g             #
#               D e l t a   H e d g i n g   S t r a t e g y                   #
# - - - - - - - - - - - - - S h i n y   A p p - - - - - - - - - - - - - - - - #
#=============================================================================#

library(BlackScholes); library(shiny)

ui <- fluidPage(
  # T i t l e
  titlePanel(
    h1(strong("Black & Scholes Option Pricing and Delta Hedge"),
              style = "font-size: 24px")
  ),
  # S i d e b a r
  sidebarPanel(
    width = 5,
    fluidRow(
      column(width = 6,
                    selectInput("type","option", choices = c("Call","Put"),selected = "Put")),
      column(width = 6,
                    numericInput("stockprice","stock prive [purchase (€)]",100))
    ),
    fluidRow(
      column(width = 6,
                    numericInput("stockprice2","stock price [current (€)]",100)),
      column(width = 6,numericInput("strikeprice","strike (€)",75))
    ),
    fluidRow(
      column(width = 4,numericInput("maturity",HTML("maturity<br/>(years)"),1,step = .05,min=0)),
      column(width = 4,numericInput("current","current (years)",0.1,step = .05,min=0)),
      column(width = 4,numericInput("purchase","purchase (years)",0,step = .05,min = 0))
    ),
    fluidRow(
      column(width = 6,numericInput("riskfree","riskfree rate of interest (%)",1)),
      column(width = 6,numericInput("vola","implicit volatility (%)", 30, min = 0))),
    fluidRow(
      column(width = 6,numericInput("ratio","ratio",0.1)),
      column(width = 6,numericInput("dividend","dividende yield (%)",2)))
  ),
  # M a i n p a n e l
  mainPanel(
    width = 7,
    tabsetPanel(type = "tabs",
                       tabPanel("option overview",tableOutput("overview")),
                       tabPanel("option value in time",plotOutput("hockeystick")),
                       tabPanel("value Delta hedge",plotOutput("deltahedge"),
                                       fluidRow(
                                         column(width = 6,textOutput("amount")),
                                         column(width = 6,textOutput("price_1_option"))),
                                       fluidRow(
                                         column(width = 6,textOutput("price_options")),
                                         column(width = 6,textOutput("initalPF"))),
                                       fluidRow(
                                         column(width = 6,textOutput("price_1_option_2")),
                                         column(width = 6,textOutput("actualPF"))),
                                       fluidRow(
                                         column(width = 6,textOutput("profitPF")))),
                       tabPanel("profit Delta-Hedge",plotOutput("deltahedge2"))
    )))

server <- function(input, output, session) {
  #====================================================================#
  #             O v e r v i e w   a t   p u r c h a s e                #
  #====================================================================#
  output$overview <- renderTable({
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
  output$hockeystick <- renderPlot({
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
  output$deltahedge <- renderPlot({
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
  output$amount <- renderText({
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

  output$price_1_option <- renderText({
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

  output$price_options <- renderText({
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

  output$initalPF <- renderText({
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

  output$price_1_option_2 <- renderText({
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

  output$profitPF <- renderText({
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
  output$deltahedge2 <- renderPlot({
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
