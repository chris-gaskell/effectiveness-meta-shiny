library(shiny)
library(tidyverse)
 lookup <- read.csv("data/lookup.csv") %>% mutate(es = format(es, nsmall = 2)) %>% arrange(es) %>% mutate(es = stringr::str_replace(es, fixed(" "), "")) %>% distinct()

  #filter(group == "depression" & setting == "all")

ui <- fluidPage(
  titlePanel("Benchmarks for Psychological Interventions in Routine Practice"),
  sidebarLayout(
    sidebarPanel(
      h4("Purpose"),
      p("This is the pupose.", style = "font-family: 'times'; font-si16pt"),
      br(),

      h4("Instructions"),
      p("Complete the fields in the right hand box from top to bottom before hitting complete.", style = "font-family: 'times'; font-si16pt"),
      br(),

      h4("Citation"),
      p("If using this shiny app for academic purposes then please cite the original article.", style = "font-family: 'times'; font-si16pt"),
      br(),

      p("Gaskell, Simmonds-Buckley, Kellett, Stockton, Somerville, Rogerson & Delgadillo. (2023). The effectiveness of psychological interventions delivered in routine practice: Systematic review and meta-analysis. Administration and Policy in Mental Health and Mental Health Services Research. 50, 43–57."),
    ),
    mainPanel(
  selectInput("group",     label = "Outcome Type", choices = c("depression", "anxiety", "other")),
  selectInput("setting",   label = "Service Setting", choices = c("outpatient", "inpatient", "all")),
  selectInput("direction", label = "Intended Direction of Change", choices = c("scores decrease", "scores increase")),
  numericInput("n",   label = "Sample Size", value = NA),
  numericInput("mi",  label = "Pre Mean", value = NA),
  numericInput("sdi", label = "Pre SD", value = NA),
  numericInput("mii", label = "Post Mean", value = NA),
  numericInput("r",   label = "Reliability", value = NA),
  verbatimTextOutput("es"),
  verbatimTextOutput("rank"),
  submitButton("Calculate")
    )
  )
  # verbatimTextOutput("d.unadjusted"),
  # verbatimTextOutput("estr"),
  # verbatimTextOutput("d.adjusted"),
  # verbatimTextOutput("var"),
  # verbatimTextOutput("ci.lb"),
  # verbatimTextOutput("ci.ub"),
)
server <- function(input, output, session) {
  d.unadjusted <- reactive(round((input$mi - input$mii) / input$sdi, 2))
  estr <-         reactive(round(input$r + input$r*(1-input$r*input$r)/2/(input$n-4), 2))
  d.adjusted <-   reactive(round((1-3/(4*input$n-5))*d.unadjusted(), 2))
  d.adjusted.2 <- reactive(if (input$direction == "scores increase") {d.adjusted()*-1} else {d.adjusted()})
  var <-          reactive(round(2*(1-estr())/input$n+d.adjusted.2()*d.adjusted.2()/(2*input$n), 2))
  ci.lb <-        reactive(round(d.adjusted.2()-1.96*sqrt(var()), 2))
  ci.ub <-        reactive(round(d.adjusted.2()+1.96*sqrt(var()), 2))
  d.lookup <-     reactive(format(d.adjusted.2(), nsmall = 2))
  group <-        reactive(input$group)
  setting <-      reactive(input$setting)

  rank <-        reactive(lookup %>% filter(es == d.lookup() & setting == setting() & group == group()) %>% distinct(2, 3, .keep_all = T) %>% select(final.rank))


  # output$d.unadjusted <- renderText(paste0("Cohen's d (unadjusted) = ", d.unadjusted()))
  # output$estr <-         renderText(paste0("Reliability estimate = ", estr()))
  # output$d.adjusted <-   renderText(paste0("Adjusted d = ", d.adjusted()))
  # output$var <-     renderText(paste0("Variance = ", var()))
  # output$ci.lb <-   renderText(paste0("CI lb= ", ci.lb()))
  # output$ci.ub <-   renderText(paste0("CI ub = ", ci.ub()))
  output$es <-   renderText(paste0("The Cohen's d effect size is ", d.adjusted.2(),
                                   " (95% CI = ", ci.lb(), " to ", ci.ub(), ")."))
  output$rank <- renderText(paste0("This is the ", rank()))

}
shinyApp(ui, server)

