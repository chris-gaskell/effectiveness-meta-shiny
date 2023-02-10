library(shiny)
library(tidyverse)
 lookup <- read.csv("data/lookup.csv") %>%
   mutate(es = format(es, nsmall = 2)) %>%
   arrange(es) %>%
   mutate(es = stringr::str_replace(es, fixed(" "), "")) %>%
   distinct() %>%
   mutate(group = str_to_title(group),
          setting = str_to_title(setting))

 ks <-
   data.frame(
     group =   rep(c("Depression", "Anxiety", "Other"), 3),
     setting = c(rep("Outpatient", 3), rep("Inpatient", 3), rep("All", 3)),
     k = c(121, 75, 153,
           16, 9, 28,
           140, 84, 184)
   )


  #filter(group == "depression" & setting == "all")

ui <- fluidPage(
  titlePanel("Benchmarks for Psychological Interventions in Routine Practice"),
  sidebarLayout(
    sidebarPanel(
      h4("Purpose"),
      p("This shiny app calculates a pre-post Cohen's d effect-size using the approach provided by Minami et al., (2008) before comparing it to a large set of available benchmarks for routine clinical services (Gaskell et al., 2023).", style = "font-family: 'times'; font-si16pt"),
      br(),

      h4("Instructions"),
      p("Complete the fields in the right hand box from top to bottom before hitting 'complete'.", style = "font-family: 'times'; font-si16pt"),
      br(),

      h4("Citation"),
      p("If using this shiny app for academic purposes then please cite the original article.", style = "font-family: 'times'; font-si16pt"),

      p("Gaskell, Simmonds-Buckley, Kellett, Stockton, Somerville, Rogerson & Delgadillo. (2023). The effectiveness of psychological interventions delivered in routine practice: Systematic review and meta-analysis. Administration and Policy in Mental Health and Mental Health Services Research. 50, 43–57.", style = "font-family: 'times'; font-si16pt"),
      p("Minami, T., Serlin, R. C., Wampold, B. E., Kircher, J. C., & Brown, G. J. (2008). Using clinical trials to benchmark effects produced in clinical practice. Quality and Quantity, 42(4), 513.", style = "font-family: 'times'; font-si16pt"),

    ),
    mainPanel(
  selectInput("group",     label = "Outcome Type", choices = c("Depression", "Anxiety", "Other")),
  selectInput("setting",   label = "Service Setting", choices = c("Outpatient", "Inpatient", "All")),
  selectInput("direction", label = "Intended Direction of Change", choices = c("Scores Decrease", "Scores Increase")),
  numericInput("n",   label = "Sample Size", value = 0),
  numericInput("mi",  label = "Pre Mean",    value = 0),
  numericInput("sdi", label = "Pre SD",      value = 0),
  numericInput("mii", label = "Post Mean",   value = 0),
  numericInput("r",   label = "Reliability", value = 0, min = -1, max = 1),
  verbatimTextOutput("es"),
  verbatimTextOutput("rank"),
  #verbatimTextOutput("k"),
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
  rank <-         reactive(lookup %>% filter(es == d.lookup() & setting == setting() & group == group()) %>% distinct(2, 3, .keep_all = T) %>% select(final.rank))
  k            <- reactive(ks %>% filter(setting == setting() & group == group()) %>% select(k) %>% as.character())

  # output$d.unadjusted <- renderText(paste0("Cohen's d (unadjusted) = ", d.unadjusted()))
  # output$estr <-         renderText(paste0("Reliability estimate = ", estr()))
  # output$d.adjusted <-   renderText(paste0("Adjusted d = ", d.adjusted()))
  # output$var <-     renderText(paste0("Variance = ", var()))
  # output$ci.lb <-   renderText(paste0("CI lb= ", ci.lb()))
  # output$ci.ub <-   renderText(paste0("CI ub = ", ci.ub()))
  output$es <-   renderText(paste0("The Cohen's d effect size is ", d.adjusted.2(),
                                   " (95% CI = ", ci.lb(), " to ", ci.ub(), ")."))
  output$rank <- renderText(paste0("This is the ", rank(), " (this is based on ", k(), " studies)."))

}
shinyApp(ui, server)

