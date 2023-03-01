library(shiny)
library(tidyverse)
library(gridExtra)
library(showtext)
library(ggtext)
library(ggpubr)
library(bslib)


primary_color = "grey"
main.title.size = 18
subtitle.size = 15
plot.text.size = 14
axis.text = 14
background_color = "white"
tbody.style = tbody_style(color = "black",
                          fill = c("white", "white"), hjust=0.1, x=0.1)
lay <- rbind(c(1,1),
             c(2,3))

 lookup <- read.csv("data/look.up.csv") %>%
   mutate(es = format(es, nsmall = 2)) %>%
   mutate(es = stringr::str_replace(es, fixed(" "), "")) %>%
   distinct() %>%
    mutate(group = recode(group, "general" = "other"),
           group = str_to_title(group),
           setting = str_to_title(setting)
    )

 norm.datasets <- read.csv("data/norm.datsets.csv")  %>%
   mutate(group = recode(group, "general" = "other")) %>%
   mutate(group = recode(group, "general" = "other"),
          group = str_to_title(group),
          setting = str_to_title(setting)
   )

 ks <-
   data.frame(
     group =   rep(c("Depression", "Anxiety", "Other"), 3),
     setting = c(rep("Outpatient", 3), rep("Inpatient", 3), rep("All", 3)),
     k = c(121, 75, 153,
           16, 9, 28,
           140, 84, 184)
   )

 showtext::showtext_auto()

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  theme = bslib::bs_theme(bootswatch = 'readable'),
  titlePanel("Benchmarks for Psychological Interventions in Routine Practice"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Service Name", value = "your service"),
  selectInput("group",     label = "Outcome Type", choices = c("Depression", "Anxiety", "Other")),
  selectInput("setting",   label = "Service Setting", choices = c("Outpatient", "Inpatient", "All")),
  selectInput("direction", label = "Intended Direction of Change", choices = c("Scores Decrease", "Scores Increase")),
  numericInput("n",   label = "Sample Size", value = 0),
  numericInput("mi",  label = "Pre Mean",    value = 0),
  numericInput("sdi", label = "Pre SD",      value = 0),
  numericInput("mii", label = "Post Mean",   value = 0),
  numericInput("r",   label = "Pre-Post Treatment Correlation*", value = 0.5, min = -1, max = 1),
  p("*Ths is the correlation between pre-treatment and post-treatment measure scores. If this is unknown then we suggest using r = 0.50.", style = "font-family: 'times'; font-si16pt"),
  submitButton("Calculate"),
    ),
  mainPanel(
    h4("Purpose"),
    p("This shiny app calculates a pre-post Cohen's d effect-size using the approach provided by Minami et al., (2008) before comparing it to a large set of available benchmarks for routine clinical services (Gaskell et al., 2023).", style = "font-family: 'times'; font-si16pt"),
    br(),
    verbatimTextOutput("es"),
    verbatimTextOutput("rank"),
  plotOutput("plot", width = "1000px", height = "600px"),
  tableOutput("tbl"),
  tableOutput("cit"),
  #downloadButton("export", label = "Download"),
  downloadButton('export', 'Download', class = "butt"),
  tags$head(tags$style(".butt{background-color:green;} .butt{color: white;}")),
  h4("Citation"),
  p("If using this shiny app for academic purposes then please cite the original article.", style = "font-family: 'times'; font-si16pt"),
  strong("Gaskell, C., Simmonds-Buckley, M., Kellett, S., Stockton, C., Somerville, E,. Rogerson E., & Delgadillo, J. (2023)."), em(" The effectiveness of psychological interventions delivered in routine practice: Systematic review and meta-analysis. Administration and Policy in Mental Health and Mental Health Services Research. 50, 43–57. https://doi.org/10.1007/s10488-022-01225-y", style = "font-family: 'times'; font-si16pt"),
  p(),
  strong("Minami, T., Serlin, R. C., Wampold, B. E., Kircher, J. C., & Brown, G. J. (2008)."), em(" Using clinical trials to benchmark effects produced in clinical practice. Quality and Quantity, 42(4), 513. https://doi.org/10.1007/s11135-006-9057-z", style = "font-family: 'times'; font-si16pt"),

    )
  )


)
server <- function(input, output, session) {
  service.name <- reactive(input$name)
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

  output$es <-   renderText(ifelse(is.nan(d.adjusted.2()),
                                   paste0("The Cohen's d effect size for ", service.name()," is..."),
                                   paste0("The Cohen's d effect size for ", service.name()," is ", d.adjusted.2(), " (95% CI = ", ci.lb(), " to ", ci.ub(), ")."
                                                 )
  )
                            )

  output$rank <- renderText(ifelse(
    lookup %>% filter(es == d.lookup() & setting == setting() & group == group()) %>% distinct(2, 3, .keep_all = T) %>% select(final.rank) %>% nrow() == 0,
    "This is the ... percentile rank.", paste0("This is the ", rank(), " (based on ", k(), " studies)."))
  )
  output$plot <- renderPlot({

    vals$plt1 <-
    norm.datasets %>%
      filter(setting == setting() & group == group()) %>%
      mutate(reference = "benchmarks") %>%
      add_row(es = d.adjusted.2(), reference = "study", n = input$n, ci.lb = ci.lb(), ci.ub = ci.ub()) %>%
      arrange(es) %>%
      tibble::rowid_to_column("ID") %>%
      mutate(effective.group =
               case_when(
                 centile <= 25 ~ "Lower (≤ 25th percentile)",
                 centile >= 75 ~ "Upper (≥ 75th percentile)",
                 centile >= 25 ~ "Middle (25th-75th percentile)",
                 TRUE ~ paste(service.name())
               )) %>%
      mutate(effective.group = factor(effective.group, levels = c("Lower (≤ 25th percentile)", "Middle (25th-75th percentile)", "Upper (≥ 75th percentile)", paste(service.name())))) %>%
      ggplot(aes(x = ID, y = es, col = effective.group)) +
      geom_point()  +
      geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub)) +
      theme_bw(base_size = 14)  +
      scale_x_continuous(expand = c(0.01, 0.01)) +
      labs(title = paste("Forest plot of effect sizes (and 95% CI) for", group(), "outcomes in", setting(), "settings."),
           subtitle = paste("With comparisons to ", service.name(),
                            " (d = ",
                            ifelse(is.nan(d.adjusted.2()), "__", d.adjusted.2()),
                            ", 95% CI = ",
                            ifelse(is.nan(ci.lb()), "__", ci.lb()),
                            " to ",
                            ifelse(is.nan(ci.ub()), "__", ci.ub()),
                            ", ",
                            ifelse(lookup %>% filter(es == d.lookup() & setting == setting() & group == group()) %>% distinct(2, 3, .keep_all = T) %>% select(final.rank) %>% nrow() == 0,
                              "This is the __ percentile rank.", rank()),
                            #rank(),
                            ").",
                            sep = ""), x = "Participating Services", y = "Cohen's d", col = NULL) +
      theme(legend.position = "bottom",
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      theme(plot.title.position = "plot")  +
      theme(
        plot.background = element_rect(fill = background_color),
        strip.background =element_rect(fill= primary_color),
        strip.text = element_text(colour = 'white', face = "bold",
                                  size = 12),
        axis.line = element_line(colour = primary_color, size = 1,
                                 linetype = "solid"),
        axis.text = element_text(size = axis.text),
        axis.title = element_text(size = plot.text.size),
        plot.title = element_text(size = main.title.size, face = "bold"),
        plot.subtitle = element_text(size = subtitle.size),
      )

    vals$plt1
  })

  ## output table
  output$tbl <- renderTable({

    tbl_df <- data.frame(
      Field = c("Service", "Date", "Time",
                "Pre Mean", "Pre SD", "Post Mean", "Pre-Post Treatment Correlation",
                "Cohen's d", "95% CI", "Rank"),
      Input = c(service.name(), paste(format(Sys.time(), "%x")), paste(format(Sys.time(), "%X")),
                input$mi , input$sdi , input$mii, input$r,
                d.adjusted.2(), paste(ci.lb(), " to ", ci.ub()), paste(rank()))
    )

    # store table for printing
    vals$tbl <- ggtexttable(tbl_df,
                            rows = NULL,
                            cols = c(NULL, NULL),
                            theme = ttheme(
                              colnames.style = colnames_style(color = "white", fill = "#8cc257"),
                              #tbody.style = tbody.style
                            )

    )
    # return table for viewing
    vals$tbl_df

  })




  ## output table
  output$cit <- renderTable({

    cit_df <- data.frame(
      type = c("Benchmarks & Shiny App:", "Effect-size Calculation:"),
      citation = c("Gaskell et al., (2023). https://doi.org/10.1007/s10488-022-01225-y",
                   "Minami et al., (2008). https://doi.org/10.1007/s11135-006-9057-z"
    ))

    # store table for printing
    vals$cit <- ggtexttable(cit_df,
                            rows = NULL,
                            cols = c(NULL, NULL),
                            theme = ttheme(
                              colnames.style = colnames_style(color = "white", fill = "#8cc257"),
                              tbody.style = tbody.style
                            )

    )
    # return table for viewing
    vals$cit_df

  })





  ## The element vals will store all plots and tables
  vals <- reactiveValues(plt1=NULL,
                         cit_df=NULL,
                         tbl=NULL
                         )


  ## clicking on the export button will generate a pdf file
  ## containing all stored plots and tables
  output$export = downloadHandler(
    filename = function() {"plots.pdf"},
    content = function(file) {
      pdf(file, onefile = TRUE, width = 14, height = 12)
      grid.arrange(vals$plt1,
                   vals$tbl,
                   vals$cit,
                   #nrow = 2,
                   #ncol = 1,
                   layout_matrix = lay)

      dev.off()
    })


}
shinyApp(ui, server)

