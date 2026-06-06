library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(scales)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1F4E79",
    secondary = "#5B6B7A",
    base_font = "system-ui"
  ),
  tags$head(
    tags$style(HTML(
      "
      body {
        background: linear-gradient(180deg, #f7f9fc 0%, #eef3f8 100%);
      }
      .app-shell {
        max-width: 1380px;
        margin: 0 auto;
      }
      .hero {
        background: white;
        border: 1px solid rgba(31, 78, 121, 0.08);
        border-radius: 20px;
        box-shadow: 0 10px 28px rgba(18, 38, 63, 0.06);
        padding: 22px 24px;
        margin-bottom: 18px;
      }
      .hero h2 {
        margin-top: 0;
        margin-bottom: 6px;
        font-weight: 700;
      }
      .hero p {
        margin-bottom: 0;
        color: #5d6b7a;
      }
      .panel-card {
        background: white;
        border: 1px solid rgba(31, 78, 121, 0.08);
        border-radius: 18px;
        box-shadow: 0 8px 24px rgba(18, 38, 63, 0.05);
        padding: 18px;
        margin-bottom: 18px;
      }
      .stat-card {
        border-radius: 18px;
        background: white;
        border: 1px solid rgba(31, 78, 121, 0.08);
        box-shadow: 0 8px 18px rgba(18, 38, 63, 0.05);
        padding: 16px 18px;
        min-height: 110px;
      }
      .stat-title {
        font-size: 0.88rem;
        color: #6c7a88;
        margin-bottom: 6px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }
      .stat-value {
        font-size: 2rem;
        line-height: 1.1;
        font-weight: 700;
        color: #1f4e79;
      }
      .stat-subtle {
        font-size: 0.92rem;
        color: #6c7a88;
        margin-top: 6px;
      }
      .small-note {
        color: #6c7a88;
        font-size: 0.95rem;
      }
      .well {
        background: #ffffff;
        border: 1px solid rgba(31, 78, 121, 0.08);
        border-radius: 16px;
        box-shadow: 0 8px 18px rgba(18, 38, 63, 0.04);
      }
      .shiny-input-container { margin-bottom: 10px; }
      .nav-tabs > li > a {
        border-radius: 14px 14px 0 0 !important;
      }
      .tab-content {
        background: transparent;
      }
      .btn-primary {
        border-radius: 999px;
        padding-left: 16px;
        padding-right: 16px;
      }
      .btn-outline-secondary {
        border-radius: 999px;
      }
      "
    ))
  ),
  div(
    class = "app-shell",
    div(
      class = "hero",
      fluidRow(
        column(
          width = 8,
          h2("Value of Information Explorer"),
          p("A small interactive dashboard for understanding how uncertainty, expected value, and EVPI fit together."),
          div(style = "margin-top: 14px; display: flex; gap: 10px; flex-wrap: wrap;",
              actionButton("summary_btn", "How it works", class = "btn btn-primary"),
              actionButton("reset", "Load toy example", class = "btn btn-outline-secondary")
          )
        ),
        column(
          width = 4,
          div(style = "display:flex; justify-content:flex-end; align-items:flex-start; height:100%;",
              tags$div(class = "small-note", "Play with the assumptions, see the decision move, and get a feel for what VOI is doing.")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        div(
          class = "panel-card",
          h4("Inputs", style = "margin-top: 0;"),
          helpText("Adjust the scenario assumptions to explore the decision."),
          sliderInput("wtp", "WTP threshold (£ per QALY)", min = 10000, max = 100000, value = 50000, step = 1000),
          numericInput("pop", "Population size", value = 10000, min = 1, step = 100),
          sliderInput("p_low", "Probability of low-effect scenario", min = 0, max = 1, value = 0.40, step = 0.01),
          hr(),
          h5("Standard care"),
          numericInput("cost_soc", "Cost", value = 10000, min = 0, step = 100),
          numericInput("qaly_soc", "QALYs", value = 5.00, min = 0, step = 0.01),
          hr(),
          h5("Scenario 1: low effect"),
          numericInput("cost_new_low", "New treatment cost", value = 16000, min = 0, step = 100),
          numericInput("qaly_new_low", "New treatment QALYs", value = 5.10, min = 0, step = 0.01),
          hr(),
          h5("Scenario 2: high effect"),
          numericInput("cost_new_high", "New treatment cost", value = 16000, min = 0, step = 100),
          numericInput("qaly_new_high", "New treatment QALYs", value = 5.25, min = 0, step = 0.01)
        )
      ),
      column(
        width = 8,
        fluidRow(
          column(width = 4,
                 div(class = "stat-card",
                     div(class = "stat-title", "Expected INMB"),
                     div(class = "stat-value", textOutput("exp_inmb", inline = TRUE)),
                     div(class = "stat-subtle", "Expected net value under current evidence")
                 )),
          column(width = 4,
                 div(class = "stat-card",
                     div(class = "stat-title", "EVPI per patient"),
                     div(class = "stat-value", textOutput("evpi_pp", inline = TRUE)),
                     div(class = "stat-subtle", "Value of removing uncertainty")
                 )),
          column(width = 4,
                 div(class = "stat-card",
                     div(class = "stat-title", "Population EVPI"),
                     div(class = "stat-value", textOutput("evpi_pop", inline = TRUE)),
                     div(class = "stat-subtle", "Population-level value")
                 ))
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "panel-card",
              tabsetPanel(
                tabPanel(
                  "Results",
                  fluidRow(
                    column(width = 6, plotOutput("scenario_plot", height = 320)),
                    column(width = 6, plotOutput("voi_curve", height = 320))
                  ),
                  fluidRow(
                    column(width = 12, h4("Scenario summary"), DTOutput("summary_tbl"))
                  )
                ),
                tabPanel(
                  "Explore",
                  p(class = "small-note", "Try moving the WTP threshold and scenario probabilities. Watch how the preferred option and EVPI respond."),
                  fluidRow(
                    column(width = 12, plotOutput("decision_chart", height = 360))
                  ),
                  fluidRow(
                    column(width = 12, DTOutput("calc_tbl"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(session, "wtp", value = 50000)
    updateNumericInput(session, "pop", value = 10000)
    updateSliderInput(session, "p_low", value = 0.40)
    updateNumericInput(session, "cost_soc", value = 10000)
    updateNumericInput(session, "qaly_soc", value = 5.00)
    updateNumericInput(session, "cost_new_low", value = 16000)
    updateNumericInput(session, "qaly_new_low", value = 5.10)
    updateNumericInput(session, "cost_new_high", value = 16000)
    updateNumericInput(session, "qaly_new_high", value = 5.25)
  })
  
  observeEvent(input$summary_btn, {
    showModal(modalDialog(
      title = "How the calculation works",
      easyClose = TRUE,
      size = "m",
      tags$p("The app compares a new treatment with standard care under two possible evidence scenarios."),
      tags$p(HTML("For each scenario, it calculates <b>incremental net monetary benefit</b>:")),
      tags$div(style = "padding: 10px 14px; background: #f6f9fc; border-radius: 12px; margin-bottom: 10px;",
               tags$code("INMB = WTP x (QALYsnew - QALYsstandard) - (Costnew - Coststandard)")),
      tags$p("Then it averages across the scenario probabilities to get expected INMB."),
      tags$p("EVPI compares that expected value with the value you would get if uncertainty were fully resolved."),
      tags$p("In plain language: EVPI is the value of knowing the truth before deciding."),
      footer = modalButton("Close")
    ))
  })
  
  scenario_df <- reactive({
    p_low <- input$p_low
    p_high <- 1 - p_low
    
    df <- data.frame(
      Scenario = c("Low effect", "High effect"),
      Probability = c(p_low, p_high),
      Cost_new = c(input$cost_new_low, input$cost_new_high),
      QALY_new = c(input$qaly_new_low, input$qaly_new_high),
      Cost_soc = c(input$cost_soc, input$cost_soc),
      QALY_soc = c(input$qaly_soc, input$qaly_soc)
    )
    
    df$Delta_Cost <- df$Cost_new - df$Cost_soc
    df$Delta_QALY <- df$QALY_new - df$QALY_soc
    df$INMB <- input$wtp * df$Delta_QALY - df$Delta_Cost
    df$Best_choice <- ifelse(df$INMB > 0, "New treatment", "Standard care")
    df$Best_INMB <- pmax(df$INMB, 0)
    df
  })
  
  calc_vals <- reactive({
    df <- scenario_df()
    expected_inmb <- sum(df$Probability * df$INMB)
    expected_with_perfect_info <- sum(df$Probability * df$Best_INMB)
    evpi <- expected_with_perfect_info - expected_inmb
    
    wtp_seq <- seq(10000, 100000, by = 1000)
    curve_df <- do.call(rbind, lapply(wtp_seq, function(w) {
      inmb_s <- w * df$Delta_QALY - df$Delta_Cost
      data.frame(
        WTP = w,
        Expected_INMB = sum(df$Probability * inmb_s),
        EVPI = sum(df$Probability * pmax(inmb_s, 0)) - sum(df$Probability * inmb_s)
      )
    }))
    
    list(
      df = df,
      expected_inmb = expected_inmb,
      evpi = evpi,
      pop_evpi = evpi * input$pop,
      curve_df = curve_df
    )
  })
  
  pretty_money <- function(x) paste0("£", comma(round(x, 0)))
  
  output$exp_inmb <- renderText({
    pretty_money(calc_vals()$expected_inmb)
  })
  
  output$evpi_pp <- renderText({
    pretty_money(calc_vals()$evpi)
  })
  
  output$evpi_pop <- renderText({
    pretty_money(calc_vals()$pop_evpi)
  })
  
  output$summary_tbl <- renderDT({
    df <- calc_vals()$df
    datatable(
      df[, c("Scenario", "Probability", "Delta_Cost", "Delta_QALY", "INMB", "Best_choice")],
      rownames = FALSE,
      options = list(dom = "t", pageLength = 5),
      colnames = c("Scenario", "Probability", "ΔCost", "ΔQALY", "INMB", "Preferred option")
    ) |>
      formatPercentage("Probability", 1) |>
      formatCurrency(c("Delta_Cost", "INMB"), currency = "£", digits = 0) |>
      formatRound("Delta_QALY", 2)
  })
  
  output$scenario_plot <- renderPlot({
    df <- calc_vals()$df
    df$Label <- ifelse(df$INMB > 0, "Favour new treatment", "Favour standard care")
    
    ggplot(df, aes(x = Scenario, y = INMB, fill = Label)) +
      geom_col(width = 0.58, alpha = 0.95) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#7a8693") +
      geom_text(aes(label = pretty_money(INMB)), vjust = ifelse(df$INMB >= 0, -0.4, 1.2), size = 4.2, fontface = "bold") +
      scale_fill_manual(values = c("Favour new treatment" = "#2C7FB8", "Favour standard care" = "#E07A5F"), guide = "none") +
      labs(
        x = NULL,
        y = "Incremental NMB (£ per patient)",
        title = "Value by scenario"
      ) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.margin = margin(10, 18, 10, 10)
      )
  })
  
  output$voi_curve <- renderPlot({
    curve_df <- calc_vals()$curve_df
    current <- curve_df[which.min(abs(curve_df$WTP - input$wtp)), , drop = FALSE]
    
    ggplot(curve_df, aes(x = WTP)) +
      geom_line(aes(y = Expected_INMB, colour = "Expected INMB"), linewidth = 1.15) +
      geom_line(aes(y = EVPI, colour = "EVPI"), linewidth = 1.15) +
      geom_vline(xintercept = input$wtp, linetype = "dashed", colour = "#667085") +
      geom_point(data = current, aes(y = Expected_INMB), colour = "#2C7FB8", size = 2.4) +
      geom_point(data = current, aes(y = EVPI), colour = "#E07A5F", size = 2.4) +
      scale_colour_manual(
        values = c("Expected INMB" = "#2C7FB8", "EVPI" = "#E07A5F"),
        name = NULL
      ) +
      labs(
        x = "WTP threshold (£ per QALY)",
        y = "£ per patient",
        title = "How value changes with WTP"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box.margin = margin(0, 0, 0, 0),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  
  output$decision_chart <- renderPlot({
    df <- calc_vals()$df
    df_long <- rbind(
      data.frame(Scenario = df$Scenario, Metric = "New treatment", Value = df$INMB, stringsAsFactors = FALSE),
      data.frame(Scenario = df$Scenario, Metric = "Zero line", Value = 0, stringsAsFactors = FALSE)
    )
    
    ggplot(df, aes(x = Scenario, y = INMB, fill = Scenario)) +
      geom_col(width = 0.55, alpha = 0.95) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#7a8693") +
      geom_label(aes(label = paste0(ifelse(INMB > 0, "+", ""), pretty_money(INMB))),
                 vjust = ifelse(df$INMB >= 0, -0.4, 1.2),
                 label.size = 0,
                 fill = "white",
                 colour = "#1f2937",
                 size = 4.2,
                 fontface = "bold") +
      scale_fill_manual(values = c("Low effect" = "#9CC3E6", "High effect" = "#2C7FB8"), guide = "none") +
      labs(
        x = NULL,
        y = "Incremental NMB (£ per patient)",
        title = "Decision signal by scenario"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(face = "bold")
      )
  })
  
  output$calc_tbl <- renderDT({
    out <- data.frame(
      Metric = c(
        "Preferred option under current evidence",
        "Expected INMB",
        "EVPI per patient",
        "Population EVPI"
      ),
      Value = c(
        ifelse(calc_vals()$expected_inmb > 0, "New treatment", "Standard care"),
        pretty_money(calc_vals()$expected_inmb),
        pretty_money(calc_vals()$evpi),
        pretty_money(calc_vals()$pop_evpi)
      )
    )
    
    datatable(out, rownames = FALSE, options = list(dom = "t", paging = FALSE, searching = FALSE))
  })
}

shinyApp(ui, server)
