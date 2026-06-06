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
          p("An interactive dashboard for exploring how uncertainty affects decision value."),
          div(style = "margin-top: 14px; display: flex; gap: 10px; flex-wrap: wrap;",
              actionButton("summary_btn", "How it works", class = "btn btn-primary"),
              actionButton("reset", "Load toy example", class = "btn btn-outline-secondary")
          )
        ),
        column(
          width = 4,
          div(style = "display:flex; justify-content:flex-end; align-items:flex-start; height:100%;",
              tags$div(class = "small-note", "Enter a base case, add uncertainty, and see what VOI says.")
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
          helpText("Start with a base case, then add uncertainty around cost and QALYs."),
          sliderInput("wtp", "WTP threshold (£ per QALY)", min = 10000, max = 100000, value = 50000, step = 1000),
          numericInput("pop", "Population size", value = 10000, min = 1, step = 100),
          numericInput("n_sim", "Simulation draws", value = 5000, min = 500, step = 500),
          hr(),
          h5("Base case: standard care"),
          numericInput("cost_soc", "Cost", value = 10000, min = 0, step = 100),
          numericInput("qaly_soc", "QALYs", value = 5.00, min = 0, step = 0.01),
          hr(),
          h5("Base case: new treatment"),
          numericInput("cost_new", "Cost", value = 16000, min = 0, step = 100),
          numericInput("qaly_new", "QALYs", value = 5.18, min = 0, step = 0.01),
          hr(),
          h5("Uncertainty around the new treatment"),
          numericInput("sd_cost", "SD of incremental cost", value = 1500, min = 0, step = 50),
          numericInput("sd_qaly", "SD of incremental QALYs", value = 0.08, min = 0, step = 0.01),
          sliderInput("rho", "Correlation between cost and QALY uncertainty", min = -0.9, max = 0.9, value = 0, step = 0.1)
        )
      ),
      column(
        width = 8,
        fluidRow(
          column(width = 4,
                 div(class = "stat-card",
                     div(class = "stat-title", "Expected INMB"),
                     div(class = "stat-value", textOutput("exp_inmb", inline = TRUE)),
                     div(class = "stat-subtle", "Average value under uncertainty")
                 )),
          column(width = 4,
                 div(class = "stat-card",
                     div(class = "stat-title", "EVPI per patient"),
                     div(class = "stat-value", textOutput("evpi_pp", inline = TRUE)),
                     div(class = "stat-subtle", "Value of perfect information")
                 )),
          column(width = 4,
                 div(class = "stat-card",
                     div(class = "stat-title", "P(New treatment cost-effective)"),
                     div(class = "stat-value", textOutput("prob_ce", inline = TRUE)),
                     div(class = "stat-subtle", "At the chosen WTP")
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
                    column(width = 6, plotOutput("nmb_plot", height = 320)),
                    column(width = 6, plotOutput("wtp_curve", height = 320))
                  ),
                  fluidRow(
                    column(width = 12, h4("Simulation summary"), DTOutput("summary_tbl"))
                  )
                ),
                tabPanel(
                  "Explore",
                  p(class = "small-note", "Try changing the base case or uncertainty inputs and watch the decision metrics move."),
                  fluidRow(
                    column(width = 12, plotOutput("dist_plot", height = 360))
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
    updateNumericInput(session, "n_sim", value = 5000)
    updateNumericInput(session, "cost_soc", value = 10000)
    updateNumericInput(session, "qaly_soc", value = 5.00)
    updateNumericInput(session, "cost_new", value = 16000)
    updateNumericInput(session, "qaly_new", value = 5.18)
    updateNumericInput(session, "sd_cost", value = 1500)
    updateNumericInput(session, "sd_qaly", value = 0.08)
    updateSliderInput(session, "rho", value = 0)
  })
  
  observeEvent(input$summary_btn, {
    showModal(modalDialog(
      title = "How the calculation works",
      easyClose = TRUE,
      size = "m",
      tags$p("The app starts with a base case for standard care and a new treatment."),
      tags$p("It then adds uncertainty around the incremental cost and incremental QALYs for the new treatment."),
      tags$div(style = "padding: 10px 14px; background: #f6f9fc; border-radius: 12px; margin-bottom: 10px;",
               tags$code("INMB = WTP x (QALYsnew - QALYsstandard) - (Costnew - Coststandard)")),
      tags$p("By simulating many possible values, the app estimates the average value of the new treatment and how much perfect information would be worth."),
      tags$p("In plain language: EVPI is the value of knowing the truth before deciding."),
      footer = modalButton("Close")
    ))
  })
  
  sim_df <- reactive({
    validate(
      need(input$sd_cost >= 0, "SD of incremental cost must be non-negative."),
      need(input$sd_qaly >= 0, "SD of incremental QALYs must be non-negative."),
      need(input$n_sim >= 500, "Use at least 500 draws for a stable estimate.")
    )
    
    set.seed(123)
    
    delta_cost_mean <- input$cost_new - input$cost_soc
    delta_qaly_mean <- input$qaly_new - input$qaly_soc
    
    if (input$sd_cost == 0 && input$sd_qaly == 0) {
      delta_cost <- rep(delta_cost_mean, input$n_sim)
      delta_qaly <- rep(delta_qaly_mean, input$n_sim)
    } else {
      z1 <- rnorm(input$n_sim)
      z2 <- rnorm(input$n_sim)
      delta_cost <- delta_cost_mean + input$sd_cost * z1
      delta_qaly <- delta_qaly_mean + input$sd_qaly * (input$rho * z1 + sqrt(1 - input$rho^2) * z2)
    }
    
    delta_cost <- pmax(delta_cost, -0.99 * input$cost_soc)
    inmb <- input$wtp * delta_qaly - delta_cost
    
    data.frame(
      delta_cost = delta_cost,
      delta_qaly = delta_qaly,
      inmb = inmb
    )
  })
  
  calc_vals <- reactive({
    df <- sim_df()
    expected_inmb <- mean(df$inmb)
    evpi <- mean(pmax(df$inmb, 0)) - max(expected_inmb, 0)
    prob_ce <- mean(df$inmb > 0)
    
    wtp_seq <- seq(10000, 100000, by = 1000)
    curve_df <- do.call(rbind, lapply(wtp_seq, function(w) {
      inmb_w <- w * df$delta_qaly - df$delta_cost
      data.frame(
        WTP = w,
        Expected_INMB = mean(inmb_w),
        EVPI = mean(pmax(inmb_w, 0)) - max(mean(inmb_w), 0),
        Prob_CE = mean(inmb_w > 0)
      )
    }))
    
    list(
      df = df,
      expected_inmb = expected_inmb,
      evpi = evpi,
      prob_ce = prob_ce,
      pop_evpi = evpi * input$pop,
      curve_df = curve_df
    )
  })
  
  pretty_money <- function(x) paste0("£", comma(round(x, 0)))
  pretty_pct <- function(x) paste0(round(100 * x, 1), "%")
  
  output$exp_inmb <- renderText({
    pretty_money(calc_vals()$expected_inmb)
  })
  
  output$evpi_pp <- renderText({
    pretty_money(calc_vals()$evpi)
  })
  
  output$prob_ce <- renderText({
    pretty_pct(calc_vals()$prob_ce)
  })
  
  output$summary_tbl <- renderDT({
    df <- calc_vals()$df
    datatable(
      data.frame(
        Metric = c("Expected INMB", "EVPI per patient", "Probability new treatment is cost-effective"),
        Value = c(
          pretty_money(calc_vals()$expected_inmb),
          pretty_money(calc_vals()$evpi),
          pretty_pct(calc_vals()$prob_ce)
        )
      ),
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE)
    )
  })
  
  output$nmb_plot <- renderPlot({
    df <- calc_vals()$df
    ggplot(df, aes(x = inmb)) +
      geom_histogram(bins = 40, fill = "#2C7FB8", alpha = 0.85, colour = "white") +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#7a8693") +
      geom_vline(xintercept = mean(df$inmb), linewidth = 1, colour = "#E07A5F") +
      labs(
        x = "Incremental NMB (£ per patient)",
        y = "Simulated draws",
        title = "Distribution of decision value"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank()
      )
  })
  
  output$wtp_curve <- renderPlot({
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
        legend.position = "bottom"
      )
  })
  
  output$dist_plot <- renderPlot({
    df <- calc_vals()$df
    plot_df <- df
    plot_df$status <- ifelse(plot_df$inmb > 0, "Cost-effective", "Not cost-effective")
    
    boundary <- data.frame(
      delta_qaly = seq(min(plot_df$delta_qaly), max(plot_df$delta_qaly), length.out = 100)
    )
    boundary$delta_cost <- input$wtp * boundary$delta_qaly
    
    ggplot(plot_df, aes(x = delta_qaly, y = delta_cost)) +
      geom_point(aes(color = status), alpha = 0.35, size = 1.4) +
      geom_line(data = boundary, aes(x = delta_qaly, y = delta_cost), inherit.aes = FALSE, linetype = "dashed", colour = "#1F4E79", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dotted", colour = "#7a8693") +
      geom_vline(xintercept = 0, linetype = "dotted", colour = "#7a8693") +
      scale_color_manual(values = c("Cost-effective" = "#2C7FB8", "Not cost-effective" = "#E07A5F"), name = NULL) +
      labs(
        x = "Incremental QALYs",
        y = "Incremental cost (£)",
        title = "Simulated uncertainty around the new treatment"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
  })
  
  output$calc_tbl <- renderDT({
    out <- data.frame(
      Metric = c(
        "Expected INMB",
        "EVPI per patient",
        "Population EVPI",
        "Probability new treatment is cost-effective"
      ),
      Value = c(
        pretty_money(calc_vals()$expected_inmb),
        pretty_money(calc_vals()$evpi),
        pretty_money(calc_vals()$pop_evpi),
        pretty_pct(calc_vals()$prob_ce)
      )
    )
    
    datatable(out, rownames = FALSE, options = list(dom = "t", paging = FALSE, searching = FALSE))
  })
}

shinyApp(ui, server)
