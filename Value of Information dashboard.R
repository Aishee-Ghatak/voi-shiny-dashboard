library(shiny)
library(bslib)
library(ggplot2)
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
        max-width: 1400px;
        margin: 0 auto;
        padding: 16px 0 24px 0;
      }
      .hero,
      .panel-card,
      .stat-card,
      .input-block {
        background: white;
        border: 1px solid rgba(31, 78, 121, 0.08);
        border-radius: 18px;
        box-shadow: 0 8px 24px rgba(18, 38, 63, 0.05);
      }
      .hero {
        padding: 18px 20px;
        margin-bottom: 16px;
      }
      .hero h2 {
        margin-top: 0;
        margin-bottom: 6px;
        font-weight: 750;
      }
      .hero p {
        margin-bottom: 0;
        color: #5d6b7a;
      }
      .topbar {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 12px;
      }
      .hero-actions {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 12px;
      }
      .theme-switch {
        display: flex;
        align-items: center;
        gap: 6px;
        white-space: nowrap;
        color: #5d6b7a;
        font-size: 0.92rem;
        margin-top: 4px;
      }
      .theme-switch .shiny-input-container,
      .theme-switch .checkbox {
        margin: 0 !important;
      }
      .theme-switch label {
        margin-bottom: 0;
        font-weight: 600;
      }
      .panel-card {
        padding: 16px;
        margin-bottom: 16px;
      }
      .panel-card h4 {
        margin-top: 0;
        margin-bottom: 8px;
        font-weight: 750;
      }
      .panel-card .small-note,
      .small-note {
        color: #6c7a88;
        font-size: 0.95rem;
      }
      .input-scroll {
        max-height: calc(100vh - 260px);
        overflow-y: auto;
        overflow-x: hidden;
        padding-right: 6px;
      }
      .input-block {
        padding: 12px 12px 8px 12px;
        margin-bottom: 10px;
        background: #fbfdff;
      }
      .input-block h5 {
        margin-top: 0;
        margin-bottom: 4px;
        font-weight: 700;
        font-size: 1rem;
      }
      .input-block p {
        color: #6c7a88;
        margin-bottom: 8px;
        font-size: 0.88rem;
      }
      .compact-row {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
      }
      .compact-half {
        flex: 1 1 0;
        min-width: 0;
      }
      .compact-half .form-group,
      .compact-half .shiny-input-container {
        margin-bottom: 0;
      }
      .stat-card {
        padding: 14px 16px;
        min-height: 110px;
      }
      .stat-title {
        font-size: 0.82rem;
        color: #6c7a88;
        margin-bottom: 6px;
        text-transform: uppercase;
        letter-spacing: 0.05em;
      }
      .stat-value {
        font-size: 1.9rem;
        line-height: 1.1;
        font-weight: 750;
        color: #1f4e79;
      }
      .stat-subtle {
        font-size: 0.88rem;
        color: #6c7a88;
        margin-top: 6px;
      }
      .btn-primary,
      .btn-outline-secondary {
        border-radius: 999px;
      }
      .nav-tabs > li > a {
        border-radius: 14px 14px 0 0 !important;
      }
      body.dark-mode {
        background: linear-gradient(180deg, #0b1220 0%, #111827 100%);
        color: #e5e7eb;
      }
      body.dark-mode .hero,
      body.dark-mode .panel-card,
      body.dark-mode .stat-card,
      body.dark-mode .input-block {
        background: #111827;
        border-color: rgba(148, 163, 184, 0.18);
        box-shadow: 0 10px 24px rgba(0, 0, 0, 0.28);
        color: #e5e7eb;
      }
      body.dark-mode .hero p,
      body.dark-mode .small-note,
      body.dark-mode .input-block p,
      body.dark-mode .stat-title,
      body.dark-mode .stat-subtle {
        color: #cbd5e1;
      }
      body.dark-mode .stat-value,
      body.dark-mode .hero h2,
      body.dark-mode .panel-card h4,
      body.dark-mode .input-block h5,
      body.dark-mode .theme-switch {
        color: #f8fafc;
      }
      body.dark-mode .form-control,
      body.dark-mode .selectize-input,
      body.dark-mode .selectize-dropdown,
      body.dark-mode .shiny-input-container input,
      body.dark-mode .shiny-input-container select {
        background-color: #0f172a !important;
        color: #e5e7eb !important;
        border-color: rgba(148, 163, 184, 0.25) !important;
      }
      body.dark-mode .well {
        background: #111827;
        color: #e5e7eb;
      }
      body.dark-mode .nav-tabs > li > a {
        color: #e5e7eb;
      }
      body.dark-mode .btn-outline-secondary {
        color: #e5e7eb;
        border-color: rgba(148, 163, 184, 0.35);
      }
      "
    )),
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('setDarkMode', function(enabled) {
        document.body.classList.toggle('dark-mode', enabled);
      });
      "
    ))
  ),
  div(
    class = "app-shell",
    div(
      class = "hero",
      div(
        class = "topbar",
        div(
          div(
            h2("Value of Information Explorer"),
            p("An interactive dashboard for exploring how uncertainty affects decision value."),
            div(
              class = "hero-actions",
              actionButton("summary_btn", "How it works", class = "btn btn-primary btn-sm"),
              actionButton("reset", "Load toy example", class = "btn btn-outline-secondary btn-sm")
            )
          ),
          div(
            class = "theme-switch",
            checkboxInput("dark_mode", "Dark mode", value = FALSE)
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        div(
          class = "panel-card",
          h4("Inputs"),
          p(class = "small-note", "Adjust the base case and uncertainty. The panel scrolls if needed."),
          div(
            class = "input-scroll",
            div(
              class = "input-block",
              h5("Comparator"),
              p("Standard care"),
              div(
                class = "compact-row",
                div(class = "compact-half", numericInput("cost_soc", "Cost", value = 10000, min = 0, step = 100)),
                div(class = "compact-half", numericInput("qaly_soc", "QALYs", value = 5.00, min = 0, step = 0.01))
              )
            ),
            div(
              class = "input-block",
              h5("Intervention"),
              p("New treatment"),
              div(
                class = "compact-row",
                div(class = "compact-half", numericInput("cost_new", "Cost", value = 16000, min = 0, step = 100)),
                div(class = "compact-half", numericInput("qaly_new", "QALYs", value = 5.18, min = 0, step = 0.01))
              )
            ),
            div(
              class = "input-block",
              h5("Uncertainty"),
              p("Around incremental cost and incremental QALYs"),
              div(
                class = "compact-row",
                div(class = "compact-half", numericInput("sd_cost", "SD cost", value = 1500, min = 0, step = 50)),
                div(class = "compact-half", numericInput("sd_qaly", "SD QALY", value = 0.08, min = 0, step = 0.01))
              ),
              sliderInput("rho", "Correlation", min = -0.9, max = 0.9, value = 0, step = 0.1)
            ),
            div(
              class = "input-block",
              h5("Display"),
              p("How you want to view the outputs"),
              radioButtons("scope", label = NULL, choices = c("Per patient", "Total population"), selected = "Per patient", inline = TRUE),
              div(
                class = "compact-row",
                div(class = "compact-half", numericInput("pop", "Population", value = 10000, min = 1, step = 100)),
                div(class = "compact-half", numericInput("n_sim", "Draws", value = 5000, min = 500, step = 500))
              ),
              sliderInput("wtp", "WTP (£/QALY)", min = 10000, max = 100000, value = 50000, step = 1000)
            )
          )
        )
      ),
      column(
        width = 8,
        fluidRow(
          column(
            width = 4,
            div(
              class = "stat-card",
              div(class = "stat-title", "Expected INMB"),
              div(class = "stat-value", textOutput("exp_inmb", inline = TRUE)),
              div(class = "stat-subtle", textOutput("scope_note1", inline = TRUE))
            )
          ),
          column(
            width = 4,
            div(
              class = "stat-card",
              div(class = "stat-title", "EVPI"),
              div(class = "stat-value", textOutput("evpi_value", inline = TRUE)),
              div(class = "stat-subtle", textOutput("evpi_note", inline = TRUE))
            )
          ),
          column(
            width = 4,
            div(
              class = "stat-card",
              div(class = "stat-title", "Probability cost-effective"),
              div(class = "stat-value", textOutput("prob_ce", inline = TRUE)),
              div(class = "stat-subtle", "At the chosen WTP")
            )
          )
        ),
        div(
          class = "panel-card",
          tabsetPanel(
            tabPanel(
              "Results",
              fluidRow(
                column(width = 6, plotOutput("nmb_plot", height = 320)),
                column(width = 6, plotOutput("wtp_curve", height = 320))
              )
            ),
            tabPanel(
              "Explore",
              p(class = "small-note", "Move the inputs around and watch the uncertainty cloud and decision boundary shift."),
              fluidRow(
                column(width = 12, plotOutput("scatter_plot", height = 360))
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$dark_mode, {
    session$sendCustomMessage("setDarkMode", isTRUE(input$dark_mode))
  }, ignoreInit = FALSE)
  
  observeEvent(input$reset, {
    updateCheckboxInput(session, "dark_mode", value = FALSE)
    updateRadioButtons(session, "scope", selected = "Per patient")
    updateNumericInput(session, "pop", value = 10000)
    updateNumericInput(session, "n_sim", value = 5000)
    updateNumericInput(session, "cost_soc", value = 10000)
    updateNumericInput(session, "qaly_soc", value = 5.00)
    updateNumericInput(session, "cost_new", value = 16000)
    updateNumericInput(session, "qaly_new", value = 5.18)
    updateNumericInput(session, "sd_cost", value = 1500)
    updateNumericInput(session, "sd_qaly", value = 0.08)
    updateSliderInput(session, "rho", value = 0)
    updateSliderInput(session, "wtp", value = 50000)
  })
  
  observeEvent(input$summary_btn, {
    showModal(modalDialog(
      title = "How the calculation works",
      easyClose = TRUE,
      size = "m",
      tags$p("The app starts with a base case for standard care and the new treatment."),
      tags$p("It then adds uncertainty around incremental cost and incremental QALYs for the new treatment."),
      tags$div(
        style = "padding: 10px 14px; background: #f6f9fc; border-radius: 12px; margin-bottom: 10px;",
        tags$code("INMB = WTP x (QALYsnew - QALYsstandard) - (Costnew - Coststandard)")
      ),
      tags$p("The simulation repeats that calculation many times, then averages the results to estimate expected value and EVPI."),
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
    
    data.frame(delta_cost = delta_cost, delta_qaly = delta_qaly, inmb = inmb)
  })
  
  calc_vals <- reactive({
    df <- sim_df()
    scope_mult <- if (input$scope == "Total population") input$pop else 1
    alt_mult <- if (input$scope == "Total population") 1 else input$pop
    
    expected_inmb_pp <- mean(df$inmb)
    evpi_pp <- mean(pmax(df$inmb, 0)) - max(expected_inmb_pp, 0)
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
      expected_inmb = expected_inmb_pp * scope_mult,
      evpi = evpi_pp * scope_mult,
      evpi_pp = evpi_pp,
      evpi_alt = evpi_pp * alt_mult,
      prob_ce = prob_ce,
      curve_df = curve_df
    )
  })
  
  pretty_money <- function(x) paste0("£", comma(round(x, 0)))
  pretty_pct <- function(x) paste0(round(100 * x, 1), "%")
  
  output$scope_note1 <- renderText({
    if (input$scope == "Total population") paste0("Total for ", comma(input$pop), " people") else "Per patient"
  })
  
  output$evpi_note <- renderText({
    if (input$scope == "Total population") {
      paste0("Per patient equivalent: ", pretty_money(calc_vals()$evpi_pp))
    } else {
      paste0("Population equivalent: ", pretty_money(calc_vals()$evpi_alt))
    }
  })
  
  output$exp_inmb <- renderText({
    pretty_money(calc_vals()$expected_inmb)
  })
  
  output$evpi_value <- renderText({
    pretty_money(calc_vals()$evpi)
  })
  
  output$prob_ce <- renderText({
    pretty_pct(calc_vals()$prob_ce)
  })
  
  plot_theme <- reactive({
    if (isTruthy(input$dark_mode)) {
      theme_minimal(base_size = 14) +
        theme(
          plot.background = element_rect(fill = "#111827", colour = NA),
          panel.background = element_rect(fill = "#111827", colour = NA),
          legend.background = element_rect(fill = "#111827", colour = NA),
          text = element_text(colour = "#E5E7EB"),
          axis.text = element_text(colour = "#E5E7EB"),
          axis.title = element_text(colour = "#E5E7EB"),
          plot.title = element_text(colour = "#F8FAFC", face = "bold", size = 15),
          legend.text = element_text(colour = "#E5E7EB"),
          legend.title = element_text(colour = "#E5E7EB"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
    } else {
      theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 15),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
    }
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
      plot_theme()
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
      plot_theme()
  })
  
  output$scatter_plot <- renderPlot({
    df <- calc_vals()$df
    df$status <- ifelse(df$inmb > 0, "Cost-effective", "Not cost-effective")
    
    boundary <- data.frame(delta_qaly = seq(min(df$delta_qaly), max(df$delta_qaly), length.out = 100))
    boundary$delta_cost <- input$wtp * boundary$delta_qaly
    
    ggplot(df, aes(x = delta_qaly, y = delta_cost)) +
      geom_point(aes(color = status), alpha = 0.32, size = 1.5) +
      geom_line(
        data = boundary,
        aes(x = delta_qaly, y = delta_cost),
        inherit.aes = FALSE,
        linetype = "dashed",
        colour = "#1F4E79",
        linewidth = 1
      ) +
      geom_hline(yintercept = 0, linetype = "dotted", colour = "#7a8693") +
      geom_vline(xintercept = 0, linetype = "dotted", colour = "#7a8693") +
      scale_color_manual(
        values = c("Cost-effective" = "#2C7FB8", "Not cost-effective" = "#E07A5F"),
        name = NULL
      ) +
      labs(
        x = "Incremental QALYs",
        y = "Incremental cost (£)",
        title = "Simulated uncertainty around the new treatment"
      ) +
      plot_theme()
  })
}

shinyApp(ui, server)
