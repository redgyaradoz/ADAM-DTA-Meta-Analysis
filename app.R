library(shiny)
library(meta)
library(forestplot)
library(gridExtra)
library(grid)
library(readxl)
library(dplyr)
library(mada)
library(metafor)
library(shinythemes)
library(ggplot2)
library(tidyr) 

# ==============================================================================
# 0. DEMO DATA (Updated with 'StudyDesign' for Subgroup Analysis)
# ==============================================================================
demo_data <- data.frame(
  Author = c("Smith", "Jones", "Williams", "Brown", "Taylor", "Davies", "Evans", "Wilson"),
  Year   = c(2010, 2011, 2012, 2013, 2015, 2016, 2018, 2020),
  Region = c("Europe", "South-East Asia", "Europe", "Africa", "Europe", "South-East Asia", "Africa", "Europe"), # NEW COLUMN
  TP     = c(45, 120, 33, 25, 60, 95, 40, 150),
  FP     = c(10, 45, 5, 8, 15, 30, 12, 50),
  FN     = c(5, 15, 3, 12, 10, 20, 8, 25),
  TN     = c(100, 200, 50, 60, 120, 180, 90, 300),
  D1     = c("Low", "High", "Low", "Unclear", "Low", "Low", "High", "Low"),
  D2     = c("Low", "Low", "High", "Low", "Low", "Unclear", "Low", "Low"),
  D3     = c("Unclear", "Low", "Low", "High", "Low", "Low", "Low", "Low"),
  D4     = c("Low", "High", "Low", "Low", "Low", "Low", "Unclear", "High"),
  D5     = c("Low", "Low", "Low", "High", "Low", "Low", "Low", "Low"),
  D6     = c("Low", "Unclear", "Low", "Low", "High", "Low", "Low", "Low"),
  D7     = c("Low", "Low", "High", "Low", "Low", "Low", "Low", "Unclear")
)

# ==============================================================================
# 1. HELPER FUNCTIONS
# ==============================================================================

# --- Custom Traffic Light Plot (Manual ggplot) ---
plot_rob_traffic_manual <- function(data) {
  long_dat <- data %>%
    pivot_longer(cols = -Study, names_to = "Domain", values_to = "Risk") %>%
    mutate(
      Risk = factor(Risk, levels = c("Low", "Unclear", "High")),
      Domain = factor(Domain, levels = colnames(data)[-1]),
      Study = factor(Study, levels = rev(data$Study)),
      Symbol = case_when(
        Risk == "Low" ~ "+",
        Risk == "High" ~ "X",
        Risk == "Unclear" ~ "?",
        TRUE ~ "" 
      )
    )
  
  ggplot(long_dat, aes(x = Domain, y = Study)) +
    geom_point(aes(fill = Risk), shape = 21, size = 7, stroke = 0.5, color = "black") +
    geom_text(aes(label = Symbol), color = "black", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("Low" = "#009E73", "High" = "#D55E00", "Unclear" = "#F0E442")) +
    labs(title = "Risk of Bias Traffic Light Plot", x = "", y = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
      axis.text.y = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray90")
    )
}

# --- Custom Summary Plot (Manual ggplot) ---
plot_rob_summary_manual <- function(data) {
  long_dat <- data %>%
    pivot_longer(cols = -Study, names_to = "Domain", values_to = "Risk") %>%
    mutate(
      Risk = factor(Risk, levels = c("Low", "Unclear", "High")),
      Domain = factor(Domain, levels = rev(colnames(data)[-1]))
    )
  
  ggplot(long_dat, aes(y = Domain, fill = Risk)) +
    geom_bar(position = "fill", width = 0.7, color = "black", size = 0.3) +
    scale_fill_manual(values = c("Low" = "#009E73", "High" = "#D55E00", "Unclear" = "#F0E442")) +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Risk of Bias Summary", x = "Proportion of Studies", y = "") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )
}

plot_fagan_custom <- function(pre.pos, sens, spec) {
  lr.pos <- sens / (1 - spec)
  lr.neg <- (1 - sens) / spec
  post.pos <- (pre.pos * lr.pos) / (1 - pre.pos + (pre.pos * lr.pos))
  post.neg <- (pre.pos * lr.neg) / (1 - pre.pos + (pre.pos * lr.neg))
  
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(0, type="n", xlim=c(0, 2), ylim=c(0, 100), axes=FALSE, xlab="", ylab="Probability (%)", main="Fagan's Nomogram")
  axis(2, at=seq(0, 100, 10), las=1)
  
  points(x=0.2, y=pre.pos*100, pch=19, cex=2, col="black")
  text(0.2, pre.pos*100, paste("Pre-Test\n", round(pre.pos*100, 1), "%"), pos=2)
  points(x=1.8, y=post.pos*100, pch=19, cex=2, col="darkblue")
  text(1.8, post.pos*100, paste("Post-Test (+)\n", round(post.pos*100, 1), "%"), pos=4)
  points(x=1.8, y=post.neg*100, pch=19, cex=2, col="red")
  text(1.8, post.neg*100, paste("Post-Test (-)\n", round(post.neg*100, 1), "%"), pos=4)
  lines(c(0.2, 1.8), c(pre.pos*100, post.pos*100), col="darkblue", lwd=2)
  lines(c(0.2, 1.8), c(pre.pos*100, post.neg*100), col="red", lwd=2, lty=2)
  legend("bottom", legend=c(paste0("LR+ = ", round(lr.pos, 2)), paste0("LR- = ", round(lr.neg, 2))), col=c("darkblue", "red"), lty=c(1, 2), lwd=2, bty="n")
}

get_summary_table <- function(res) {
  fmt <- function(e, l, u) paste0(formatC(e, format = "f", digits = 2), " [", formatC(l, format = "f", digits = 2), ", ", formatC(u, format = "f", digits = 2), "]")
  cor_test_safe <- suppressWarnings(cor.test(res$study_sens, res$study_fpr, method = "spearman"))
  cor_str <- paste0("rho = ", round(cor_test_safe$estimate, 3), " (p = ", formatC(cor_test_safe$p.value, format="f", digits=3), ")")
  auc_str <- if(is.na(res$auc_val)) "Model Failed" else formatC(res$auc_val, format = "f", digits = 3)
  
  data.frame(
    Parameter = c("Pooled Sensitivity", "Pooled Specificity", "Diagnostic Odds Ratio (DOR)", "AUC (HSROC)", "Spearman Correlation"),
    `Estimate [95% CI]` = c(
      fmt(res$meta_sens$TE.random, res$meta_sens$lower.random, res$meta_sens$upper.random),
      fmt(res$meta_spec$TE.random, res$meta_spec$lower.random, res$meta_spec$upper.random),
      fmt(exp(res$meta_dor$TE.random), exp(res$meta_dor$lower.random), exp(res$meta_dor$upper.random)),
      auc_str, cor_str
    ),
    `Heterogeneity (I2)` = c(paste0(round(res$meta_sens$I2 * 100, 1), "%"), paste0(round(res$meta_spec$I2 * 100, 1), "%"), paste0(round(res$meta_dor$I2 * 100, 1), "%"), "NA", "NA"), check.names = FALSE
  )
}

plot_forest <- function(res, boxsize_val) {
  # 1. Define Colors for the Graphics (Box & Lines)
  col_sens <- fpColors(box = "#1f77b4", lines = "#1f77b4", summary = "#1f77b4") # Blue
  col_spec <- fpColors(box = "#d62728", lines = "#d62728", summary = "#d62728") # Red
  
  # 2. Define Text Styles (NEW CODE)
  # Style for Sensitivity (Blue text for headers, labels, and summary)
  txt_sens <- fpTxtGp(
    label   = gpar(cex = 0.9, col = "#1f77b4"),
    ticks   = gpar(cex = 0.8, col = "#1f77b4"),
    summary = gpar(cex = 0.9, col = "#1f77b4", fontface = "bold"),
    title   = gpar(fontface = "bold", cex = 1.2, col = "#1f77b4")
  )
  
  # Style for Specificity (Red text)
  txt_spec <- fpTxtGp(
    label   = gpar(cex = 0.9, col = "#d62728"),
    ticks   = gpar(cex = 0.8, col = "#d62728"),
    summary = gpar(cex = 0.9, col = "#d62728", fontface = "bold"),
    title   = gpar(fontface = "bold", cex = 1.2, col = "#d62728")
  )
  
  # 3. Formatting Helper
  fmt <- function(est, lower, upper) sprintf("%.2f [%.2f, %.2f]", est, lower, upper)
  
  # 4. Prepare Data Matrix
  # We construct the text table
  label_text <- cbind(
    c("Study", res$df$StudyName, "Pooled Estimate", "Heterogeneity"),
    c("Sensitivity", fmt(res$meta_sens$TE, res$meta_sens$lower, res$meta_sens$upper), fmt(res$meta_sens$TE.random, res$meta_sens$lower.random, res$meta_sens$upper.random), paste0("I2 = ", round(res$meta_sens$I2 * 100, 0), "%")),
    c("Specificity", fmt(res$meta_spec$TE, res$meta_spec$lower, res$meta_spec$upper), fmt(res$meta_spec$TE.random, res$meta_spec$lower.random, res$meta_spec$upper.random), paste0("I2 = ", round(res$meta_spec$I2 * 100, 0), "%"))
  )
  
  # 5. Prepare Plot Data
  prep <- function(m) list(mean=c(NA, m$TE, m$TE.random, NA), lower=c(NA, m$lower, m$lower.random, NA), upper=c(NA, m$upper, m$upper.random, NA))
  d_sens <- prep(res$meta_sens)
  d_spec <- prep(res$meta_spec)
  is_summary <- c(TRUE, rep(FALSE, nrow(res$df)), TRUE, FALSE)
  
  # 6. Generate Plots
  
  # Plot 1: Sensitivity (Uses col_sens AND txt_sens)
  # Note: We use label_text columns 1 and 2 here
  p1 <- grid.grabExpr(print(forestplot(
    labeltext = label_text[, 1:2], 
    mean = d_sens$mean, lower = d_sens$lower, upper = d_sens$upper, 
    is.summary = is_summary, 
    xlab = "Sensitivity", 
    zero = NA, 
    boxsize = boxsize_val, 
    graph.pos = 2, # Position graph after text columns
    col = col_sens, 
    txt_gp = txt_sens, # <--- APPLIES BLUE TEXT
    title = "Sensitivity"
  )))
  
  # Plot 2: Specificity (Uses col_spec AND txt_spec)
  # Note: We only use label_text column 3 here
  p2 <- grid.grabExpr(print(forestplot(
    labeltext = label_text[, 3, drop = FALSE], 
    mean = d_spec$mean, lower = d_spec$lower, upper = d_spec$upper, 
    is.summary = is_summary, 
    xlab = "Specificity", 
    zero = NA, 
    boxsize = boxsize_val, 
    graph.pos = 1, # Position graph before text column
    col = col_spec, 
    txt_gp = txt_spec, # <--- APPLIES RED TEXT
    title = "Specificity"
  )))
  
  # 7. Combine
  grid.arrange(p1, p2, ncol = 2, widths = c(0.6, 0.4))
}

plot_hsroc <- function(fit, df) {
  # 1. Handle Model Failure
  if(is.null(fit)) { 
    plot.new()
    text(0.5, 0.5, "Model Failed (Need >4 studies)")
    return() 
  }
  
  # 2. Base Plot (HSROC Curve & Regions)
  # 'predict = TRUE' adds the prediction region (dashed/dotted)
  # 'col = "royalblue"' colors the main SROC curve BLUE
  # Note: The Regions (Confidence & Prediction) usually default to BLACK in mada
  plot(fit, predict = TRUE, predlty = 3, sroclwd = 2, 
       xlim = c(0, 1), ylim = c(0, 1), 
       main = "HSROC Curve", col = "royalblue")
  
  # 3. Add Grid & Diagonal
  grid(col = "lightgray", lty = "dotted")
  abline(0, 1, col = "gray50", lty = 2)
  
  # 4. Add Individual Studies (Points)
  points(mada::fpr(df), mada::sens(df), 
         pch = 21, bg = rgb(0.2, 0.2, 0.2, 0.5), col = "black", cex = 1.5)
  
  # 5. CORRECTED LEGEND
  # We explicitly match what 'mada' draws:
  # - Studies: Gray/Black Circles
  # - Summary: Black Dot (Default in mada)
  # - Curve: Blue Line (defined by col="royalblue")
  # - Conf Region: Black Solid Line (Default)
  # - Pred Region: Black Dotted Line (defined by predlty=3)
  
  legend("bottomright", 
         legend = c("Individual Studies", "Summary Estimate", 
                    "HSROC Curve", "95% Conf. Region", "95% Pred. Region"),
         pch = c(21, 1, NA, NA, NA), 
         pt.bg = c(rgb(0.2, 0.2, 0.2, 0.5), NA, NA, NA, NA),
         col = c("black", "royalblue",  "black", "royalblue", "black"), 
         lty = c(NA, NA, 1, 1, 3), 
         lwd = c(NA, NA, 2, 1, 1),
         bty = "y", bg = "white", cex = 0.8)
}

plot_deeks <- function(res) {
  p_val <- summary(res$fit_deeks)$coefficients[2, 4]
  p_text <- paste0("P-value = ", formatC(p_val, format = "f", digits = 3))
  plot(res$deeks_data$term, res$deeks_data$logDOR, xlab = "1/sqrt(ESS)", ylab = "lnDOR", main = "Deeks' Funnel Plot", pch = 21, bg = "darkorange")
  abline(res$fit_deeks, col = "darkblue", lwd = 2, lty = 2)
  legend("bottomright", legend = c(p_text), bty="n")
}


# ==============================================================================
# 2. UI
# ==============================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$title("ADAM: Automated Diagnostic Accuracy Meta-analysis Tool"), 
    tags$meta(name = "description", content = "Free online tool for Diagnostic Test Accuracy (DTA) Meta-analysis."),
    tags$meta(name = "author", content = "Vignesh D")
  ),
  titlePanel("ADAM: Automated Diagnostic Accuracy Meta-analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Data Input"),
      div(style="display: flex; gap: 10px; margin-bottom: 10px;",
          actionButton("use_demo", "Load Demo Data", icon = icon("table"), class = "btn-info"),
          p(style="margin-top: 8px;", "OR")
      ),
      fileInput("file1", "Upload Excel (.xlsx)", accept = c(".xlsx")),
      helpText("Required columns: Author, Year, TP, FP, FN, TN"),
      
      hr(),
      h4("Step 2: Column Mapping"),
      uiOutput("column_selector"),
      
      
      
      # --- QUADAS SECTION ---
      br(),
      tags$details(
        tags$summary(
          strong("Optional: QUADAS-2 Mapping (Click to Expand)"), 
          style = "background-color: #2c3e50; color: white; padding: 10px; border-radius: 5px; cursor: pointer; width: 100%; margin-bottom: 10px; text-align: center;"
        ),
        helpText("Map your Excel columns to the 7 QUADAS domains. Values MUST be 'High', 'Low', or 'Unclear'."),
        uiOutput("quadas_selector")
      ),
      
      # --- MODIFIED: SUBGROUP SELECTORS ---
      hr(),
      h4("Step 3: Subgroup Analysis (Optional)"),
      uiOutput("subgroup_selector"),
      
      hr(),
      sliderInput("boxsize", "Box Size", min = 0.1, max = 0.5, value = 0.25),
      hr(),
      actionButton("run_analysis", "Step 4: Run Analysis", class = "btn-primary", width = "100%"),
      br(), br(),
      wellPanel(
        style = "background-color: #f8f9fa; padding: 10px; border-left: 3px solid #2c3e50;",
        h5(strong("Created By:")),
        p("Vignesh D", style = "margin-bottom: 2px; font-weight: bold;"),
        p("Asst. Professor, Community Medicine", style = "font-size: 11px; margin-bottom: 2px; color: #555;"),
        p("ESIC Medical College and Hospital, Chennai", style = "font-size: 11px; margin-bottom: 2px; color: #555;"),
        p("Email: d.vignesh1991@gmail.com", style = "font-size: 11px; margin-bottom: 10px; color: #555;"),
        h5(strong("Please Cite As:")),
        p("Vignesh D. (2026). ADAM : Automated Diagnostic Accuracy Meta-Analysis (1.1.0). Zenodo. https://doi.org/10.5281/zenodo.18195912", 
          style = "font-size: 11px; color: #333; font-style: italic; line-height: 1.4; margin-bottom: 10px;"),
        tags$a(
          href = "https://doi.org/10.5281/zenodo.18195912", 
          target = "_blank",
          img(src = "https://zenodo.org/badge/DOI/10.5281/zenodo.18195912.svg", alt = "DOI")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", tableOutput("view_data")),
        
        tabPanel("Paired Forest Plot", 
                 div(style="text-align: right; margin-bottom: 5px;", downloadButton("dl_forest", "Download Forest Plot (.png)")),
                 plotOutput("distPlot", height = "600px")
        ),
        
        tabPanel("HSROC", 
                 div(style="text-align: right; margin-bottom: 5px;", downloadButton("dl_sroc", "Download HSROC (.png)")),
                 plotOutput("sroc_plot"),
                 # --- NEW: HSROC EXPLANATION ---
                 br(),
                 wellPanel(
                   h4("How to Interpret the HSROC Curve"),
                   tags$ul(
                     tags$li(strong("The Curve:"), " Represents the trade-off between Sensitivity and Specificity across studies. Studies in the top-left corner indicate high diagnostic accuracy."),
                     tags$li(strong("The Summary Point (Blue Hollow Circle):"), " The pooled estimate of Sensitivity and Specificity."),
                     tags$li(strong("Confidence Region (Blue Solid Ellipse):"), " The region where the true mean value likely lies."),
                     tags$li(strong("Prediction Region (Dotted Ellipse):"), " Where the results of a future study are predicted to fall. If this is wide, there is high heterogeneity.")
                   )
                 )
        ),
        
        tabPanel("Fagan's Nomogram", 
                 div(style="background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 10px;",
                     sliderInput("pretest", "Adjust Pre-test Probability (%):", min = 1, max = 99, value = 25, width = "50%")),
                 br(),
                 div(style="text-align: right; margin-bottom: 5px;", downloadButton("dl_fagan", "Download Nomogram (.png)")),
                 plotOutput("fagan_plot", height = "500px"),
                 # --- NEW: FAGAN EXPLANATION ---
                 wellPanel(
                   h4("How to Interpret Fagan's Nomogram"),
                   p("This tool helps apply the test results to a specific patient scenario."),
                   tags$ul(
                     tags$li(strong("Left Axis (Pre-test):"), "The probability the patient has the disease BEFORE the test."),
                     tags$li(strong("Middle Axis (LR):"), "Likelihood Ratio. Lines connect the pre-test probability through the LR."),
                     tags$li(strong("Right Axis (Post-test):"), "The probability the patient has the disease AFTER the test result."),
                     tags$li(strong("Blue Line:"), "Post-test probability if the test is POSITIVE."),
                     tags$li(strong("Red Dotted Line:"), "Post-test probability if the test is NEGATIVE.")
                   ),
                   textOutput("fagan_text")
                 )
        ),
        
        tabPanel("Quality Assessment (QUADAS-2)", 
                 h3("Risk of Bias & Applicability"),
                 p("These plots generate the standard 'robvis' Traffic Light and Summary Plots."),
                 div(style="text-align: right; margin-bottom: 5px;", downloadButton("dl_traffic", "Download Traffic Light (.png)")),
                 plotOutput("rob_traffic", height = "600px"),
                 hr(),
                 div(style="text-align: right; margin-bottom: 5px;", downloadButton("dl_summary", "Download Summary Plot (.png)")),
                 plotOutput("rob_summary", height = "400px")
        ),
        
        tabPanel("Summary Stats", tableOutput("summary_table")),
        
        tabPanel("Deeks' Funnel Plot", 
                 div(style="text-align: right; margin-bottom: 5px;", downloadButton("dl_funnel", "Download Funnel Plot (.png)")),
                 plotOutput("funnel_plot"),
                 helpText("P < 0.10 indicates potential publication bias.")
        ),
        
        tabPanel("About", 
                 h3("About This Tool"),
                 p("This application performs diagnostic test accuracy (DTA) meta-analysis using R."),
                 h4("How to Cite"),
                 p("If you use this tool for your research, please cite it as:"),
                 wellPanel(
                   p(strong("Vignesh D. (2026). ADAM : Automated Diagnostic Accuracy Meta-Analysis (1.1.0). Zenodo. https://doi.org/10.5281/zenodo.18195912"))
                 ),
                 
                 # --- SECTION 2: METHODS TEMPLATE (NEW) ---
                 h4("2. Statistical Methods Template (For your Manuscript)"),
                 p("You can copy and paste the following text into the 'Methods' section of your paper. It accurately describes the statistical models used by ADAM."),
                 wellPanel(
                   style = "background-color: #fcfcfc; border: 1px solid #e3e3e3; padding: 15px;",
                   tags$pre(
                     style = "white-space: pre-wrap; font-family: 'Courier New', monospace; font-size: 13px;",
                     "Statistical analysis was performed using the ADAM (Automated Diagnostic Accuracy Meta-analysis) tool (Version 1.1.0) [1]. 

Forest Plots and Pooled Estimates:
Paired forest plots were generated to visualize the sensitivity and specificity of each included study. Pooled sensitivity and specificity were estimated using a random-effects model (DerSimonian-Laird estimator) via the 'meta' package in R. Confidence intervals for individual studies were calculated using the Clopper-Pearson method.

Hierarchical Summary ROC (HSROC) Model:
To account for the correlation between sensitivity and specificity and to evaluate the threshold effect, we fitted a Hierarchical Summary Receiver Operating Characteristic (HSROC) model using the bivariate approach described by Reitsma et al. (2005) [2]. This analysis was performed using the 'mada' package in R.

Publication Bias:
Potential publication bias was assessed using Deeks' funnel plot asymmetry test, which is the recommended method for diagnostic test accuracy studies [3]. A p-value < 0.10 was considered indicative of significant asymmetry.

Quality Assessment:
Risk of bias and applicability concerns were assessed using the QUADAS-2 tool. The results were visualized as traffic light plots and summary bar charts using standard visualization techniques [4].

References:
[1] Vignesh D. (2026). ADAM: Automated Diagnostic Accuracy Meta-Analysis (1.1.0). Zenodo.
[2] Reitsma JB, et al. (2005). Bivariate analysis of sensitivity and specificity produces informative summary measures in diagnostic reviews. Journal of Clinical Epidemiology.
[3] Deeks JJ, et al. (2005). Assessment of publication bias in systematic reviews of diagnostic test accuracy studies.
[4] McGuinness LA, Higgins JP (2021). Risk-of-bias VISualization (robvis): An R shiny application."
                   )
                 ),
                 
                 hr(),
                 
                 h4("R Packages Used"),
                 p("This tool is built using the following R packages:"),
                 tags$ul(
                   tags$li("mada (Metaprop/HSROC)"),
                   tags$li("meta (Standard Meta-Analysis)"),
                   tags$li("metafor (Advanced Calculations)"),
                   tags$li("forestplot (Visualization)"),
                   tags$li("ggplot2 (Graphics)")
                 ),
                 
                 hr(),
                 
                 h3("License"),
                 p("Copyright (c) 2026 Dr. Vignesh D (ADAM Tool)"),
                 p("This tool is open-source and free for academic and non-commercial use under the ", strong("MIT License"), "."),
                 tags$ul(
                   tags$li("You are free to use, copy, and modify this tool."),
                   tags$li(strong("Attribution is required:"), " You must give appropriate credit to the original author (Vignesh D) in any derivative work or publication.")
                 ),
                 
                 hr(),
                 p(em("Disclaimer: This tool is for educational and research purposes. While every effort has been made to ensure accuracy, the author accepts no responsibility for errors or omissions."), style = "color: gray; font-size: 0.9em;")
        )
      )
    )
  )
)


# ==============================================================================
# 3. SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  current_data <- reactiveVal(NULL)
  
  observeEvent(input$use_demo, { current_data(demo_data); showNotification("Demo data loaded!", type = "message") })
  observeEvent(input$file1, { req(input$file1); current_data(read_excel(input$file1$datapath)) })
  
  # --- UI SELECTORS ---
  output$column_selector <- renderUI({
    req(current_data()); cols <- names(current_data())
    tagList(
      selectInput("author", "Select Author:", choices = cols, selected = if("Author" %in% cols) "Author" else cols[1]),
      selectInput("year", "Select Year:", choices = cols, selected = if("Year" %in% cols) "Year" else cols[2]),
      selectInput("col_tp", "TP:", choices = cols, selected = "TP"),
      selectInput("col_fp", "FP:", choices = cols, selected = "FP"),
      selectInput("col_fn", "FN:", choices = cols, selected = "FN"),
      selectInput("col_tn", "TN:", choices = cols, selected = "TN")
    )
  })
  
  # --- MODIFIED: SUBGROUP UI ---
  output$subgroup_selector <- renderUI({
    req(current_data())
    cols <- names(current_data())
    tagList(
      selectInput("col_subgroup", "Select Subgroup Column:", choices = c("None", cols), selected = if("StudyDesign" %in% cols) "StudyDesign" else "None"),
      uiOutput("subgroup_value_ui")
    )
  })
  
  output$subgroup_value_ui <- renderUI({
    req(current_data(), input$col_subgroup)
    if(input$col_subgroup == "None") return(NULL)
    
    # Get unique values from the selected column
    vals <- unique(as.character(current_data()[[input$col_subgroup]]))
    selectInput("subgroup_filter", "Filter by Category:", choices = c("All", vals), selected = "All")
  })
  
  output$quadas_selector <- renderUI({
    req(current_data()); cols <- names(current_data())
    tagList(
      selectInput("q_d1", "Patient Selection (Risk):", choices = c("None", cols), selected = if("D1" %in% cols) "D1" else "None"),
      selectInput("q_d2", "Index Test (Risk):", choices = c("None", cols), selected = if("D2" %in% cols) "D2" else "None"),
      selectInput("q_d3", "Reference Standard (Risk):", choices = c("None", cols), selected = if("D3" %in% cols) "D3" else "None"),
      selectInput("q_d4", "Flow & Timing (Risk):", choices = c("None", cols), selected = if("D4" %in% cols) "D4" else "None"),
      selectInput("q_d5", "Patient Selection (Applicability):", choices = c("None", cols), selected = if("D5" %in% cols) "D5" else "None"),
      selectInput("q_d6", "Index Test (Applicability):", choices = c("None", cols), selected = if("D6" %in% cols) "D6" else "None"),
      selectInput("q_d7", "Reference Standard (Applicability):", choices = c("None", cols), selected = if("D7" %in% cols) "D7" else "None")
    )
  })
  
  # --- DATA PROCESSING ---
  processed_data <- eventReactive(input$run_analysis, {
    req(current_data())
    df <- current_data()
    clean_df <- data.frame(
      TP = as.numeric(as.character(df[[input$col_tp]])),
      FP = as.numeric(as.character(df[[input$col_fp]])),
      FN = as.numeric(as.character(df[[input$col_fn]])),
      TN = as.numeric(as.character(df[[input$col_tn]])),
      Author = df[[input$author]], Year = df[[input$year]]
    )
    
    # Add Subgroup column if selected
    if(input$col_subgroup != "None") {
      clean_df$SubgroupCol <- as.character(df[[input$col_subgroup]])
    } else {
      clean_df$SubgroupCol <- "All"
    }
    
    clean_df$StudyName <- paste(clean_df$Author, clean_df$Year)
    return(clean_df)
  })
  
  # --- NEW: FILTERED DATA REACTIVE ---
  # This listens to processed_data() AND the dropdown filter
  filtered_data <- reactive({
    req(processed_data())
    df <- processed_data()
    
    # If a filter is applied, subset the data
    if (!is.null(input$subgroup_filter) && input$subgroup_filter != "All") {
      df <- df %>% filter(SubgroupCol == input$subgroup_filter)
    }
    return(df)
  })
  
  rob_data <- eventReactive(input$run_analysis, {
    req(current_data()); df <- current_data()
    req(input$q_d1, input$q_d2, input$q_d3, input$q_d4)
    if(input$q_d1 == "None") return(NULL) 
    
    clean_val <- function(x) {
      x <- trimws(x) 
      x <- tools::toTitleCase(tolower(x))
      x[!x %in% c("Low", "High", "Unclear")] <- "Unclear"
      return(x)
    }
    
    raw_rob <- data.frame(
      Study = paste(df[[input$author]], df[[input$year]]),
      `Patient Selection` = clean_val(as.character(df[[input$q_d1]])),
      `Index Test` = clean_val(as.character(df[[input$q_d2]])),
      `Reference Standard` = clean_val(as.character(df[[input$q_d3]])),
      `Flow and Timing` = clean_val(as.character(df[[input$q_d4]])),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    
    if(input$q_d5 != "None" & input$q_d6 != "None" & input$q_d7 != "None") {
      raw_rob$`Applicability: Patient Selection` <- clean_val(as.character(df[[input$q_d5]]))
      raw_rob$`Applicability: Index Test`   <- clean_val(as.character(df[[input$q_d6]]))
      raw_rob$`Applicability: Reference Standard`     <- clean_val(as.character(df[[input$q_d7]]))
    } else {
      d1 <- raw_rob[[2]]; d2 <- raw_rob[[3]]; d3 <- raw_rob[[4]]; d4 <- raw_rob[[5]]
      overall <- mapply(function(a, b, c, d) {
        vals <- c(a, b, c, d)
        if ("High" %in% vals) return("High")
        if ("Unclear" %in% vals) return("Unclear")
        return("Low")
      }, d1, d2, d3, d4)
      raw_rob$`Overall Risk` <- overall
    }
    return(raw_rob)
  })
  
  # Update view_data to show filtered data
  output$view_data <- renderTable({ req(filtered_data()); head(filtered_data()) }, striped = TRUE)
  
  analysis_results <- reactive({
    req(filtered_data())
    df <- filtered_data()
    
    # Ensure enough studies for meta-analysis
    validate(
      need(nrow(df) > 1, "Error: Need at least 2 studies for meta-analysis. Please check your data or subgroup filter.")
    )
    
    validate(need(all(!is.na(df$TP) & !is.na(df$TN)), "Error: Non-numeric data found."))
    
    meta_sens <- metaprop(event = TP, n = TP + FN, data = df, sm = "PRAW")
    meta_spec <- metaprop(event = TN, n = TN + FP, data = df, sm = "PRAW")
    
    df_c <- df; if(any(df$TP==0|df$FP==0|df$FN==0|df$TN==0)) { df_c[1:4] <- df_c[1:4]+0.5 }
    logDOR <- log((df_c$TP * df_c$TN) / (df_c$FP * df_c$FN))
    SE_logDOR <- sqrt(1/df_c$TP + 1/df_c$TN + 1/df_c$FP + 1/df_c$FN)
    ESS <- (4 * df_c$TP * df_c$TN * df_c$FP * df_c$FN) / (df_c$TP + df_c$TN + df_c$FP + df_c$FN)^2
    fit_deeks <- lm(logDOR ~ I(1/sqrt(ESS)), weights = ESS)
    meta_dor <- metagen(logDOR, SE_logDOR)
    fit_hsroc <- tryCatch({ mada::reitsma(df) }, error = function(e) { NULL })
    auc_val <- if(!is.null(fit_hsroc)) mada::AUC(fit_hsroc)$AUC else NA
    
    list(df=df, meta_sens=meta_sens, meta_spec=meta_spec, meta_dor=meta_dor, fit_deeks=fit_deeks, deeks_data=data.frame(term=1/sqrt(ESS), logDOR=logDOR), fit_hsroc=fit_hsroc, auc_val=auc_val, study_sens=df$TP/(df$TP+df$FN), study_fpr=1-(df$TN/(df$TN+df$FP)))
  })
  
  output$distPlot <- renderPlot({ plot_forest(analysis_results(), input$boxsize) })
  output$sroc_plot <- renderPlot({ plot_hsroc(analysis_results()$fit_hsroc, analysis_results()$df) })
  output$funnel_plot <- renderPlot({ plot_deeks(analysis_results()) })
  output$summary_table <- renderTable({ get_summary_table(analysis_results()) }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$fagan_plot <- renderPlot({
    res <- analysis_results(); sens <- res$meta_sens$TE.random; spec <- res$meta_spec$TE.random; pre <- input$pretest / 100
    plot_fagan_custom(pre, sens, spec)
  })
  
  output$fagan_text <- renderText({
    res <- analysis_results(); sens <- res$meta_sens$TE.random; spec <- res$meta_spec$TE.random; lr_pos <- sens / (1 - spec)
    paste0("Based on the pooled Sensitivity of ", round(sens*100,1), "% and Specificity of ", round(spec*100,1), "%, a positive test result increases the likelihood of disease by a factor of ", round(lr_pos, 2), ".")
  })
  
  # --- QUADAS OUTPUTS ---
  output$rob_traffic <- renderPlot({
    req(rob_data())
    print(plot_rob_traffic_manual(rob_data()))
  })
  
  output$rob_summary <- renderPlot({
    req(rob_data())
    print(plot_rob_summary_manual(rob_data()))
  })
  
  # DOWNLOAD HANDLERS (Same as before, relying on analysis_results which is now filtered)
  output$dl_forest <- downloadHandler(
    filename = function() { paste0("Forest_Plot_", Sys.Date(), ".png") },
    content = function(file) {
      req(analysis_results())
      n_studies <- nrow(analysis_results()$df)
      plot_height <- max(6, 3 + (n_studies * 0.3))
      png(file, width = 12, height = plot_height, units = "in", res = 300)
      plot_forest(analysis_results(), input$boxsize)
      dev.off()
    }
  )
  
  output$dl_sroc <- downloadHandler(
    filename = function() { paste0("HSROC_Plot_", Sys.Date(), ".png") },
    content = function(file) {
      req(analysis_results())
      png(file, width = 8, height = 8, units = "in", res = 300)
      plot_hsroc(analysis_results()$fit_hsroc, analysis_results()$df)
      dev.off()
    }
  )
  
  output$dl_fagan <- downloadHandler(
    filename = function() { paste0("Fagan_Nomogram_", Sys.Date(), ".png") },
    content = function(file) {
      req(analysis_results())
      png(file, width = 9, height = 7, units = "in", res = 300)
      res <- analysis_results()
      sens <- res$meta_sens$TE.random
      spec <- res$meta_spec$TE.random
      pre <- input$pretest / 100
      plot_fagan_custom(pre, sens, spec)
      dev.off()
    }
  )
  
  output$dl_funnel <- downloadHandler(
    filename = function() { paste0("Deeks_Funnel_", Sys.Date(), ".png") },
    content = function(file) {
      req(analysis_results())
      png(file, width = 8, height = 6, units = "in", res = 300)
      plot_deeks(analysis_results())
      dev.off()
    }
  )
  
  output$dl_traffic <- downloadHandler(
    filename = function() { paste0("ROB_Traffic_Light_", Sys.Date(), ".png") },
    content = function(file) {
      req(rob_data())
      n_studies <- nrow(rob_data())
      plot_height <- max(5, 2 + (n_studies * 0.3)) 
      p <- plot_rob_traffic_manual(rob_data())
      ggsave(file, plot = p, device = "png", width = 12, height = plot_height, dpi = 300)
    }
  )
  
  output$dl_summary <- downloadHandler(
    filename = function() { paste0("ROB_Summary_", Sys.Date(), ".png") },
    content = function(file) {
      req(rob_data())
      p <- plot_rob_summary_manual(rob_data())
      ggsave(file, plot = p, device = "png", width = 10, height = 5, dpi = 300)
    }
  )
}

shinyApp(ui = ui, server = server)