#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages <- c("shiny", 
                      "ggplot2", 
                      "ggExtra",
                      "ggcorrplot",
                      "ggfortify",
                      "ggrepel",
                      "RColorBrewer",
                      "shinyWidgets",
                      "tidyverse",
                      "broom",
                      "Rtsne",
                      "factoextra",
                      "cluster",
                      "gtools",
                      "devtools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
library(ggfortify)
library(ggrepel)
library(RColorBrewer)
library(shinyWidgets)
library(tidyverse)
library(broom)
library(Rtsne)
library(factoextra)
library(cluster)
library(gtools)
library(devtools)
devtools::install_github("tpmp-inra/tpmp_shiny_common")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  headerPanel("ML Tools"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    fileInput(inputId = 'datafile', 
              label = 'Choose CSV file',
              accept = c("text/csv", 
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    fluidRow(column(6,
                    uiOutput("cbTreatmentSelection"),
                    uiOutput("cbPlantSelection"),
                    uiOutput("chkShowOutliers"),
                    uiOutput("cbSelectedVariables"),
                    uiOutput("chkFrameClusters"),
                    uiOutput("cbShowLabels")
                    ),
             column(6,
                    uiOutput("dotSize"),
                    uiOutput("dotShape"),
                    uiOutput("colorBy"),
                    uiOutput("cbSplitScatter"),
                    uiOutput("cbDateTimeSelector")
                    )
             ),
    
    conditionalPanel(
      condition="input.selected_clustering_method==2", 
      fluidRow(column(6, uiOutput("ni_xPrincipalComponent"), uiOutput("chkShowLoadings")),
               column(6, uiOutput("ni_yPrincipalComponent")))
      ),
    conditionalPanel(condition="input.selected_clustering_method==3", uiOutput("perplexitySelector")),
    conditionalPanel(condition="input.selected_clustering_method==5", uiOutput("centerCount")),
    
    tags$head(tags$style("#pca_plot{height:80vh !important;}")),
    tags$head(tags$style("#cumulative_variance_plots{height:80vh !important;}")),
    tags$head(tags$style("#variance_plots{height:80vh !important;}")),
    tags$head(tags$style("#tsne_plot{height:70vh !important;}")),
    tags$head(tags$style("#mds_plot{height:70vh !important;}")),
    tags$head(tags$style("#kms_plot{height:70vh !important;}")),
    tags$head(tags$style("#kms_elbow{height:70vh !important;}")),
    tags$head(tags$style("#kms_silhouette{height:70vh !important;}")),
    tags$head(tags$style("#kms_gap{height:70vh !important;}")),
    tags$head(tags$style("#histogram_plot{height:90vh !important;}")),
    tags$head(tags$style("#efa_plot{height:70vh !important;}"))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      id = "selected_clustering_method",
      tabPanel("Filtered CSV File",
               value = 1, 
               tabsetPanel(
                 tabPanel("Dataframe", dataTableOutput("filetable")),
                 tabPanel("Summary", verbatimTextOutput("dataframe_summary")))),
      tabPanel("PCA",
               value = 2,
               tabsetPanel(
                 tabPanel("PCA", plotOutput(outputId = "pca_plot", 
                                            inline = FALSE,
                                            dblclick = "pca_plot_dblclick",
                                            brush = brushOpts(id = "pca_plot_brush", resetOnNew = TRUE))),
                 tabPanel("Cumulative Variance Explained", plotOutput(outputId = "cumulative_variance_plots", inline = FALSE)),
                 tabPanel("Variance Explained", plotOutput(outputId = "variance_plots", inline = FALSE)),
                 tabPanel("PCA output", verbatimTextOutput("pca_details")))),
      tabPanel("t-SNE", 
               value = 3,
               plotOutput(outputId = "tsne_plot", inline = FALSE, hover = "tsne_plot_hover"),
               tableOutput("tsne_observationInfo")),
      tabPanel("MDS",
               value = 4,
               plotOutput(outputId = "mds_plot", inline = FALSE, hover = "mds_plot_hover"),
               tableOutput("mds_observationInfo")),
      tabPanel("K-means",
               value = 5,
               tabsetPanel(
                 tabPanel("K-means", 
                          plotOutput(outputId = "kms_plot", inline = FALSE, hover = "kms_plot_hover"),
                          tableOutput("kms_observationInfo")),
                 tabPanel("Optimal cluster number (silhouette)", plotOutput(outputId = "kms_silhouette", inline = FALSE)),
                 tabPanel("Optimal cluster number (elbow)", plotOutput(outputId = "kms_elbow", inline = FALSE)),
                 tabPanel("Optimal cluster number (gap)", plotOutput(outputId = "kms_gap", inline = FALSE)))),
      tabPanel("Histograms",
               value = 6,
               plotOutput(outputId = "histogram_plot", inline = FALSE))
      # tabPanel("Exploratory Factor Analysis (WIP)",
      #          value = 6,
      #          plotOutput(outputId = "efa_plot", inline = FALSE, hover = "efa_plot_hover"),
      #          tableOutput("efa_observationInfo")),
      # tabPanel("Spider plots (WIP)",
      #          value = 7,
      #          plotOutput(outputId = "spd_plot", inline = FALSE, hover = "spd_plot_hover"),
      #          tableOutput("spd_observationInfo"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  options(shiny.maxRequestSize=30*1024^2) # Still not sure it's a good idea
  
  # Data building functions -----------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  filtered_data <- reactive({
    req(input$cbSelectedVariables,
        input$cbTreatmentSelection)
    
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    selVar <- input$cbSelectedVariables
    if (is.null(selVar)) return(NULL)
    
    selTreatment <- input$cbTreatmentSelection
    if (is.null(selTreatment)) return(NULL)
    
    selPlant <- input$cbPlantSelection
    if (is.null(selPlant)) return(NULL)
    
    dotSize <- input$dotSize
    if (is.null(dotSize)) return(NULL)
    
    dotShape <- input$dotShape
    if (is.null(dotShape)) return(NULL)
    
    dotColor <- input$colorBy
    if (is.null(dotColor)) return(NULL)
    
    facetSelector <- input$cbSplitScatter
    if (is.null(facetSelector)) return(NULL)
    
    labelSelector <- input$cbShowLabels
    if (is.null(labelSelector)) return(NULL)
    
    if (sum(selVar %in% names(df)) > 0 & length(selTreatment) > 0) {
      withProgress(message = "Filtering data", value = 0, {
        df_filtered <- 
          df %>% 
          filter(trunc_day_after_start %in% input$cbDateTimeSelector) %>%
          filter(treatment %in% selTreatment) %>%
          filter(plant %in% selPlant) %>%
          unique() %>%
          drop_na()
          
        if (!input$chkShowOutliers & ("outlier" %in% colnames(df))) {
          df_filtered <- df_filtered %>% filter(outlier == 0)
        }
        
        df_num <- df_filtered %>% subset(select=selVar)
        
        # R is bad with rounding, like really bad
        df_num <- df_num[lapply(df_num, var, na.rm = TRUE) > 0.001]
        
        df_num <- 
          df_num %>% 
          mutate(plant = df_filtered$plant) %>%
          mutate(treatment = df_filtered$treatment) %>%
          mutate(disease_index = df_filtered$disease_index) %>%
          mutate(day_after_start = df_filtered$day_after_start) %>%
          select(plant, treatment, disease_index, day_after_start, everything()) %>%
          drop_na()
        
        col_to_remove <- c("plant", "treatment")
        if (!"disease_index" %in% selVar | var(df_num$disease_index) < 0.001) {
          col_to_remove <- c(col_to_remove, "disease_index")
        }
        if (!"day_after_start" %in% selVar | var(df_num$day_after_start) < 0.001) {
          col_to_remove <- c(col_to_remove, "day_after_start")
        }
        
        if ((!dotSize %in% colnames(df_num)) & (dotSize != 'none')) {
          df_num <- df_num %>% mutate(!!dotSize := df_filtered[[dotSize]])
          col_to_remove <- c(col_to_remove, dotSize)
        }
        
        if ((!dotShape %in% colnames(df_num)) & (dotShape != 'none')) {
          df_num <- df_num %>% mutate(!!dotShape := df_filtered[[dotShape]])
          col_to_remove <- c(col_to_remove, dotShape)
        }
        
        if ((dotColor != "none") & (!dotColor %in% colnames(df_num))) {
          df_num <- df_num %>% mutate(!!dotColor := df_filtered[[dotColor]])
          col_to_remove <- c(col_to_remove, dotColor)
        }
        
        if ((facetSelector != "none") & (!facetSelector %in% colnames(df_num))) {
          df_num <- df_num %>% mutate(!!facetSelector := df_filtered[[facetSelector]])
          col_to_remove <- c(col_to_remove, facetSelector)
        }
        
        if ((labelSelector != "none") & (!labelSelector %in% colnames(df_num))) {
          df_num <- df_num %>% mutate(!!labelSelector := df_filtered[[labelSelector]])
          col_to_remove <- c(col_to_remove, labelSelector)
        }
        
        # Create color column
        ccn = color_column_name()
        if (dotColor != 'none') {
          ccn.vector <- as.factor(df_num[,dotColor][[1]])
          if (length(unique(ccn.vector)) > 20) {
            ccn.vector <- df_num[,dotColor][[1]]
          }
          df_num <- 
            df_num %>%
            mutate(!!ccn := ccn.vector)
        } else {
          df_num <- 
            df_num %>%
            mutate(!!ccn := as.factor(1))
        }
        col_to_remove <- c(col_to_remove, color_column_name())
        
        df_num <- df_num %>% drop_na()
        
        return(list(df_num = unique(df_num) %>% drop_na(),
                    col_to_remove = col_to_remove))
      })
    } else {
      return(NULL)
    }
  })
  
  dot_data <- reactive({
    req(input$dotSize,
        input$dotShape)
    dotSize <- input$dotSize
    if (dotSize == 'none') dotSize <- 4
    
    dotShape <- input$dotShape
    if (dotShape == 'none') dotShape <- NULL
    
    return(list(dotSize = dotSize,
                dotShape = dotShape))
  })
  
  color_column_name <- reactive({
    dotColor <- input$colorBy
    if (dotColor == 'none') {
      'no_color'
    } else {
      paste('color', dotColor, 'xyz', sep = '_')
    }
  })
  
  # Calculates and groups PCA data
  pca_data <- reactive({
    df <- filtered_data()
    if (is.null(df)) return(NULL)
    withProgress(message = "Building PCA", value = 0, {
      df_num <- df$df_num %>% drop_na()
      col_to_remove <- df$col_to_remove
      
      df_pca <- 
        df_num %>%
        nest() %>%
        mutate(pca = map(data, 
                         ~prcomp(.x %>% select(-col_to_remove), 
                                 center = TRUE, 
                                 scale = TRUE)),
               pca_aug = map2(pca, data,  ~ augment(.x, data=.y)))
      
      var_exp <-
        df_pca %>%
        unnest(pca_aug) %>%
        summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>%
        gather(key = pc, value = variance) %>%
        mutate(var_exp = variance/sum(variance),
               cum_var_exp = cumsum(var_exp),
               pc = str_replace(pc, ".fitted", "")) %>%
        mutate(pc = as.numeric(str_extract_all(pc, "\\-*\\d+\\.*\\d*")))
      
      pcs_df <- cbind(df_num, df_pca$pca[[1]]$x)
    })
    
    return(list(df_num = df_num,
                pca = df_pca$pca[[1]],
                df_pca = df_pca, 
                var_exp = var_exp,
                pcs_df = pcs_df))
  })
  
  # Calculates t-SNE
  tsne_data <- reactive({
    req(input$perplexitySelector)
    df <- filtered_data()
    if (is.null(df)) return(NULL)
    withProgress(message = "Building T-SNE", value = 0, {
      df_num <- df$df_num
      col_to_remove <- df$col_to_remove
      
      # df_filtered <- select(df_num, -col_to_remove)
      
      df_filtered <- df_num[!duplicated(df_num[, setdiff(colnames(df_num), col_to_remove)]), ]
      set.seed(42)
      tsne <- Rtsne(select(df_filtered, -col_to_remove), perplexity = input$perplexitySelector, dims = 2)
    })
    data.frame(x = tsne$Y[,1], y = tsne$Y[,2], df_filtered)
  })
  
  # Calculates MDS
  mds_data <- reactive({
    df <- filtered_data()
    if (is.null(df)) return(NULL)
    df_num <- df$df_num
    withProgress(message = "Building MDS", value = 0, {
      col_to_remove <- df$col_to_remove
      
      df_filtered <- select(df_num, -col_to_remove)
      
      cmd <- cmdscale(dist(scale(df_filtered)), eig = T, k = 2)
    })
    data.frame(x = cmd$points[, 1], y = cmd$points[, 2], df_num)
  })
  
  # Calculates K-means
  kms_data <- reactive({
    req(input$centerCount)
    df <- filtered_data()
    if (is.null(df)) return(NULL)
    withProgress(message = "Building K-Means", value = 0, {
      df_num <- df$df_num
      col_to_remove <- df$col_to_remove
      
      df_filtered <- select(df_num, -col_to_remove)
      
      kms <- kmeans(df_filtered, input$centerCount)
    })
    return(list(kms = kms, 
                df_num = df_num,
                df_filtered = df_filtered))
  })
  
  # Calculates EFA
  efa_data <- reactive({
    df <- filtered_data()
    if (is.null(df)) return(NULL)
    withProgress(message = "Building EFA", value = 0, {
      df_num <- df$df_num
      col_to_remove <- df$col_to_remove
      
      df_filtered <- scale(select(df_num, -col_to_remove))
      
      efa <- factanal(scale(df_filtered), 2, rotation="varimax", scores="regression")
    })
    return(list(efa = efa, 
                df_num = df_num))
  })
  
  # Finalizes spider plot data
  spd_data <- reactive({
    df <- filtered_data()
    if (is.null(df)) return(NULL)
    withProgress(message = "Finalizing Spider plot data", value = 0, {
      df_num <- df$df_num
      col_to_remove <- df$col_to_remove
      
      normalize <- function(x) {
        return((x-min(x)) / (max(x)-min(x)))
      }
      df_norm <- df_norm %>% mutate_if(is.numeric, funs(normalize(.) %>% as.vector))
    })
  })

  hist_data <- reactive({
    filtered_data()
  })
  
  # Main render functions -----------------------------------------------------
  ## Here it renders the t-SNE
  output$tsne_plot = renderPlot({
    dt_tsne <- tsne_data()
    if (is.null(dt_tsne)) return(NULL)
    
    withProgress(message = "Rendering ", value = 0, {
      gg <- ggplot(dt_tsne) 
      gg <- gg + geom_point(aes_string(x = "x", 
                                       y = "y",
                                       size = dot_data()$dotSize,
                                       shape = dot_data()$dotShape,
                                       colour = color_column_name(),
                                       alpha = 0.4))
      
      if (input$cbShowLabels != "none") {
        gg <- gg + geom_text_repel(aes_string(x = "x", y = "y", color = color_column_name(), label = input$cbShowLabels), vjust = -1)
      }
      
      if (!is.null(ranges$x) & !is.null(ranges$y)) {
        gg <- gg + scale_x_continuous(limits = ranges$x)
        gg <- gg + scale_y_continuous(limits = ranges$y)
      }
      
      if (input$chkFrameClusters) {
        gg <- gg + stat_ellipse(aes_string(x = "x", y = "y", colour = color_column_name()), type = "t")
      }
      
      # Format title and axis
      gg <- gg + theme(plot.title = element_text(hjust=0.5, vjust=0.5, size=20, face = "bold"))
      gg <- gg + theme(axis.title.x = element_text(size=20))
      gg <- gg + theme(axis.title.y = element_text(size=20))
      
      
      # Remove legend
      if (nrow(unique(dt$df[,color_column_name()])) > 40) {
        gg <- gg + theme(legend.position = "none")
      }
      
      # Scatter the PCA
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }
    })
    
    gg
  })
  
  # Here it renders the MDS
  output$mds_plot = renderPlot({
    dt_mds <- mds_data()
    if (is.null(dt_mds)) return(NULL)
    withProgress(message = "Rendering ", value = 0, {
      gg <- ggplot(dt_mds) 
      gg <- gg + geom_point(aes_string(x = "x", y = "y", 
                                       size = dot_data()$dotSize,
                                       shape = dot_data()$dotShape, 
                                       colour = color_column_name()),
                            alpha = 0.4)
      
      if (input$cbShowLabels != "none") {
        gg <- gg + geom_text_repel(aes_string(x = "x", y = "y", colour = color_column_name(), label = input$cbShowLabels), vjust = -1)
      }
      
      if (!is.null(ranges$x) & !is.null(ranges$y)) {
        gg <- gg + scale_x_continuous(limits = ranges$x)
        gg <- gg + scale_y_continuous(limits = ranges$y)
      }
      
      if (input$chkFrameClusters) {
        gg <- gg + stat_ellipse(aes_string(x = "x", y = "y", colour = color_column_name()), type = "t")
      }
      
      # Format title and axis
      gg <- gg + theme(plot.title = element_text(hjust=0.5, vjust=0.5, size=20, face = "bold"))
      gg <- gg + theme(axis.title.x = element_text(size=20))
      gg <- gg + theme(axis.title.y = element_text(size=20))
      
      # Remove legend
      if (nrow(unique(dt$df[,color_column_name()])) > 40) {
        gg <- gg + theme(legend.position = "none")
      }
      
      # Scatter the PCA
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }
    })
    
    gg
  })
  
  # Here it renders the PCA
  output$pca_plot = renderPlot({
    dt_pca <- pca_data()
    if (is.null(dt_pca)) return(NULL)
    withProgress(message = "Rendering ", value = 0, {
      gg <- autoplot(dt_pca$pca,
                     data = dt_pca$df_num,
                     x = input$ni_xPrincipalComponent,
                     y = input$ni_yPrincipalComponent,
                     loadings = input$chkShowLoadings,
                     loadings.label = input$chkShowLoadings,
                     loadings.label.size = 5,
                     loadings.label.colour = 'black',
                     loadings.label.repel=T,
                     loadings.colour = 'black',
                     size = dot_data()$dotSize,
                     shape = dot_data()$dotShape,
                     alpha = 0.4,
                     colour = color_column_name(),
                     frame = input$chkFrameClusters,
                     frame.type = 'norm')
      if (input$cbShowLabels != "none") {
        gg <- gg + geom_text_repel(aes_string(color = color_column_name(), label = input$cbShowLabels), vjust = -1)
      }
      
      if (!is.null(ranges$x) & !is.null(ranges$y)) {
        gg <- gg + scale_x_continuous(limits = ranges$x)
        gg <- gg + scale_y_continuous(limits = ranges$y)
      }
      
      # Display eigenvalues for PC1 & PC2 in title
      eigs <- dt_pca$pca$sdev^2
      eigs <- eigs / sum(eigs)
      gg <- gg + ggtitle(label = sprintf("Principal Component Analysis PC1: %s%%, PC2: %s%% Total: %s%%",
                                         format(round(eigs[input$ni_xPrincipalComponent] * 100, 2), nsmall = 2),
                                         format(round(eigs[input$ni_yPrincipalComponent] * 100, 2), nsmall = 2),
                                         format(round((eigs[input$ni_xPrincipalComponent] + eigs[input$ni_yPrincipalComponent]) * 100 ), 2), nsmall = 2))
      
      # Format title and axis
      gg <- gg + theme(plot.title = element_text(hjust=0.5, vjust=0.5, size=20, face = "bold"))
      gg <- gg + theme(axis.title.x = element_text(size=20))
      gg <- gg + theme(axis.title.y = element_text(size=20))
      
      # Remove legend
      if (nrow(unique(dt_pca$df_num[,color_column_name()])) > 40) {
        gg <- gg + theme(legend.position = "none")
      }
      
      # Scatter the PCA
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }
      
      # gg <- gg + theme(legend.title = element_text(size=32, face = "bold"),
      #                  legend.text=element_text(size=30),
      #                  axis.text=element_text(size=20),
      #                  axis.title=element_text(size=22,face="bold"),
      #                  title = element_text(size=20))
    })
    
    gg 
  })
  
  # Here it renders the K-means
  output$kms_plot = renderPlot({
    dt_kms <- kms_data()
    if (is.null(dt_kms)) return(NULL)
    withProgress(message = "Rendering ", value = 0, {
      gg <- autoplot(dt_kms$kms,
                     data = dt_kms$df_num,
                     size = dot_data()$dotSize,
                     shape = dot_data()$dotShape,
                     frame = input$chkFrameClusters,
                     alpha = 0.4)
      if (input$cbShowLabels != "none") {
        gg <- gg + geom_text_repel(aes_string(color = color_column_name(), label = input$cbShowLabels), vjust = -1)
      }
      
      if (!is.null(ranges$x) & !is.null(ranges$y)) {
        gg <- gg + scale_x_continuous(limits = ranges$x)
        gg <- gg + scale_y_continuous(limits = ranges$y)
      }
      
      # Remove legend
      if (nrow(unique(dt$df[,color_column_name()])) > 40) {
        gg <- gg + theme(legend.position = "none")
      }
      
      # Scatter the PCA
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }
    })
    
    gg 
  })
  
  ## Here it renders the histograms
  output$histogram_plot = renderPlot({
    df_hist <- hist_data()$df_num
    ctr <- hist_data()$col_to_remove
    if (is.null(df_hist)) return(NULL)
    
    df_hist %>% 
      gather(key="key", value="value", -ctr) %>%
      ggplot(aes(value)) + 
      geom_histogram(aes_string(colour = color_column_name())) +
      facet_wrap(~key, scales = "free")
  })
  
  # Here it renders the EFA
  output$efa_plot = renderPlot({
    dt_efa <- efa_data()
    if (is.null(dt_efa)) return(NULL)
    withProgress(message = "Rendering ", value = 0, {
      gg <- autoplot(dt_efa$efa, 
                     data = dt_efa$df_num,
                     size = dot_data()$dotSize,
                     shape = dot_data()$dotShape,
                     alpha = 0.4)
      
      if (input$cbShowLabels != "none") {
        gg <- gg + geom_text_repel(aes_string(x = "x", y = "y", color = color_column_name(), label = input$cbShowLabels), vjust = -1)
      }
      
      if (!is.null(ranges$x) & !is.null(ranges$y)) {
        gg <- gg + scale_x_continuous(limits = ranges$x)
        gg <- gg + scale_y_continuous(limits = ranges$y)
      }
      
      if (input$chkFrameClusters) {
        gg <- gg + stat_ellipse(aes_string(x = "x", y = "y", colour = color_column_name()), type = "t")
      }
      
      # Format title and axis
      gg <- gg + theme(plot.title = element_text(hjust=0.5, vjust=0.5, size=20, face = "bold"))
      gg <- gg + theme(axis.title.x = element_text(size=20))
      gg <- gg + theme(axis.title.y = element_text(size=20))
      
      # Scatter the PCA
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }
    })
    
    gg
  })
  
  
  # Ancillary render functions -----------------------------------------------------
  output$kms_elbow = renderPlot({
    dt_kms <- kms_data()
    if (is.null(dt_kms)) return(NULL)
    
    set.seed(42)
    fviz_nbclust(dt_kms$df_filtered, kmeans, method = "wss")
  })
  
  output$kms_silhouette = renderPlot({
    dt_kms <- kms_data()
    if (is.null(dt_kms)) return(NULL)
    
    set.seed(42)
    fviz_nbclust(dt_kms$df_filtered, kmeans, method = "silhouette")
  })
  
  output$kms_gap = renderPlot({
    dt_kms <- kms_data()
    if (is.null(dt_kms)) return(NULL)
    
    set.seed(42)
    gap_stat <- clusGap(dt_kms$df_filtered, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
    fviz_gap_stat(gap_stat)
  })
  
  # Here it renders the cumulative variance
  output$cumulative_variance_plots = renderPlot({
    dt_pca <- pca_data()
    if (is.null(dt_pca)) return(NULL)
    
    dt_pca$var_exp %>%
      rename(
        `Cumulative Variance Explained` = cum_var_exp
      ) %>%
      gather(key = key, value = value, `Cumulative Variance Explained`) %>%
      ggplot(aes(pc, value, group = key)) +
      geom_point() +
      geom_line() +
      lims(y = c(0, 1)) +
      labs(y = "Variance",
           title = "Variance explained by each principal component")
  })
  
  # Here it renders the variance
  output$variance_plots = renderPlot({
    dt_pca <- pca_data()
    if (is.null(dt_pca)) return(NULL)
    
    dt_pca$var_exp %>%
      rename(
        `Variance Explained` = var_exp,
        `Cumulative Variance Explained` = cum_var_exp
      ) %>%
      gather(key = key, value = value, `Variance Explained`) %>%
      ggplot(aes(pc, value, group = key)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      lims(y = c(0, 1)) +
      labs(y = "Variance",
           title = "Variance explained by each principal component")
  })
  
  # Print loading details
  output$pca_details <- renderPrint({
    options(max.print=999999)
    print(pca_data()$pca$rotation)
    summary(pca_data()$pca)
  })
  
  output$dataframe_summary <- renderPrint({
    options(max.print=999999)
    summary(filtered_data()$df_num)
  })
  
  
  # Text rendering functions -----------------------------------------------------
  
  #This previews the CSV data file
  output$filetable <- renderDataTable({
    filtered_data()$df_num
  })
  
  # Print t-SNE observation details
  output$tsne_observationInfo <- renderTable({
    df <-tsne_data()
    if (is.null(df)) return(NULL)
    
    nearPoints(df, input$tsne_plot_hover, maxpoints = 3)
  })
  
  # Print MDS observation details
  output$mds_observationInfo <- renderTable({
    df <-mds_data()
    if (is.null(df)) return(NULL)
    
    nearPoints(df, input$mds_plot_hover, maxpoints = 3)
  })
  
  # Print EFA observation details
  output$efa_observationInfo <- renderTable({
    df <-efa_data()
    if (is.null(df)) return(NULL)
    
    nearPoints(df, input$mds_plot_hover, maxpoints = 3)
  })
  
  # Print Spider plot observation details
  output$spd_observationInfo <- renderTable({
    df <- spd_data()
    if (is.null(df)) return(NULL)
    
    nearPoints(df, input$spd_plot_hover, maxpoints = 3)
  })
  
  # Reacts to vover event to display dot data
  output$hovered_point <- renderText({
    dt_pca <- pca_data()
    if (is.null(dt_pca)) return(NULL)
    
    toto <- brushedPoints(dt_pca$pcs_df, input$pca_plot_brush)
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    
    paste0("hover: ", xy_str(input$pca_plot_hover))
  })
  
  
  # Wigget initialization functions -----------------------------------------------------
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    load_experience_csv(input)
  })
  
  # Populate treatment selector
  output$cbTreatmentSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_treatment_selection(df,
                             "cbTreatmentSelection",
                             "Select treatments to be displayed",
                             "count > 3")
  })
  
  # Populate plants selector
  output$cbPlantSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_plant_selection(df)
  })
  
  # Populate palette selector
  output$cbPaletteSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fill_palette_selector("cbPaletteSelector")
  })
  
  #The following set of functions populate the x axis selectors
  output$cbSelectedVariables <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    new_df <- new_df %>% subset(select=-c(trunc_day_after_start))
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
    pickerInput(
      inputId = "cbSelectedVariables", 
      label = "Selected variables:", 
      choices = cb_options,
      options = list(
        `selected-text-format` = "count > 5",
        `count-selected-text` = "{0} attributes selelcted",
        `actions-box` = TRUE,
        `deselect-all-text` = "Select none",
        `select-all-text` = "Select all"
      ), 
      selected = cb_options,
      multiple = TRUE
    )
  })
  
  # Populate scatter selector
  output$cbSplitScatter <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    build_discret_selectImput(df, "cbSplitScatter",  "Separate graphs using:", "treament", c("none", "trunc_day_after_start"))
  })
  
  #The following set of functions populate the dot size selectors
  output$dotSize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    build_numeric_selectImput(df, "dotSize", "Dot Size:", "none")
  })
  
  # The following set of functions populate the dot size selectors
  output$dotShape <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]

    build_string_selectImput(df, "dotShape", "Dot Shape:", "none")
  })
  
  output$colorBy <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    dsnames <- names(df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
    if ('treatment' %in% cb_options) {
      selected_choice <- 'treatment'
    } else {
      selected_choice <- 'none'
    }
    selectInput("colorBy", "Color dots using:", choices = c('none', cb_options), selected = selected_choice)
  })
  
  output$chkFrameClusters <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkFrameClusters", "Frame clusters", FALSE)
  })
  
  output$chkShowOutliers <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if ("outlier" %in% colnames(df)) {
      checkboxInput("chkShowOutliers", paste('Show outliers (', length(which(df$outlier==1)), ')', sep=''), TRUE)
    } else {
      checkboxInput("chkShowOutliers", 'No outliers detected, option ignored', FALSE)
    }
  })
  
  output$chkShowLoadings <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkShowLoadings", "Show component loadings", FALSE)
  })
  
  output$ni_xPrincipalComponent <- renderUI({
    req(input$cbSelectedVariables)
    numericInput(inputId = "ni_xPrincipalComponent", label = "PCX", value = 1, min = 1, max = length(input$cbSelectedVariables) -1)
  })
  
  output$ni_yPrincipalComponent <- renderUI({
    req(input$cbSelectedVariables)
    numericInput(inputId = "ni_yPrincipalComponent", label = "PCY", value = 2, min = 1, max = length(input$cbSelectedVariables) -1)
  })
  
  output$cbShowLabels <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    build_string_selectImput(df, 
                             "cbShowLabels",  
                             "Show plant name (if all dots are displayed graph will become cluttered:", 
                             "none")
  })
  
  output$cbShowLabels <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    build_string_selectImput(df, 
                             "cbShowLabels",  
                             "Show plant name (if all dots are displayed graph will become cluttered:", 
                             "none")
  })
  
  output$cbDateTimeSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_time_selection(df)
  })
  
  # Perplexity t-SNE only
  output$perplexitySelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    sliderInput(inputId =  "perplexitySelector", 
                label = "Perplexity", 
                min = 0, 
                max = 100, 
                value = 30,
                step = 1)
  })
  
  # Centers for K-means only
  output$centerCount <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    sliderInput(inputId =  "centerCount", 
                label = "Centers", 
                min = 0, 
                max = 100, 
                value = 3,
                step = 1)
  })
  
  # Listeners -----------------------------------------------------
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$pca_plot_dblclick, {
    brush <- input$pca_plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)