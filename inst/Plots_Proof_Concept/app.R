library(plotly)
library(shiny)
library(shinyjs)
library(logging)

setLevel("DEBUG")

# Function that produces default gg-colours is taken from this discussion:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
# https://stackoverflow.com/questions/38822863/shiny-dynamic-colour-fill-input-for-ggplot
gg_fill_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}


ui <- fluidPage(
    titlePanel("Proof of Concept"),
    sidebarPanel(tabsetPanel(
        tabPanel(
            "Sample Set",

            #May remove this. Not clear yet.
            selectInput(
                inputId = "plotType",
                label = "Plot Type:",
                choices = c("Boxplot", "Lineplot", "RBoxplot", "2D PCA", "3D PCA"),
                selected = "3D PCA"
            ),

            numericInput("SSID", label = "SSID", value = 1),

            sliderInput(
                "pointSize",
                label = "Point Size",
                min = 1,
                max = 8,
                value = 4
            ),

            textInput("pcaTitle", "PCA Title", value = "PCA"),

            uiOutput("columnChoices"),
            uiOutput("myPanel")
        ),
        tabPanel("Samples to Remove",
                 uiOutput("samplesToRemove"))
    )),

    mainPanel(
        plotlyOutput("plot"),
        verbatimTextOutput("hover"),
        verbatimTextOutput("click")
    )
)

server <- function(input, output, session) {
    output$myPanel <- renderUI({
        logging::logdebug("Within myPanel")
        lev <- sort(unique(as.character(prinCompData()$Group)))
        customColors <- gg_fill_hue(length(lev))

        lapply(seq_along(lev), function(i) {
            colourInput(
                inputId = paste0("col", lev[i]),
                label = paste0("Choose color for ", lev[i]),
                value = customColors[i]
            )
        })
    })

    dataFrame = reactive({
        logging::logdebug("Loading data frame")
        #TODO: Fetch data from somewhere
        if (input$SSID == 1) {
            data = data.frame(cbind(iris$Species, iris[, 1:4]))
        } else {
            data = data.frame(cbind(mtcars[, "cyl"], mtcars[, 3:7]))
        }
        colnames(data)[1] = "Group"
        data$Sample = 1:nrow(data)
        data
        #TODO: Format the data (better) such that the data frame is consistent across sample sets
        #It will likely be prudent to track the log data and the meta data separately.
        #TODO: log transform the data prior to performing PCA on it
    })

    output$samplesToRemove = renderUI({
        logging::logdebug("Selecting samples to remove")
        checkboxGroupInput("samplesToRemove",
                           label = "Samples to remove",
                           choices = dataFrame()[, which(colnames(dataFrame()) == "Sample")])
    })

    output$columnChoices = renderUI({
        logging::logdebug("Selecting group column")
        selectInput(
            "Group",
            "Grouping Column",
            choices = colnames(dataFrame()),
            selected = "Group"
        )
    })

    pointSize = reactive({
        logging::logdebug("Rescaling points")
        input$pointSize
    })

    prinCompData = reactive({
        logging::logdebug("Removing Samples")
        data = dataFrame()
        if (!is.null(input$samplesToRemove)) {
            data = data[-(which(data$Sample %in% input$samplesToRemove)), ]
        }
        PCAdata = princomp(data[, 2:ncol(data)], cor = TRUE)
        PCAdata = data.frame(
            PCAdata$scores,
            "Group" = as.factor(data$Group),
            "Sample" = data$Sample
        )
        PCAdata
    })

    output$plot <- renderPlotly({
        customColors <-
            paste0("c(", paste0("input$col", sort(
                as.character(prinCompData()$Group)
            ), collapse = ", "), ")")
        customColors <- eval(parse(text = customColors))

        plotData = prinCompData()
        # To prevent errors
        req(length(customColors) == nrow(plotData))

        plotData = plotData[order(plotData$Group),]
        plotData$Color = customColors

        logging::logdebug("Rendering Plot")
        PCA3D <- plot_ly(
            prinCompData(),
            x = plotData$Comp.1,
            y = plotData$Comp.2,
            z = plotData$Comp.3,
            type = "scatter3d",
            mode = "markers",
            color = plotData$Group,
            colors = customColors,
            marker = list(size = pointSize())
        ) %>%
            layout(
                title = input$pcaTitle,
                scene = list(
                    xaxis = list(title = "PC1"),
                    yaxis = list(title = "PC2"),
                    zaxis = list(title = "PC3")
                )
            ) %>%
            add_trace(
                text = paste0("Sample: ", plotData$Sample),
                hoverinfo = text,
                showlegend = FALSE
            ) %>%
            add_annotations(
                text = input$Group,
                xref = "paper",
                yref = "paper",
                x = 1.02,
                xanchor = "left",
                y = 0.8,
                yanchor = "bottom",
                legendtitle = TRUE,
                showarrow = FALSE
            ) %>%
            layout(legend = list(y = 0.8, yanchor = "top"))
    })


}

shinyApp(ui, server)
