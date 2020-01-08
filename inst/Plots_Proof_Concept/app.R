library(plotly)
library(shiny)
library(shinyjs)
library(logging)
library(colourpicker)

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

            numericInput("SSID", label = "SSID", value = 1),

            sliderInput(
                "pointSize",
                label = "Point Size",
                min = 1,
                max = 8,
                value = 4
            ),

            textInput("pcaTitle", "PCA Title", value = "PCA"),

            hr(),
            checkboxInput("includeLabels", label = "Include Labels", value = FALSE),
            hr(),

            uiOutput("columnChoices"),
            uiOutput("colorPanel"),
            uiOutput("pchPanel")
        ),
        tabPanel("Samples to Remove",
                 uiOutput("samplesToRemove"))
    )),

    mainPanel(plotlyOutput("plot"))
)

server <- function(input, output, session) {
    validPCH = c("circle",
                 "square",
                 "diamond",
                 "cross",
                 "x")

    output$pchPanel <- renderUI({
        logging::logdebug("Within pchPanel")
        lev <- sort(unique(as.character(prinCompData()$Group)))
        customPCH <- validPCH[length(lev)]

        lapply(seq_along(lev), function(i) {
            selectInput(
                inputId = paste0("pch", lev[i]),
                label = paste0("Choose point type for ", lev[i]),
                choices = validPCH,
                selected = validPCH[1]
            )
        })
    })

    output$colorPanel <- renderUI({
        logging::logdebug("Within colorPanel")
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
            data = data[-(which(data$Sample %in% input$samplesToRemove)),]
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

        customPCH <-
            paste0("c(", paste0("input$pch", sort(
                as.character(unique(prinCompData()$Group))
            ), collapse = ", "), ")")
        logging::logdebug(customPCH)
        customPCH <- eval(parse(text = customPCH))
        logging::logdebug(customPCH)

        plotData = prinCompData()
        # To prevent errors
        req(length(customColors) == nrow(plotData))
        req(length(customPCH) == length(unique(plotData$Group)))

        plotData = plotData[order(plotData$Group), ]
        plotData$Color = customColors

        textOptions = list(family = "sans serif",
                           size = 14,
                           color = toRGB("grey50"))

        logging::logdebug("Rendering Plot")
        logging::logdebug(unique(customPCH))
        if(length(unique(customPCH)) > 1) {
            PCA3D <- plot_ly(
                plotData,
                x = plotData$Comp.1,
                y = plotData$Comp.2,
                z = plotData$Comp.3,
                text =  paste0("Sample: ", plotData$Sample),
                type = "scatter3d",
                mode = "markers",
                color = plotData$Group,
                colors = customColors,
                symbol = ~Group,
                symbols = customPCH,
                marker = list(size = pointSize())
            )}
        else{
            PCA3D <- plot_ly(
                plotData,
                x = plotData$Comp.1,
                y = plotData$Comp.2,
                z = plotData$Comp.3,
                text =  paste0("Sample: ", plotData$Sample),
                type = "scatter3d",
                mode = "markers",
                color = plotData$Group,
                colors = customColors,
                symbol = I(unique(customPCH)),
                marker = list(size = pointSize())
            )
            }
        PCA3D %>%
            layout(
                title = input$pcaTitle,
                scene = list(
                    xaxis = list(title = "PC1"),
                    yaxis = list(title = "PC2"),
                    zaxis = list(title = "PC3")
                ),
                legend = list(y = 0.8, yanchor = "top")
            ) %>%
            {
                #TODO: Figure out a way to avoid the warning this throws. It's not clear why.
                if (input$includeLabels) {
                    add_text(
                        .,
                        text = plotData$Sample,
                        textfont = textOptions,
                        textposition = "top",
                        showlegend = FALSE
                    )
                } else {
                    .
                }
            }
    })


}

shinyApp(ui, server)
