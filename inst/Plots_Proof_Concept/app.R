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

            numericInput("SSID", label = "SSID", value = 2),

            hr(),
            actionButton("resetDefaults", "Reset to Defaults"),
            hr(),

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

    #Read in data
    dataFrame <- reactive({
        logging::loginfo("dataFrame()")
        #Depends on SSID
        #TODO: Fetch data from somewhere
        if (input$SSID == 1) {
            data = data.frame(iris[c(5,1:4)])
        } else {
            data = data.frame(mtcars[, c(2,1,3:11)])
        }
        data$Sample = 1:nrow(data)
        data
        #TODO: Format the data (better) such that the data frame is consistent across sample sets
        #It will likely be prudent to track the log data and the meta data separately.
        #TODO: log transform the data prior to performing PCA on it
    })

    #Initialize some UI stuff
    output$samplesToRemove <- renderUI({
        #Depends on dataFrame(SSID)
        logging::loginfo("sampleToRemove")
        checkboxGroupInput("samplesToRemove",
                           label = "Samples to remove",
                           choices = dataFrame()[, which(colnames(dataFrame()) == "Sample")])
    })
    output$columnChoices <- renderUI({
        #Depends on dataFrame(SSID)
        logging::loginfo("Group")
        selectInput(
            "Group",
            "Grouping Column",
            choices = colnames(dataFrame()),
            selected = colnames(dataFrame())[1]
        )
    })

    #Compute the PCA
    prinCompData <- reactive({
        logging::loginfo("prinCompData()")
        #Depends on samplesToRemove, dataFrame(SSID), and Group
        data = dataFrame()
        if (!is.null(input$samplesToRemove)) {
            data = data[-(which(data$Sample %in% input$samplesToRemove)),]
        }
        PCAdata = princomp(data[, 2:ncol(data)], cor = TRUE)
        if(is.null(input$Group)){
            PCAdata = data.frame(
                PCAdata$scores,
                "Group" = as.factor(as.character(data[,1])),
                "Sample" = data$Sample
            )
        } else {
            PCAdata = data.frame(
                PCAdata$scores,
                "Group" = as.factor(as.character(data[,which(colnames(data) == input$Group)])),
                "Sample" = data$Sample
            )
        }
        PCAdata = PCAdata[order(PCAdata$Group),]
        PCAdata
    })

    observeEvent(input$Group,
                 {
                     #Render some plot UI stuff
                     output$pchPanel <- renderUI({
                         logging::loginfo("pchPanel")
                         #Depends on prinCompDate(samplesToRemove, dataFrame(SSID)), dataFrame(SSID), customPCH(self)
                         lev <- sort(unique(as.character(prinCompData()$Group)))
                         customPCH <- validPCH[length(lev)]

                         lapply(seq_along(lev), function(i) {
                             selectInput(
                                 inputId = paste0("pch", lev[i]),
                                 label = paste0("Choose point type for ", lev[i]),
                                 choices = validPCH,
                                 selected = customPCH()[i]
                             )
                         })
                     })
                     output$colorPanel <- renderUI({
                         #Depends on prinCompDate(samplesToRemove, dataFrame(SSID)), dataFrame(SSID), customColors(self)
                         logging::loginfo("colorPanel")
                         lev <- sort(unique(as.character(prinCompData()$Group)))
                         customColors <- gg_fill_hue(length(lev))

                         lapply(seq_along(lev), function(i) {
                             colourInput(
                                 inputId = paste0("col", lev[i]),
                                 label = paste0("Choose color for ", lev[i]),
                                 value = unique(customColors())[i]
                             )
                         })
                     })
                 })

    #Define the colors
    customColors <- reactive({
        #Depends on prinCompDate(samplesToRemove, dataFrame(SSID)), dataFrame(SSID), colorPanel(self)
        logging::loginfo("customColors()")
        customColors <-
            paste0("c(",
                   paste0("input$col", sort(
                       as.character(prinCompData()$Group)
                   ), collapse = ", "),
                   ")")
        customColors <-
            eval(parse(text = customColors))
        if(any(customColors == "#FFFFFF")){
            lev <- sort(unique(as.character(prinCompData()$Group)))
            customColors <- gg_fill_hue(length(lev))
        }
        customColors
    })

    #Define the PCH
    customPCH <- reactive({
        logging::loginfo("customPCH()")
        customPCH <-
            paste0("c(",
                   paste0("input$pch", sort(as.character(
                       unique(prinCompData()$Group)
                   )), collapse = ", "),
                   ")")
        logging::logdebug(names(input)[grep("pch", names(input))])
        logging::logdebug(grep("pch", names(input)))
        # if(!is.null(names(input)[grep("pch", names(input))])){
        #     logging::logdebug(paste0(input[grep("pch", names(input))]))
        # }
        logging::logdebug(customPCH)
        customPCH <- eval(parse(text = customPCH))
        logging::logdebug(customPCH)
        if(any(customPCH == "")) {
            logging::logwarn("custom PCH had empty value")
            customPCH[1:length(unique(prinCompData()$Group))] = "circle"
        }
        customPCH
    })

    #Create the plot
    PCA3D <- reactive({
        logging::loginfo("PCA3D()")
        plotColors = customColors()
        plotPCH = customPCH()

        plotData = prinCompData()
        # To prevent errors
        req(length(plotColors) == nrow(plotData))
        req(length(plotPCH) == length(unique(plotData$Group)))

        plotData$Color = plotColors

        textOptions = list(family = "sans serif",
                           size = 14,
                           color = toRGB("grey50"))

        if (length(unique(plotPCH)) > 1) {
            PCA3D <- plot_ly(
                plotData,
                x = plotData$Comp.1,
                y = plotData$Comp.2,
                z = plotData$Comp.3,
                text =  paste0("Sample: ", plotData$Sample),
                type = "scatter3d",
                mode = "markers",
                color = plotData$Group,
                colors = plotColors,
                symbol = ~ Group,
                symbols = plotPCH,
                marker = list(size = input$pointSize)
            )
        }
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
                colors = plotColors,
                symbol = I(unique(plotPCH)),
                marker = list(size = input$pointSize)
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

    #Render plot to web app
    output$plot <- renderPlotly({
        logging::loginfo("output$plot")
        PCA3D()
    })

    #Reset to default args
    observeEvent(input$resetDefaults,
                 {
                     output$pchPanel <- renderUI({
                         logging::loginfo("resetDefaults pchPanel")
                         lev <-
                             sort(unique(as.character(prinCompData()$Group)))
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
                         logging::loginfo("resetDefaults colorPanel")
                         lev <-
                             sort(unique(as.character(prinCompData()$Group)))
                         customColors <- gg_fill_hue(length(lev))

                         lapply(seq_along(lev), function(i) {
                             colourInput(
                                 inputId = paste0("col", lev[i]),
                                 label = paste0("Choose color for ", lev[i]),
                                 value = unique(customColors)[i]
                             )
                         })
                     })

                     logging::loginfo("Reset Size")
                     updateSliderInput(session, "pointSize", value = 4)

                     logging::loginfo("Reset Labels")
                     updateCheckboxInput(session, "includeLabels", value = FALSE)

                     logging::loginfo("Reset samples to remove")
                     updateCheckboxGroupInput(session, "samplesToRemove", selected = character(0))

                     logging::loginfo("Reset Group")
                     updateSelectInput(session, "Group", selected = 1)
                 })
}

shinyApp(ui, server)
