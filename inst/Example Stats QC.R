library(shiny)

fetch_file_dictionary = function() {
  # curl FMS to return dictionary with entries of the form
  # {
  #   "SSID":"some-guid",
  #   "SSlimsID":"SS-int",
  #   "statsService": {
  #     "normalizationServiceResponseToken":"token",
  #     "portalFileToken":"token",
  #     "statsFileToken":"token",
  #     "meanTableToken":"token",
  #     "percentFillTableToken":"token",
  #   }
  #   "plotsService"{
  #     "plotFile1":"token",
  #     ...
  #   }
  #   "someOtherService":{
  #     "someOtherFile1":":token",
  #     ...
  #   }
  # }
  # Note you'll have to get the study plan too from somewhere.
  # Also note that this leaves a lot of manual steps up to the statistician. Which is fine, this is the bare minimum that
  # makes the job easier while at the same time still being largely manual.
  return(NULL)
}

ui = fluidPage(
  mainPanel(
    # This assumes that statisticians are getting Stats QC required emails by limsID key.
    # GUID would also be fine, but they'd have to copy/paste that.
    textInput("limsID", label = h3("SS Lims ID"), value = "SS-123"),
    downloadLink("downloadData", label = "Download"),
    actionButton("statsQC", label = h3("Stats QC Complete")),
  )
)

server = function(session, input, output) {

  #TODO: look-up by lims ID all the files associated with that SSID. - i.e. fix fetch_file_dictionary function
  fileDictionary = reactive({
    fetch_file_dictionary(input$limsID)
  })

  #TODO: Zip all the files to a single "file" blob so it can be downloaded in one click.
  # https://stackoverflow.com/questions/26881368/shiny-download-zip-archive
  #TODO: A bunch of stuff here I've not thought of yet
  #TODO: Associate that zip file with the downloadLink in ui.
  # output$downloadData =
  #TODO: Figure out how to make the call above this a reactive call when limsID changes...

  observeEvent(input$statsQC, {
    showModal(modalDialog(paste0("Set ", input$limsID, " to Stats QC Complete?"),
      title=input$limsID,
      footer = tagList(actionButton("confirmQC", "QC Complete"),
                       modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$confirmQC, {
    #TODO: Send SNS QC complete message
    #quit() the app? or go one step further and launch another modal message that confirms the QC message was sent?
    removeModal()
  })

}

shinyApp(ui = ui, server = server)
