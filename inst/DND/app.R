library(shiny)
library(shinyMobile)
Classes = c(
  "Barbarian",
  "Bard",
  "Cleric",
  "Druid",
  "Fighter",
  "Monk",
  "Paladin",
  "Ranger",
  "Rogue",
  "Sorcerer",
  "Warlock",
  "Wizard"
)
Person <- R6::R6Class("Person",
                      list(
                        name = NULL,
                        age = NA,
                        initialize = function(name, age = NA) {
                          self$name <- name
                          self$age <- age
                        },
                        print = function(...) {
                          cat("Person: \n")
                          cat("  Name: ", self$name, "\n", sep = "")
                          cat("  Age:  ", self$age, "\n", sep = "")
                          invisible(self)
                        }
                      ))


logging::setLevel("DEBUG")

ui <- f7Page(
  title = NULL,
  darkmode = FALSE,
  init = f7Init(skin = "auto",
                theme = "dark"),

  f7SingleLayout(navbar = f7Navbar(
    title = "DND App",
    hairline = FALSE,
    shadow = TRUE
  )),

  #Allegedly the main content
  f7Card(
    verbatimTextOutput("Person"),
    f7Text(
      inputId = "CharacterName",
      label = "Character Name:",
      value = "Faustus"
    ),
    f7Select(
      inputId = "Class",
      label = "Class:",
      choices = Classes
    ),

    title = "Character Generation"
  )
)

server <- function(input, output) {
  # outputPerson = reactive({
  #   newPerson = Person$new(name = "Crap")
  # })

  output$Person = "Crap"
}

shinyApp(ui = ui, server = server)
