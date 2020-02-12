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
SkillProficiencies = c(
  "Arcana",
  "Acrobatics",
  "Animal Handling",
  "Athletics",
  "Deception",
  "History",
  "Insight",
  "Intimidation",
  "Investigation",
  "Medicine",
  "Nature",
  "Perception",
  "Performance",
  "Persuasion",
  "Sleight of Hand",
  "Stealth",
  "Survival",
  "Religion"
)
Races = c(
  "Dragonborn",
  "Dwarf",
  "Hill Dwarf",
  "Elf",
  "High Elf",
  "Gnome",
  "Rock Gnome",
  "Half Elf",
  "Half Orc",
  "Halfling",
  "Lightfoot Halfling",
  "Human",
  "Tiefling"
)

library(R6)
Character = R6::R6Class(
  "Character",
  public = list(
    Name = NULL,
    Race = NULL,
    Class = NULL,
    Level = 1,
    Cantrips = NULL,
    Level1Spells = NULL,
    SkillProficiencies = NULL,
    EquipmentProficiencies = NULL,
    SavingThrowProficiencies = NULL,
    initialize = function(Name,
                          Race,
                          Class,
                          Level = 1,
                          Cantrips,
                          Level1Spells,
                          SkillProficiencies,
                          EquipmentProficiencies,
                          SavingThrowProficiencies) {
      stopifnot(is.character(Name), length(Name) == 1)
      stopifnot(Race %in% Races)
      stopifnot(Class %in% Classes)
      stopifnot(is.integer(Level), Level <= 20L)
      self$Name <- Name
      self$Race <- Race
      self$Class <- Class
      self$Level <- Level
      self$Cantrips <- Cantrips
      self$Level1Spells <- Level1Spells
      self$SkillProficiencies <- SkillProficiencies
      self$EquipmentProficiencies <- EquipmentProficiencies
      self$SavingThrowProficiencies <- SavingThrowProficiencies
      return(self)
    },
    print = function(...) {
      cat(paste0("Name: ", self$Name, "\n"))
      cat(paste0("Race: ", self$Race, "\n"))
      cat(paste0("Class: ", self$Class, "\n"))
      cat(paste0("Level: ", self$Level, "\n"))
      cat(paste0("Cantrips: ", self$Cantrips, "\n"))
      cat(paste0("Level 1 Spells: ", self$Level1Spells, "\n"))
      cat(paste0("Skill Proficiencies: ", self$SkillProficiencies, "\n"))
      cat(paste0(
        "Equipment Proficiencies: ",
        self$EquipmentProficiencies,
        "\n"
      ))
      cat(paste0(
        "Saving Throw Proficiencies: ",
        self$SavingThrowProficiencies,
        "\n"
      ))
      invisible(self)
    }
  )
)


