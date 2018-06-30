#' Turn in challenge results, launch final survey
#'
#' This will open a browser with a final survey to submit your results.
#' If you do not have pop-ups enabled, you will get an error message -
#' click "Try Again" and the survey should load.
#'
#' @export
#'
turn_in_challenge <- function() {
  .turn_in_challenge()
}

.turn_in_challenge <- function() {
  if (ready()) {

    f <- write_report()
    if (isTRUE(copy_report(f))) {

      if (visit_survey_monkey()) {
        if (!("./final_analysis" %in% list.dirs())) {
          dir.create("final_analysis")
        }

        id <- stringi::stri_rand_strings(n = 1, length = 50)

        if (is.null(matahari::dance_tbl())) {
          utils::savehistory(glue::glue("final_analysis/{id}_history"))
        } else {
          analysis <- tibble::add_column(id = id,
                                         matahari::dance_tbl())
          save(analysis,
               file = glue::glue("final_analysis/{id}_analysis.rda"))
        }
        utils::browseURL(glue::glue("https://www.surveymonkey.com/r/8LPW6XM?id={id}"))
      }
      matahari::dance_start()
    }
  }
}

ready <- function() {
  rstudioapi::showQuestion("Ready?",
                           "Are you ready to turn in your final result?",
                           "Yes",
                           "No")
}
visit_survey_monkey <- function() {
  rstudioapi::showQuestion("SurveyMonkey",
                           message = "Be sure pop-ups are enabled on your browser. \n\nAre you ready to visit SurveyMonkey to input your final results?",
                           "Yes",
                           "cancel")
}

explain_report <- function() {
  rstudioapi::showQuestion(title = "Explain Report",
                           message = glue::glue("A file will open with your R report. Press ctrl + A ",
                                                "to select the text and ctrl + C to copy it.\n\n",
                                                "Note: this file will look like a garbled mess, ",
                                                "don't worry we can read it with R."),
                           "Got it",
                           "cancel")
}
write_report <- function() {
  matahari::dance_stop()
  d <- matahari::dance_report()
  file <- tempfile()
  writeLines(d, file)
  file
}

copied_report <- function() {
  rstudioapi::showQuestion("Copied Report?",
                           message = glue::glue("Did you successfully copy your full report? ",
                                                "If not, click \"No, return to report\", press ctrl + A ",
                                                "to select the report and ctrl + C to copy it."),
                           "Yes, I have the report copied to my clipboard",
                           "No, return to report")
}

copy_report <- function(f) {
  if (explain_report()){
    utils::file.edit(f)
    if (!copied_report()) {
      copy_report(f)
    } else TRUE
  }
}
