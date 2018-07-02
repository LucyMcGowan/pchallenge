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
    if (consented()) {

      f <- write_report()
      if (isTRUE(copy_report(f))) {

        if (visit_survey_monkey()) {
          if (!("./final_analysis" %in% list.dirs())) {
            dir.create("final_analysis")
          }

          id <- stringi::stri_rand_strings(n = 1, length = 50)

          utils::savehistory(glue::glue("final_analysis/{id}_history"))
          d <- matahari::dance_tbl()[, c("expr", "dt")]
          analysis <- tibble::add_column(id = id, d)
          save(analysis,
               file = glue::glue("final_analysis/{id}_analysis.rda"))

          utils::browseURL(glue::glue("https://www.surveymonkey.com/r/8LPW6XM?id={id}"))
        }
      }
    }
  }
}

consented <- function() {
  if (is.null(matahari::dance_tbl())) {
    no_consent()
    return(FALSE)
  }
  d <- matahari::dance_tbl()
  if (!any(purrr::map_lgl(d$expr, consented_string))) {
    no_consent()
    return(FALSE)
  }
  TRUE
}

consented_string <- function(x) {
  if (is.character(x)) {
    grepl("consented", x)
  } else {
    FALSE
  }
}
no_consent <- function() {
  rstudioapi::showQuestion("No Consent",
                           message = glue::glue("We do not have a record of your consent. ",
                                                "If you agree to participate in this challenge, please ",
                                                "run start_challenge()."),
                           "Okay",
                           "cancel"
  )
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
                           message = glue::glue("A file will open with your R report. \n  ",
                                                "* Press ctrl + A (cmd + A on a mac) to select the text \n  ",
                                                "* Press ctrl + C (cmd + C on a mac) to copy the text \n  ",
                                                "* Press press Save. \n\n",
                                                "Note: this file will look like a garbled mess, ",
                                                "don't worry we can read it with R."),
                           "Got it",
                           "cancel")
}
write_report <- function() {
  # TODO change this if matahari PR is accepted
  # d <- matahari::dance_report()
  d <- my_report()
  file <- tempfile()
  writeLines(d, file)
  file
}

copied_report <- function() {
  rstudioapi::showQuestion("Copied Report?",
                           message = glue::glue("Did you successfully copy your full report?"),
                           "Yes, I have the full report copied to my clipboard",
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

## TODO change this if PR is accepted to matahari package to remove clipr necessity
my_report <- function() {
  matahari:::add_session_info()
  d <- matahari::dance_tbl()[, c("expr", "dt")]
  jsonlite::base64_enc(serialize(d, NULL))
}
