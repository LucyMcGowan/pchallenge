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
  matahari::dance_stop()

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
