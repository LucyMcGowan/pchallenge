#' Start P-value challenge
#'
#' Run this to download the data set and begin the P-value challenge.
#' @export
#'
start_challenge <- function() {
 .start_challenge()
}

.start_challenge <- function() {
  if (start_question()) {
  matahari::dance_start()
  }
}

start_question <- function() {
  rstudioapi::showQuestion(title = "Ready?",
                           message = glue::glue("* We will be recording the commands you run in this session.\n  ",
                                                "* When you have finished, you will be prompted to copy the generated report ",
                                                "and paste it into SurveyMonkey\n  ",
                                                "* You will then provide a short description of your data analysis and enter your final p-value obtained",
                                                trim = FALSE),
                           "Sounds good!",
                           "cancel")
}
