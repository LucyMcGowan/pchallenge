#' Start P-value challenge
#'
#' Run this to download the data set and begin the P-value challenge.
#' @export
#'
start_challenge <- function() {
  .start_challenge()
}

.start_challenge <- function() {
  matahari::dance_start()
  if (consent()) {
    utils::data("df", package = "pchallenge")
  }
}

consent <- function() {
  consented <- start_question()
  if (consented) {
    message("You've agreed to start the challenge.")
    d <- tibble::tibble(
      expr = "consented",
      value = TRUE,
      path = NA,
      contents = NA,
      selection = NA,
      dt = Sys.time())
    assign(".dance", d, envir = .GlobalEnv)
    return(TRUE)
  } else {
    message("You did not agree to participate in this challenge.")
    d <- tibble::tibble(
      expr = "did not consent",
      value = FALSE,
      path = NA,
      contents = NA,
      selection = NA,
      dt = Sys.time())
    assign(".dance", d, envir = .GlobalEnv)
    matahari::dance_stop()
    return(FALSE)
  }
}
start_question <- function() {
  rstudioapi::showQuestion(title = "Ready?",
                           message = glue::glue("We are conducting research on the ways that people ",
                                                "use data analysis and data science tools. We will not collect ",
                                                "any personally identifiable information about you for the purposes ",
                                                "of this research. The potential risks to you are small. The potential ",
                                                "benefits to the community of data scientists, developers, and professors ",
                                                "are very high – we will be able to learn about the process of how people ",
                                                "analyze data which can improve how we use data in business, scientific studies, ",
                                                "and other areas.\n\n",
                                                "If you join this study, you will participate in the data analysis experiment. ",
                                                "You will be asked to analyze a dataset and we will record all of the R code you are ",
                                                "using while you perform this analysis in RStudio Cloud. You will then submit this for ",
                                                "review. We also will ask you to submit a one paragraph description of the final model ",
                                                "you fit. You will then be asked to fill out some answers on SurveyMonkey. The data from ",
                                                "all participants will be compiled into a single data set by the investigators in the study ",
                                                "and then analyzed and released to the community. We plan to release all data collected from ",
                                                "this study openly online via sites like Github and Figshare.\n\n",
                                                "Please read the consent form before continuing: http://phackathon.netlify.com/consent/"),
                           "Yes, I agree to participate in this challenge",
                           "No, I do not agree to participate in this challenge")
}
