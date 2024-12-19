#' Generate Patients
#'
#' Generates `n` number of random patient encounters, across an optional date range.
#'
#' If `startdate` and `enddate` are not both provided, will start and end at random
#' points within the past year.
#'
#' @param n the number of patients
#' @param seed the random seed, default is 2037
#' @param startdate optional date-time to start patient charts
#' @param enddate optional date-time to end patient charts
#'
#' @importFrom withr with_seed
#' @importFrom charlatan ch_name ch_credit_card_provider ch_job
#' @importFrom attempt stop_if_not warn_if attempt
#' @importFrom dplyr mutate select rename as_tibble left_join tibble sample_n tibble
#' @importFrom stats runif rnorm rlnorm rbeta
#'
#' @return A list containing dataframes representing NEMSIS fields for patient charts.
#' @export
#'
#' @examples
#' generate_patients(10)
#' generate_patients(10, startdate = "2024-01-01", enddate = "2024-01-02")
generate_patients <- function(n,
                              seed = 2037,
                              startdate = NULL,
                              enddate = NULL) {

  # Argument checking and setting
  stop_if_not(n, is.numeric, "Please provide a numeric value for `n`")
  stop_if(xor(missing(startdate), missing(enddate)), "Both startdate and enddate must be provided.")

  if(!missing(startdate) & !missing(enddate)) {
    startdate <- attempt(as.POSIXct(startdate),
                         "startdate must be Date, POSIXct, or in a standard unambigous format.")
    enddate <- attempt(as.POSIXct(enddate),
                         "startdate must be Date, POSIXct, or in a standard unambigous format.")
  } else {
    with_seed(
      seed = seed,
      {
        range <- Sys.time() - abs(runif(2, 0, 365)) * 24 * 3600
        startdate <- min(range)
        enddate <- max(range)
      }
    )
  }

  with_seed(
    seed = seed,
    {
      # Start with erecord.01 and build incidents from there
      records <- tibble(erecord.01 = generate_unique_ids(n, seed = seed))

      # Get eTimes
      records <- records |> generate_etimes(startdate = startdate,
                                            enddate = enddate)




    }
  )








}
