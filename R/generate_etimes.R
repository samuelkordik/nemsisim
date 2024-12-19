#' Generates eTimes fields
#'
#' Uniformly randomly distributes incident dates across range specified; assumes
#' all are transports and fills in the remaining etimes using normal distribution
#'
#' @param .data Tibble containing, at a minimum, a unique ID. One row per incident.
#' @param startdate Start date
#' @param enddate End date
#'
#' @return .data with additional fields for eTimes
generate_etimes <- function(.data,
                            startdate,
                            enddate) {

  n <- nrow(.data)
  .data |>
    dplyr::mutate(psap_notified_etimes.01 = as.POSIXct(runif(n, min = startdate, max = enddate)),
                  dispatch_time = dminutes(abs(rlnorm(n, meanlog = -.75, sdlog= 5/60))),
                  chute_time = dminutes(abs(rlnorm(n, meanlog = 0, sdlog = 20/60))),
                  response_time = dminutes(abs(rnorm(n, mean = 8, sd=1))),
                  to_patient_time = dminutes(abs(rlnorm(n, meanlog = 0.3, sdlog = 0.4))),
                  scene_time = dminutes(abs(rnorm(n, mean = 15, sd = 10))),
                  transport_time = dminutes(abs(rlnorm(n, meanlog = 1.2, sdlog = 0.9))),
                  apot_time = dminutes(abs(rnorm(n, mean = 18, sd=5))),
                  ta_time = dminutes(abs(rlnorm(n, meanlog=1,sdlog=1)))
    ) |>
    dplyr::mutate(dispatch_notified_etimes.02 = psap_notified_etimes.01,
                  unit_notified_etimes.03 = psap_notified_etimes.01 + dispatch_time) |>
    dplyr::mutate(unit_enroute_etimes.05 = unit_notified_etimes.03 + chute_time) |>
    dplyr::mutate(unit_arrived_on_scene_etimes.06 = unit_enroute_etimes.05 + response_time) |>
    dplyr::mutate(arrived_at_patient_etimes.07 = unit_arrived_on_scene_etimes.06 + to_patient_time) |>
    dplyr::mutate(unit_left_scene_etimes.09 = arrived_at_patient_etimes.07 + scene_time) |>
    dplyr::mutate(patient_arrived_at_destination_etimes.11 = unit_left_scene_etimes.09 + transport_time) |>
    dplyr::mutate(patient_transfer_of_care_etimes.12 = patient_arrived_at_destination_etimes.11 + apot_time) |>
    dplyr::mutate(unit_back_in_service_etimes.13 = patient_transfer_of_care_etimes.12 + ta_time) |>
    dplyr::select(-dplyr::ends_with("_time"))

}
