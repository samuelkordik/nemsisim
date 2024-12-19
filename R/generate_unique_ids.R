#' Generate Unique IDs
#'
#' Generates a character vector of random unique IDs.
#'
#' @param n number of IDs to generate
#' @param length length of each ID, defaults to 50 characters.
#' @param seed seed for reproducibility, default is 2126
#' @param charset optionally specify characters to use. Defaults to alphanumeric.
#'
#' @return character vector of unique IDs
#' @export
#'
#' @examples
#' ## Generate 10 unique IDs
#' generate_unique_ids(10)
#'
#' ## Unique IDs can be numeric only
#' generate_unique_ids(10, length=8, charset = c(0:9))
#'
#' ## Unique IDs can also include uppercase chars
#' generate_unique_ids(10, charset = c(LETTERS, letters, 0:9))
#'
#' ## Unique IDs can be hexadecimal
#' generate_unique_ids(10, charset = c(0:9, letters[1:6]))
#'
generate_unique_ids <- function(n,
                                length = 50,
                                charset = c(letters, 0:9),
                                seed = 2126) {

  attempt::stop_if_not(n, is.numeric, "`n` must be numeric.")
  attempt::stop_if_not(length, is.numeric, "`length` must be numeric")

  withr::with_seed(
    seed = seed,
    {

      if(n > length(charset)^length) {
        stop("Requested number of IDs exceeds the maximum possible unique combinations.")
      }


      unique_ids <- character(0)
      while (length(unique_ids) < n) {
        batch_size <- max(10 * n, n)  # Generate more than needed to ensure uniqueness
        batch <- replicate(batch_size,
                           paste0(sample(charset, length, replace = TRUE), collapse = ""))

        # Add to the unique set
        unique_ids <- unique(c(unique_ids, batch))
      }

      return(unique_ids[1:n])
    }
  )
}
