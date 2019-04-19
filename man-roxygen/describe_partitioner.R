#' @description `<%= func %>` uses the following direct, measure, reduce approach:
#' - **direct**: <%= director %>
#' - **measure**: <%= metric %>
#' - **reduce**: <%= reducer %>
#'
#' @examples
#' set.seed(123)
#' df <- simulate_block_data(c(3, 4, 5), lower_corr = .4, upper_corr = .6, n = 100)
#'
#' # fit partition using <%= func %>
#' partition(df, threshold = .6, partitioner = <%= func %>)
#'
#' @md
