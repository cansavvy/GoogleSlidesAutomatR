#' Make a new slide
#'
#' Make a new slide page with a given slides id
#'
#' @param access_token
#'
#' @return factor
#' @export
#' @examples
#'
#' make_new_slide(slides_id)

# Make a function that makes a new slide
make_new_slide <- function(slides_id) {
  # Make a new slide in the slide deck provided by the slide_id
  #
  # Args:
  #   slides_id: the slide ID of the google Slide set you are making a new slide page in.
  #
  # Returns:
  #   Uses rgoogleslides to create a new slide

  # Start up a request
  requests <- add_create_slide_page_request(predefined_layout = "BLANK")

  # Commit to the slides
  commit_to_slides(slides_id, requests)

  # Get slide details
  slide_details <- get_slides_properties(slides_id)

  # Get the newly made slide page id
  slide_page <- tail(slide_details$slides$objectId, n = 1)

  return(slide_page)
}
