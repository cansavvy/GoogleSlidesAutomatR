#' Add image to slide
#'
#' For a given slide page and slide ID, add an image to it
#'
#' @param image_url: The url to an image you would like to add to a slide
#' @param slide_page: the slide ID you want to add an image to
#' @param slides_id: The ID of slide set that contains the slide you are adding an image to

#'
#' @return factor
#' @export
#' @examples
#' token <- rgoogleslides::authorize()
#' token <- token$credentials$access_token
#' rgoogleslides::authorize(authorize_from_secret(token))
#'
#'
add_image <- function(image_url,
                      slide_page,
                      slides_id){
  # Add an image to a given slide in a given slide set.
  #
  # Args:
  #   image_url: The url to an image you would like to add to a slide
  #   slide_page: the slide ID you want to add an image to
  #   slides_id: The ID of slide set that contains the slide you are adding an image to
  #
  # Returns:
  #   Uses rgoogleslides to add an image to the given slide. Also returns a data.frame
  #   with the image url, slide page id, and image id.

  # Get the position details of the element on the slide
  page_element <- suppressWarnings(
    aligned_page_element_property(slide_page_id = slide_page,
                                  align = "full")
  )

  # Create request
  request <- add_create_image_request(url = image_url,
                                      page_element_property = page_element)

  # Commit image to slide
  response <- commit_to_slides(slides_id, request)

  # Get image info
  new_image_info <- data.frame(
    image_url = image_url,
    page_id = slide_page,
    image_id = response$replies[[1]][[1]]
  )

  # Return this
  return(new_image_info)
}
