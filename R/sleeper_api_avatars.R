# Sleeper API Endpoints for Avatars
# Full-size images or thumbnails
# Load required packages
pacman::p_load(httr, jsonlite, glue)

#' Retrieve Sleeper API Avatar Images
#'
#' @description
#' Fetches avatar images from the Sleeper API based on avatar ID and desired image type.
#' The function can retrieve either full-size avatars or thumbnails.
#'
#' @param avatar_id Character string representing the avatar identifier
#' @param image_type Character string specifying the avatar image size to retrieve.
#'                  Options: NULL, "full", "full-size", "full size", "thumb", "thumbnail".
#'                  Default is NULL (returns full-size avatar).
#'
#' @return The raw content of the avatar image (binary data)
#'
#' @examples
#' \dontrun{
#' # Get full-size avatar
#' avatar_data <- get_avatars("abc123")
#'
#' # Get thumbnail avatar
#' avatar_thumb <- get_avatars("xyz789", image_type = "thumb", save_path = "outputs/")
#' }
#'
#' @importFrom glue glue
#' @importFrom httr GET content
#'
#' @export
get_avatars <- function(avatar_id, image_type = NULL, save_path = "outputs/") {
  # Fetch avatar from Sleeper API
  if (is.null(image_type) || image_type %in% c("full", "full-size", "full size")) {
    # Full Size Avatar
    url <- glue::glue("https://sleepercdn.com/avatars/{avatar_id}")
    # Log the URL being fetched
    log_message(glue::glue("Fetching full-size avatar from endpoint: {url}"))
  } else if (image_type %in% c("thumb", "thumbnail")) {
    # Thumbnail Avatar
    url <- glue::glue("https://sleepercdn.com/avatars/thumbs/{avatar_id}")
    # Log the URL being fetched
    log_message(glue::glue("Fetching thumbnail avatar from endpoint: {url}"))
  } else {
    # Invalid image type
    log_message(glue::glue("Avatar image_type == '{image_type}' not found!"))
    log_message("Try full, full-size, full size or NULL for a full-size avatar.")
    log_message("Try thumb or thumbnail for a thumbnail avatar.")
    stop("Stopping. Avatar image type not found.")
  }

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract raw content (avatars are binary image data, not JSON)
  log_message("Extracting binary content from avatar request.")
  binary_content <- httr::content(response, "raw")

  if (!is.null(save_path)) {
    # Save the binary content to a file
    writeBin(binary_content, glue::glue("{save_path}/avatar_{image_type}_{avatar_id}.png"))
    log_message(glue::glue("Avatar saved to {save_path}/avatar_{image_type}_{avatar_id}.png"))
  }
  else {
    log_message("No save path provided. Avatar not saved to file.")
  }

  # Return the raw binary content
  return(binary_content)
}

#' Example usage:
# avatar <- get_avatars("4ce10fd3c0c13eee3371d49d1d35aa62",
#                       image_type = "full",
#                       save_path = "outputs/")
