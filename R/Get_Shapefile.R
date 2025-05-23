#' Download the shapefiles of Italian NUTS-3 and LAU administrative units
#'
#' @description Downloads either the boundaries or the centroids of the relevant administrative units,
#' either provinces or municipalities, from the ISTAT website. Geometries are expressed in meters.
#'
#'
#'
#' @param Year Numeric. Reference year for the administrative units.
#' @param level Character. Either  \code{"LAU"/"Municipality"}, \code{"NUTS-3"/"Province"},  \code{"NUTS-2"/"Region"}, . \code{"LAU"} by default
#' @param lightShp Logical. If \code{TRUE}, the function downloads a generalised, i.e.less detailed, and lighter version of the shapefiles.
#' \code{TRUE} by default.
#' @param centroids Logical. Whether to switch from polygon geometry to point geometry. In the latter case, the point is located at the centroid of the relevant area. \code{FALSE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#'
#' @return A spatial data frame of class \code{data.frame} and \code{sf}.
#'
#' @source <https://www.istat.it/it/archivio/222527>
#'
#' @examples
#'
#' \donttest{
#'   library(magrittr)
#'
#'
#'   Prov23_shp <- Get_Shapefile(2023, lightShp = TRUE, level = "NUTS-3", autoAbort = TRUE)
#'
#'  }
#'
#'
#' @export


Get_Shapefile <- function(Year, level = "LAU", lightShp = TRUE,
                          autoAbort = FALSE, centroids = FALSE){

  while(Year < 2001 & Year != 1991){
    message(paste("Year", Year, "not available. Please choose another year between 2001 and the current year"))
    Year <- readline(prompt= "   ")
  }

  if(!Check_connection(autoAbort)) return(NULL)

  home.ISTAT.Shp <- "https://www.istat.it/it/archivio/222527"
  homepage <- NULL
  attempt <- 0
  while(is.null(homepage) && attempt <= 10){
    homepage <- tryCatch({
      xml2::read_html(home.ISTAT.Shp)
    }, error = function(e){
      message("Cannot read the html; ", 10 - attempt,
              " attempts left. If the problem persists, please contact the maintainer.\n")
      return(NULL)
    })
    attempt <- attempt + 1
  }
  if(is.null(homepage)) return(NULL)

  links <- homepage %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% unique()
  links <- grep(paste0("confini_amministrativi.*", Year), links, value = TRUE)
  links.NR <- grep("ED50", links, value = TRUE)
  links <- links[which(!links %in% links.NR)]

  if(lightShp == FALSE){
    link <- grep(paste0(Year, ".zip"),links, value=TRUE)
  } else {
    link <- grep(paste0(Year, "_g.zip"),links, value=TRUE)
  }

  if(length(link) == 0L){
    message("Shapefiles not found for year: ", Year)
    return(NULL)
  }

  base.url <- dirname(home.ISTAT.Shp)
  file.url <- xml2::url_absolute(link, base.url)
  temp1 <- tempfile()
  temp2 <- tempfile()
  timeout.default <- options("timeout")$timeout
  on.exit(options(timeout = timeout.default))
  options(timeout = max(timeout.default, 120))
  x <- tryCatch({
    utils::download.file(url = file.url, destfile = temp1)},
    error = function(e) {
      NULL
    })

  if(is.null(x)){
    unlink(temp1, recursive = TRUE, force = TRUE)
    unlink(temp2, recursive = TRUE, force = TRUE)
    return(NULL)
  }
  utils::unzip(zipfile = temp1, exdir = temp2)
  files.int <- list.files(list.files(list.files(temp2, full.names = TRUE), full.names = TRUE), full.names = TRUE)

  pattern <- dplyr::case_when(
    toupper(level) %in% c("LAU", "NUTS-4", "MUNICIPALITY", "MUN") ~ "Com",
    toupper(level) %in% c("NUTS-3", "PROVINCE", "PROV") ~ "Prov",
    toupper(level) %in% c("NUTS-2", "REGION", "REG") ~ "Reg")


  filename.int <- grep(pattern, files.int, value = TRUE)
  filename.int <- filename.int[which(substr(filename.int, nchar(filename.int)-2, nchar(filename.int)) == "shp")]

  res <- sf::read_sf( filename.int)
  if(centroids) {
    sf::st_agr(res) <- "constant"
    res <- sf::st_point_on_surface(res)
  }

  unlink(temp1, recursive = TRUE, force = TRUE)
  unlink(temp2, recursive = TRUE, force = TRUE)

  return(res)
}
