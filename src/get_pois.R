#' Retrieve and Filter NPS Points of Interest
#'
#' Fetches National Park Service (NPS) Points of Interest (POIs) from the 
#' ArcGIS REST API and filters them based on a provided Area of Interest (AOI).
#'
#' @param sf_aoi An \code{sf} object representing the area of interest (e.g., a park boundary).
#'
#' @return An \code{sf} object containing POIs that intersect with the \code{sf_aoi},
#'   projected to match the AOI's CRS, with added WGS84 coordinate columns.
#'
#' @details 
#' The function queries the NPS Public POI Geographic MapServer. It performs 
#' a server-side request for all features in GeoJSON format, then filters 
#' spatially on the client side using \code{st_filter}.
#'

get_pois <- function(sf_aoi) {
  
  # Construct the REST API query URL
  pois_url <- paste0(
    "https://mapservices.nps.gov/arcgis/rest/services/NationalDatasets/",
    "NPS_Public_POIs_Geographic/MapServer/0/query",
    "?where=1%3D1",
    "&outFields=*",
    "&outSR=4326",
    "&f=geojson"
  )
  
  # Read, transform, and filter data
  pois <- st_read(pois_url, quiet = TRUE) %>% 
    st_transform(crs = st_crs(sf_aoi)) %>%
    st_filter(sf_aoi) %>%
    add_wgs_coords()
  
  return(pois)
}