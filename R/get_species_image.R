# Downloading images from RLS website
# Freddie Heather
# March 2023

library(rvest)
library(httr)
library(magick)
library(stringr)

# RLS IMAGE DOWNLOAD FUNCTION --------------------------------------------------
# Arguments: 
# species_name = Single character string of species name
# image_number = single integer value or a vector of integers with the image number
get_species_image <- function(species_name, image_number = 1){
  
  url <- 
    "https://reeflifesurvey.com//species/" |> 
    paste0(species_name |> 
             tolower() |> 
             str_replace_all(" ", "-"))
  
  if(!httr::http_error(url)){
    
    url_vector <-
      url |> 
      read_html() |> 
      html_elements("img") |> 
      html_attr("src")
    
    n_spp_images <- sum(str_detect(url_vector, "species_"))

    n_images <- image_number[image_number<=n_spp_images]

    if(n_spp_images){
      url_vector[url_vector |> str_detect("species_")][n_images] |> 
        image_read()
    } else {
      NULL
    }
    
  } else {
    NULL
  }
  
}

# FUNCTION EXAMPLES ------------------------------------------------------------


if(FALSE){
  
  # get a single image for the species
  get_species_image("Pseudocheilinus hexataenia")
  get_species_image("Pseudocheilinus hexataenia", 1)
  
  # ...or a different single image
  get_species_image("Pseudocheilinus hexataenia", 2)
  
  # or get a series of images for the species (will ignore when not enough images are available)
  get_species_image("Pseudocheilinus hexataenia", 1:30)
  
  # NULL returns when no image available
  get_species_image("Nonexistent species", 1)
  
  # including the image in a ggplot
  my_img <- get_species_image("Pseudocheilinus hexataenia", 1)
  
  ggplot() +
    aes(x = 1:10,
        y = 1:10) +
    geom_point() +
    {if(!is.null(my_img))  annotation_raster(my_img, 
                                             xmin = 1, 
                                             xmax = 3,
                                             ymin = 8, 
                                             ymax = 10) }
  
}
