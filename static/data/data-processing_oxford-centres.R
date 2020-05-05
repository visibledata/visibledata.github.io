library("here")
library("jsonlite")
library("tidyverse")
library("sf")
library("rvest")
library("ggmap")


## === Test Vault ====

## https://www.mishabalyasin.com/2017/11/05/overview-secret-spelling/
my_vault <- here("static", "data", "secret_vault.vault")
charliejhadley_private_key <- file.path("~/.ssh", "blog_vault")

get_secret("secret_two", key = charliejhadley_private_key, vault = my_vault)

## ==== OxPoints ===== 

raw_json_oxpoints_colleges <- read_json("https://maps.ox.ac.uk/api/places/search?type=%2Funiversity%2Fcollege&inoxford=true&-type_exact=%5C%2Funiversity%5C%2Fsub-library&-type_exact=%5C%2Funiversity%5C%2Froom&count=50&facet=type_exact")

oxpoints_colleges <- raw_json_oxpoints_colleges$`_embedded`$pois

get_college_data <- function(oxpoints_data,
                             college_index){
  
  college_properties <- names(oxpoints_data[[college_index]])
  
  extracted_properties <-
    college_properties[college_properties %in% c(
      "id",
      "lat",
      "lon",
      "name",
      "name_sort",
      "shape",
      "social_facebook",
      "website")]
  
  oxpoints_data[[college_index]] %>%
    .[extracted_properties] %>%
    as_tibble() %>%
    mutate_if(is.list, funs(as.character(.))) %>%
    mutate_all(funs(parse_guess(.)))
  
}


college_oxpoints_data <- oxpoints_colleges %>%
  get_college_data(1)

tibble(x = 2:38) %>%
  pwalk(function(x){college_oxpoints_data <<- college_oxpoints_data %>%
    bind_rows(get_college_data(oxpoints_colleges, x))})

college_oxpoints_data %>%
  select(-lat, -lon, -id, -name_sort) %>%
  write_csv(here("static", "data", "oxford-centres_ill-formed-gis-data.csv"))



## ==== Wikipedia Table ====

colleges_of_oxford <- read_html("https://en.wikipedia.org/wiki/Colleges_of_the_University_of_Oxford")



## THIS DOES WHAT I WANTS IT TO DO
wikitable_rows <- colleges_of_oxford %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_nodes("tbody") %>%
  html_nodes("tr") %>%
  html_text()
  
  
college_wikitable <- wikitable_rows %>%
  .[2:length(.)] %>%
  str_trim() %>%
  str_replace_all("\n{1,}", ";") %>%
  read_delim(delim = ";")
colnames(college_wikitable) <- c("name", "foundation.year", "sister.college", "total.assets", "financial.endowment", "undergraduates", "post.graduates", "visiting.students", "male.students", "female.students", "total.students", "assets.per.student")

college_wikitable <- college_wikitable %>%
  filter(!name == "Total")

college_wikitable <- college_wikitable %>%
  select(name, sister.college, everything())

college_wikitable <- college_wikitable %>%
  mutate_at(vars(foundation.year:undergraduates), funs(
    if (class(.) == "numeric") {
      .
    } else {
      parse_number(.)
    }
  ))

college_data <- college_wikitable %>%
  left_join(college_oxpoints_data)

college_data <- college_data %>%
  select(-name_sort, -lat, -lon)


## ==== Export =====


college_features <- college_data %>%
  rename(geometry = shape) %>%
  select(geometry) %>%
  .[[1]]

college_geometries <- st_as_sfc(college_features)

college_nongeometric_data <- college_data %>%
  select(-shape)

st_geometry(college_nongeometric_data) <- college_geometries
college_shapes <- college_nongeometric_data


college_features <- college_data %>%
  select(shape) %>%
  .[[1]]
college_geometries <- st_as_sfc(college_features) %>%
  st_zm() %>%
  st_set_crs(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  sf:::largest_ring()

st_geometry(college_nongeometric_data) <- college_geometries
college_shapes <- college_nongeometric_data

college_shapes %>%
  write_sf(here("static", "data", "shapefiles_oxford_colleges.json"),
           driver = "GeoJSON")


## ===== 

my_vault <- here("static", "data", "secret-vault.vault")
charliejhadley_private_key <- file.path("~/.ssh", "blog_vault")
ggmaps_rstats_key <- get_secret("ggmaps_rstats_key", key = charliejhadley_private_key, vault = my_vault)
register_google(ggmaps_rstats_key)



