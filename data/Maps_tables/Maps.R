
# Load library
library(readxl)
library(dplyr)
library(ggplot2)

#-------------------------------------------------------------------------------
# Get all Excel file names for type 2
files <- list.files(path = "data/Type2/", pattern = "\\.xlsx$", full.names = TRUE)

# Read sheet 2 from each file and combine
all_data <- lapply(files, function(file) {
  # Read sheet 2
  df <- read_excel(file, sheet = 2)
  
  # Select longitude and latitude
  df <- df %>%
    select(longitude, latitude)
  
  # Add dataset column using the file name
  df$type_2 <- basename(file)
  
  return(df)
}) %>%
  bind_rows()  # Combine all data frames into one

# View combined dataset
head(all_data)

all_data$type <- "B. trawling (n=14)"
all_data$type <- ifelse(all_data$type_2 %in% c("NS_Oostdyck_sandextr_and_btrawling.xlsx",
                                               "NS_Thornton_sandextr_and_btrawling.xlsx",
                                               "NS_Hinderbanken_sandextr_and_btrawling.xlsx"),
                        "B. trawling + sand extr. (n=3)",
                        all_data$type)

all_data$type <- ifelse(all_data$type_2 %in% c("BS_southernbaltic_oxygendepletion.xlsx"),
                        "Oygen depl. (n=1)",
                        all_data$type)


# make a map
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

type2 <- ggplot()+geom_point(data=all_data,aes(x=longitude,y=latitude,col=type),cex=1) +
  geom_sf(data = ctrys, fill="lightgrey",colour= NA) + 
  coord_sf(xlim = c(-15,33),ylim=c(34,67)) + theme_classic()

ggsave("data/Maps_tables/Map_type_2.png", plot = type2, width = 8, height = 6, dpi = 300)

#-------------------------------------------------------------------------------
# Get all Excel file names for type 3
files <- list.files(path = "data/Type3/", pattern = "\\.xlsx$", full.names = TRUE)

# Read sheet 2 from each file and combine
all_data <- lapply(files, function(file) {
  # Read sheet 2
  df <- read_excel(file, sheet = 2)
  
  # Select longitude and latitude
  df <- df %>%
    select(longitude, latitude) %>%            # select relevant columns
    mutate(
      longitude = as.numeric(longitude),       # ensure numeric type
      latitude  = as.numeric(latitude)
    )
  
  # Add dataset column using the file name
  df$type_3 <- basename(file)
  
  return(df)
}) %>%
  bind_rows()  # Combine all data frames into one

# View combined dataset
head(all_data)

# make a map
ctrys <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

type3 <- ggplot()+geom_point(data=all_data,aes(x=longitude,y=latitude,col=type_3)) +
  geom_sf(data = ctrys, fill="lightgrey",colour= NA) + 
  coord_sf(xlim = c(-15,33),ylim=c(34,67)) + theme_classic()

ggsave("data/Maps_tables/Map_type_3.png", plot = type3, width = 8, height = 6, dpi = 300)


