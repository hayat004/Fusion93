library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(maptiles)
library(tidyterra)

communes <- st_read("C:/Users/Merza/Downloads/ADMIN-EXPRESS-COG-CARTO-PE_3-1__SHP_LAMB93_FXX_2024-04-15/ADMIN-EXPRESS-COG-CARTO-PE_3-1__SHP_LAMB93_FXX_2024-04-15/ADECOGCPE_3-2_SHP_LAMB93_FR/COMMUNE.shp")

communes_93_fond <- communes %>% filter(substr(INSEE_COM, 1, 2) == "93")
communes_93 <- communes %>% filter(INSEE_COM %in% c("93064", "93066"))

communes_93_wgs      <- st_transform(communes_93, crs = 4326)
communes_93_fond_wgs <- st_transform(communes_93_fond, crs = 4326)

# Téléchargement fond raster OpenStreetMap
fond <- get_tiles(communes_93_fond_wgs, provider = "OpenStreetMap", zoom = 12, crop = TRUE)

# Carte raster
carte_raster <- ggplot() +
  geom_spatraster_rgb(data = fond) +
  geom_sf(data = communes_93_fond_wgs, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = communes_93_wgs, fill = NA, color = "red", linewidth = 1.5) +
  geom_sf_label(data = communes_93_wgs, aes(label = NOM), size = 3, fontface = "bold") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()) +
  labs(title = "Traitement raster — Fond OpenStreetMap",
       subtitle = "Localisation de Rosny-sous-Bois et Saint-Denis, Seine-Saint-Denis",
       caption = "Source : OpenStreetMap 2026 | IGN Admin Express 2024") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank())

ggsave("C:/Users/Merza/Downloads/carte_raster.pdf", carte_raster, width = 20, height = 15, units = "cm")

print("Carte raster exportée !")