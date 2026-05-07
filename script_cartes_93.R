library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(maptiles)
library(tidyterra)

communes <- st_read("C:/Users/Merza/Downloads/ADMIN-EXPRESS-COG-CARTO-PE_3-1__SHP_LAMB93_FXX_2024-04-15/ADMIN-EXPRESS-COG-CARTO-PE_3-1__SHP_LAMB93_FXX_2024-04-15/ADECOGCPE_3-2_SHP_LAMB93_FR/COMMUNE.shp")

communes_93_fond <- communes %>% filter(substr(INSEE_COM, 1, 2) == "93")
communes_93 <- communes %>% filter(INSEE_COM %in% c("93064", "93066"))

communes_93$revenu_median <- ifelse(communes_93$NOM == "Rosny-sous-Bois", 22540, 14200)
communes_93$taux_pauvrete <- ifelse(communes_93$NOM == "Rosny-sous-Bois", 16.2, 38.5)
communes_93$part_hlm      <- ifelse(communes_93$NOM == "Rosny-sous-Bois", 22, 48)

communes_93_wgs      <- st_transform(communes_93, crs = 4326)
communes_93_fond_wgs <- st_transform(communes_93_fond, crs = 4326)

fond <- get_tiles(communes_93_fond_wgs, provider = "OpenStreetMap", zoom = 12, crop = TRUE)

# ---- CARTE 1 : Revenu médian ----
carte1 <- ggplot() +
  geom_spatraster_rgb(data = fond) +
  geom_sf(data = communes_93_fond_wgs, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = communes_93_wgs, aes(fill = NOM), color = "white", linewidth = 1, alpha = 0.8) +
  scale_fill_manual(values = c("Rosny-sous-Bois" = "#1a9850", "Saint-Denis" = "#d73027"),
                    name = "",
                    labels = c("Rosny-sous-Bois\n22 540 €/an", "Saint-Denis\n14 200 €/an")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()) +
  labs(title = "Revenu médian disponible par habitant",
       subtitle = "Rosny-sous-Bois vs Saint-Denis, Seine-Saint-Denis",
       caption = "Source : INSEE Filosofi 2021 | Fond : OpenStreetMap") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank())

# ---- CARTE 2 : Taux de pauvreté ----
carte2 <- ggplot() +
  geom_spatraster_rgb(data = fond) +
  geom_sf(data = communes_93_fond_wgs, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = communes_93_wgs, aes(fill = NOM), color = "white", linewidth = 1, alpha = 0.8) +
  scale_fill_manual(values = c("Rosny-sous-Bois" = "#ffffb2", "Saint-Denis" = "#800026"),
                    name = "",
                    labels = c("Rosny-sous-Bois\n16,2%", "Saint-Denis\n38,5%")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()) +
  labs(title = "Taux de pauvreté",
       subtitle = "Rosny-sous-Bois vs Saint-Denis, Seine-Saint-Denis",
       caption = "Source : INSEE Filosofi 2021 | Fond : OpenStreetMap") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank())

# ---- CARTE 3 : Part de logements HLM ----
carte3 <- ggplot() +
  geom_spatraster_rgb(data = fond) +
  geom_sf(data = communes_93_fond_wgs, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = communes_93_wgs, aes(fill = NOM), color = "white", linewidth = 1, alpha = 0.8) +
  scale_fill_manual(values = c("Rosny-sous-Bois" = "#deebf7", "Saint-Denis" = "#08306b"),
                    name = "",
                    labels = c("Rosny-sous-Bois\n22%", "Saint-Denis\n48%")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()) +
  labs(title = "Part des logements sociaux (HLM)",
       subtitle = "Rosny-sous-Bois vs Saint-Denis, Seine-Saint-Denis",
       caption = "Source : INSEE RP 2020 | Fond : OpenStreetMap") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank())

ggsave("C:/Users/Merza/Downloads/carte1_revenus.pdf",  carte1, width = 20, height = 15, units = "cm")
ggsave("C:/Users/Merza/Downloads/carte2_pauvrete.pdf", carte2, width = 20, height = 15, units = "cm")
ggsave("C:/Users/Merza/Downloads/carte3_hlm.pdf",      carte3, width = 20, height = 15, units = "cm")

print("Cartes exportées avec succès !")