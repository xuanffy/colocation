library(ggplot2)
library(maps)
library(mapproj)
library(tidyverse)
library(ggmap)

# extract CA long and lati
pb_co <- as.data.frame(st_coordinates(ca_pb_sf))
sfs_co <- as.data.frame(st_coordinates(ca_sfs_sf))
ap_co <- as.data.frame(st_coordinates(ca_ap_sf))
tri_co <- as.data.frame(st_coordinates(ca_tri_sf))
mil_co <- as.data.frame(st_coordinates(ca_mil_sf))
plotdf <- bind_rows(list(prison = pb_co, superfund = sfs_co, airport = ap_co, TRI = tri_co, military = mil_co), .id = "type")

# google API key
register_google(key = "")

# prison and toxic sites position in California
mapcali <- get_googlemap("california", zoom = 6, color = "bw", maptype = "roadmap", size = c(640, 640))

ggmap(mapcali) + geom_point(data = plotdf[plotdf$type %in% c("prison", "superfund"), ], aes(X, Y, col = type), size = 0.5, alpha = 0.5) +
  labs(x = "", y = "", col = "Type") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values = c("prison" = "#F35E5A", "superfund" = "#4FB1FF"))

ggmap(mapcali) + geom_point(data = plotdf[plotdf$type %in% c("prison", "airport"), ], aes(X, Y, col = type), size = 0.5, alpha = 0.5) +
  labs(x = "", y = "", col = "Type") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values = c("prison" = "#F35E5A", "airport" = "#4FB1FF"))

ggmap(mapcali) + geom_point(data = plotdf[plotdf$type %in% c("prison", "TRI"), ], aes(X, Y, col = type), size = 0.5, alpha = 0.5) +
  labs(x = "", y = "", col = "Type") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values = c("prison" = "#F35E5A", "TRI" = "#4FB1FF"))

ggmap(mapcali) + geom_point(data = plotdf[plotdf$type %in% c("prison", "military"), ], aes(X, Y, col = type), size = 0.5, alpha = 0.5) +
  labs(x = "", y = "", col = "Type") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values = c("prison" = "#F35E5A", "military" = "#4FB1FF"))


# position in bay area
mapbayarea <- get_googlemap("pleasant hill", zoom = 9, color = "bw", maptype = "roadmap", size = c(640, 640))

ggmap(mapbayarea) + geom_point(data = plotdf[plotdf$type %in% c("prison", "superfund"), ], aes(X, Y, col = type), size = 1.2, alpha = 0.8) +
  labs(x = "", y = "", col = "Type") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values = c("prison" = "#F35E5A", "superfund" = "#4FB1FF"))

ggmap(mapbayarea) + geom_point(data = plotdf[plotdf$type %in% c("prison", "airport"), ], aes(X, Y, col = type), size = 1.2, alpha = 0.8) +
  labs(x = "", y = "", col = "Type") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_color_manual(values = c("prison" = "#F35E5A", "airport" = "#4FB1FF"))


# heat map in California
ggmap(mapcali) + stat_density2d(alpha = 0.25, aes(x = X, y = Y, fill = ..level..), size = 0.1, bins = 15, data = plotdf[plotdf$type %in% c("prison"), ], geom = "polygon") +
  geom_point(data = plotdf[plotdf$type %in% c("prison"), ], aes(X, Y), color = "#F76372", size = 0.5, alpha = 0.5) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "null")

ggmap(mapcali) + stat_density2d(alpha = 0.25, aes(x = X, y = Y, fill = ..level..), size = 0.01, bins = 15, data = plotdf[plotdf$type %in% c("military"), ], geom = "polygon") +
  geom_point(data = plotdf[plotdf$type %in% c("military"), ], aes(X, Y), color = "#F76372", size = 0.5, alpha = 0.5) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "null")

ggmap(mapcali) + stat_density2d(alpha = 0.25, aes(x = X, y = Y, fill = ..level..), size = 0.01, bins = 15, data = plotdf[plotdf$type %in% c("TRI"), ], geom = "polygon") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "null") +
  labs(x = "", y = "")