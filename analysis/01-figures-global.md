Figures
================
Ate Poorthuis
02/09/2019

``` r
hex_data <- readRDS(here("analysis/data/derived_data/global_hex_join_odds_ratios.rds"))
hex_sf <- readRDS(here("analysis/data/derived_data/global_hex_grid.rds")) %>% 
  left_join(., hex_data, by = "hex")

hex_union <- hex_sf %>% 
  st_buffer(1) %>% 
  summarise(count = n())
```

``` r
basecolor <- "#FFF5EB"
colors <- c("#FFF5EB", "#FDD1A5", "#FD9243", "#DE4F05", "#7F2704")
breaks <- c(0, 100, 1000, 10000, 50000, 1000000)
labels <- c(" 0", "100", "1,000", "10,000", "50,000+")
font_col <- "#22211d"

## theme
map_theme <- theme(text = element_text(family = "Helvetica Neue", color = font_col),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     axis.text.x=element_blank(),
                     axis.title.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     plot.title=element_text(hjust = 0, family = "Helvetica Neue", color = font_col, angle = 0, size = 12),
                     plot.subtitle=element_text(family = "Helvetica Neue Thin Italic", color = font_col, angle = 0, size = 10),
                     plot.caption=element_text(family = "Helvetica Neue Thin", color = font_col, angle = 0, size = 7),
                     legend.background = element_rect(fill="transparent"),
                     legend.position = c(0.675, 0.08),
                     legend.box = "horizontal")

## legend
legend <- guide_legend(keyheight = unit(4, units = "mm"),
                  keywidth = unit(70 / length(labels), units = "mm"),
                  direction = "horizontal", title.position = "top",
                  label.position="top",
                  title.theme = element_text(family = "Helvetica Neue Medium", color = font_col, angle = 0, size = 12),
                  title = "Count (absolute intensity)",
                  title.hjust = 0.0,
                  label.hjust = -0.2,
                  label.theme = element_text(family = "Helvetica Neue Thin", color = font_col, angle = 0, size = 10),
                  nrow = 1,
                  byrow = T)

## function to annotate legend
annotate_legend <- function(p, label1, label2) {
  g <- ggplotGrob(p)
  leg <- g$grobs[[which(g$layout$name == "guide-box")]]
  
  # Construct the label grob 
  text1 <- textGrob(x = 0.975, label1, gp = gpar(fontsize=9, fontfamily = "Helvetica Neue Thin Italic"), hjust = 1)
  text2 <- textGrob(x = 0.025, label2, gp = gpar(fontsize=9, fontfamily = "Helvetica Neue Thin Italic"), hjust = 0)
  height <- unit(1, "grobheight", text1) + unit(5, "points")
  labelGrob = gTree("labelGrob", children = gList(text1, text2))
  
  # Add the label grob to a new row added to the legend
  pos <- subset(leg$layout, grepl("guides", name), t:r)
  leg <- gtable_add_rows(leg, height, pos = pos$t+1)
  leg <- gtable_add_grob(leg, labelGrob, t = pos$t+2, l = pos$l)
  
  # Return the modified legend to the origial plot
  g$grobs[[which(g$layout$name == "guide-box")]] <- leg
  return(g)
}

## color scale
scale_fill <- scale_fill_manual(values=colors, guide = legend, labels = labels)
```

Figure 2
--------

``` r
fig2 <- hex_sf %>% 
  filter(total > 10) %>% 
  ggplot() +
  ylim(c(-6000148, 8248865)) + xlim(c(-10097010, 14597010)) +
  geom_sf(data = hex_union, fill = basecolor, color = NA) +
  geom_sf(aes(fill = cut(total, breaks=breaks, include.lowest = T)), color = NA) +
  scale_fill +
  geom_sf(data = hex_union, fill = NA, color = "gray75", size = 0.1) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Total Attention to Fashion", subtitle = "absolute counts for all keywords")

fig2 <- annotate_legend(fig2, label1 = "more attention", label2 = "less attention")
plot(fig2)
```

![](01-figures-global_files/figure-markdown_github/fig2-1.png)

``` r
#ggsave(plot = fig2, filename = "Figure2.pdf", width = 9, height = 5.5, dpi = 600, device = cairo_pdf)
```

Figure 3
--------

``` r
breaks <- c(0, 0.5, 0.8, 1/0.8, 1/0.5, 100000000)
labels <- c("0", "0.5", "0.8", "1.25", "2+")
legend <- guide_legend(keyheight = unit(4, units = "mm"),
                  keywidth = unit(70 / length(labels), units = "mm"),
                  direction = "horizontal", title.position = "top",
                  label.position="top",
                  title.theme = element_text(family = "Helvetica Neue Medium", color = "#22211d", angle = 0, size = 12),
                  title = "Odds Ratio (relative intensity)",
                  title.hjust = 0.0,
                  label.hjust = -0.2,
                  label.theme = element_text(family = "Helvetica Neue Thin", color = "#22211d", angle = 0, size = 10),
                  nrow = 1,
                  byrow = T)
scale_fill <- scale_fill_manual(values=colors, guide = legend, labels = labels)

fig3 <- hex_sf %>% 
  filter(total > 10) %>% 
  ggplot() +
  ylim(c(-6000148, 8248865)) + xlim(c(-10097010, 14597010)) +
  geom_sf(data = hex_union, fill = basecolor, color = NA) +
  geom_sf(aes(fill = cut(total_ORc, breaks=breaks, include.lowest = T)), color = NA) +
  scale_fill +
  geom_sf(data = hex_union, fill = NA, color = "gray75", size = 0.1) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Total Attention to Fashion", subtitle = "odds ratio for all keywords")


fig3 <- annotate_legend(fig3, label1 = "more attention", label2 = "less attention")
plot(fig3)
```

![](01-figures-global_files/figure-markdown_github/fig3-1.png)

``` r
#ggsave(plot = fig3, filename = "Figure3.png", width = 9, height = 5.5, dpi = 600)
```