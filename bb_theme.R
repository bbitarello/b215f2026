suppressPackageStartupMessages({
  #library(tidyverse)
  library(cols4all)
})
# Fonts ----
#extrafont::font_import(prompt = F)
#extrafont::loadfonts(quiet = T)
hrbrthemes::import_roboto_condensed()
sysfonts::font_add_google("Roboto")
sysfonts::font_add_google("Roboto Condensed")
gdtools::register_gfont(family = "Roboto Condensed")
systemfonts::get_from_google_fonts(family = "Roboto Condensed")
fonts <- systemfonts::system_fonts()
systemfonts::fonts_as_import(family = "Roboto Condensed")
systemfonts::match_fonts("Roboto Condensed")
showtext::showtext_opts(dpi = 150)
myokabe_ito<-c("#000000", "#E69F00", "#56B4E9", "#6C8645", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
"#999999")
names(myokabe_ito)<-c("oki_black", "oki_orange", "oki_blue", "oki_green", "oki_yellow", "oki_darkblue", "oki_darkorange", "oki_pink", "oki_gray")
asteroidcity1<-wesanderson::wes_palette("AsteroidCity1")

banff<-function (n = NULL) {

 banFF <- c("#006475", "#00A1B7", "#55CFD8", "#586028", "#898928", "#616571", 
            "#9DA7BF")
  if (is.null(n)) 
    banFF
  else banFF[seq_len(n)]
}
asteroidcity1<-function (n = NULL) {
  
  asteroidCity <- c("#0A9F9D", "#CEB175", "#E54E21", "#6C8645", "#C18748")
  if (is.null(n)) 
    asteroidCity
  else asteroidCity[seq_len(n)]
}

# scale_color_qual_bb<-function (n = NULL) {
#   scale_color_manual(values = myokabe_ito)
# }
# 
# scale_color_qual2_bb<-function(n = NULL){
#   require(cols4all)
#   scale_color_manual(values = cols4all::c4a("kings_canyon"))
# }
# 
# scale_fill_qual_bb<-function (n = NULL) {
#   scale_fill_manual(values = myokabe_ito)
# }
# 
# scale_fill_qual2_bb<-function (n = NULL) {
#   scale_fill_manual(values = myokabe_ito)
# }

kings_canyon_palette<-cols4all::c4a("kings_canyon")
kings_canyon_pal<-function(){scales::manual_pal(kings_canyon_palette)}

scale_color_kings_canyon<-function(...) {
  discrete_scale(aesthetics="color",palette=kings_canyon_pal(),...)
}

scale_fill_kings_canyon<-function(...) {
  discrete_scale(aesthetics="fill",palette=kings_canyon_pal(),...)
}
bb_theme <-  function() {
  theme(
    text = element_text(face = "plain", family = "Roboto Condensed"),
    axis.text.x = element_text(
      size = 14,
      family = "Roboto Condensed",
      face = "plain"
    ),
    axis.text.y = element_text(
      size = 14,
      family = "Roboto Condensed",
      face = "plain"
    ),
    axis.title.x = element_text(
      size = 16,
      hjust = 1,
      family = "Roboto Condensed",
      face = "plain"
    ),
    axis.title.y = element_text(
      size = 16,
      hjust = 1,
      family = "Roboto Condensed",
      face = "plain",
      angle = 90
    ),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 12,
      family = "Roboto Condensed",
      face = "plain"
    ),
    plot.title = element_text(family = "Roboto Condensed", size = 20),
    plot.subtitle = element_text(family = "Roboto Condensed", size = 16),
    strip.text =  element_text(size = 14),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.1),
    panel.grid.major.x = element_line(color = "gray", linewidth = 0.1)
    #strip.background = element_blank()
  )
}



#-----rstudio-font-settings
if(FALSE){
  showtext::showtext_auto()
  showtext::showtext_opts(dpi=300)
  
  if (interactive()) {
    options(device = "RStudioGD")
  } else {
    options(
      device = function(...)
        ragg::agg_png(...)
    )
  }
}


plot_palette <- function(palette) {
  g <- ggplot2::ggplot(
    data = data.frame(
      x = seq_len(length(palette)),
      y = "1",
      fill = palette
    ),
    mapping = ggplot2::aes(
      x = x, y = y, fill = fill
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
  return(g)
}
set_theme(bb_theme())
old <- update_theme(palette.colour.discrete = scales::pal_viridis())