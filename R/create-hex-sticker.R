create_gasr_hex_sticker <- function() {
  library(hexSticker)
  library(tidyverse)
  library(ardea)

  # Some alternate colorings
  #gas_img <- file.path("man", "figures", "gas-logo.png")
  #gas_img <- file.path("man", "figures", "gas-logo-magenta.png")
  gas_img <- file.path("man", "figures", "gas-logo-blue.png")

  hex_sticker <- sticker(
    gas_img, package = "gasr", s_width = 0.8, s_x = 1.2,
    p_size = 80, p_family = "sans", p_color = "white", p_y = 1.5,
    h_fill = ardea_colors("purple"), h_color = ardea_colors("blue"),
    filename = "gasr-sticker.png", dpi = 800
  )
  plot(hex_sticker)
}
