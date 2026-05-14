# ================================================================
# 05_sacilim_grafigi.R
# Sikayet Turune Gore Kayip-Kacak Orani
# Kaynak: EPDK (2015-2024)
# Encoding: UTF-8
#
# LEJANT ESLESTIRME:
#   Faturalama -> shape = 21 (yuvarlak), fill = "#c1121f" (kirmizi)
#   Sayac      -> shape = 22 (kare),     fill = "#003f88" (mavi)
# ================================================================

library(ggplot2)
library(readxl)
library(dplyr)

# -- Sabitler -----------------------------------------------------
DOSYA <- "05_sacilim_grafigi_verisi.xlsx"
CIKTI <- "../Grafikler/05_sacilim_grafigi.png"
W <- 14; H <- 8; RES <- 300

# Renk ve sekil -- veri kategorileriyle %100 senkronize
KATEGORILER <- c("Faturalama", "Sayac")

RENKLER <- c(
  "Faturalama" = "#c1121f",
  "Sayac"      = "#003f88"
)

SEKILLER <- c(
  "Faturalama" = 21,
  "Sayac"      = 22
)

ETIKETLER <- c(
  "Faturalama" = "Faturalama",
  "Sayac"      = "Saya\u00e7"
)

# -- Tema ---------------------------------------------------------
tema <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "#F8F9FA", colour = NA),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "#E5E5E5", linewidth = 0.35),
      axis.ticks         = element_line(colour = "#BBBBBB", linewidth = 0.3),
      axis.title         = element_text(size = base - 2, colour = "#444444"),
      axis.text          = element_text(size = base - 3, colour = "#555555"),
      plot.title         = element_text(size = base + 4, face = "bold",
                                        colour = "#111111", hjust = 0.5,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = base - 2, colour = "#777777",
                                        hjust = 0.5, margin = margin(b = 14)),
      plot.caption       = element_text(size = base - 3, colour = "#999999",
                                        hjust = 0.5, face = "italic",
                                        margin = margin(t = 10)),
      legend.position      = "top",
      legend.justification = "center",
      legend.title         = element_blank(),
      legend.text          = element_text(size = base),
      legend.key.size      = unit(0.9, "cm"),
      legend.spacing.x     = unit(1.0, "cm"),
      plot.margin          = margin(16, 20, 12, 16)
    )
}

# -- Veri ---------------------------------------------------------
df <- read_excel(DOSYA) |>
  mutate(
    Yil      = as.integer(Yil),
    Kategori = as.character(Kategori),
    Sikayet  = as.numeric(Sikayet_orani),
    Kacak    = as.numeric(Kacak_usulsuz_kullanim_orani)
  ) |>
  filter(!is.na(Yil), !is.na(Sikayet), !is.na(Kacak)) |>
  mutate(
    Kategori = ifelse(
      grepl("Say", Kategori, ignore.case = TRUE),
      "Sayac",
      "Faturalama"
    ),
    Kategori = factor(Kategori, levels = KATEGORILER)
  )

# -- Grafik -------------------------------------------------------
# Kritik: colour, fill ve shape scale'leri ayni KATEGORILER
# vektorunu kullanarak tamamen senkronize edilmistir.
p <- ggplot(df, aes(
    x      = Sikayet,
    y      = Kacak,
    colour = Kategori,
    fill   = Kategori,
    shape  = Kategori
  )) +
  geom_smooth(
    method      = "lm",
    formula     = y ~ x,
    se          = TRUE,
    alpha       = 0.15,
    linewidth   = 1.6,
    show.legend = FALSE
  ) +
  geom_point(size = 5, alpha = 0.90, stroke = 1.2) +
  scale_colour_manual(
    values = RENKLER,
    labels = ETIKETLER
  ) +
  scale_fill_manual(
    values = RENKLER,
    labels = ETIKETLER
  ) +
  scale_shape_manual(
    values = SEKILLER,
    labels = ETIKETLER
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        shape  = unname(SEKILLER),
        fill   = unname(RENKLER),
        colour = unname(RENKLER),
        size   = 6,
        alpha  = 1,
        stroke = 1.2
      )
    ),
    fill  = "none",
    shape = "none"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = 0.09)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = 0.09)
  ) +
  labs(
    title    = "\u015eikayet T\u00fcr\u00fcne G\u00f6re Kay\u0131p-Ka\u00e7ak Oran\u0131",
    subtitle = "Faturalama ve Saya\u00e7 \u015fikayetleri  |  2015\u20132024  |  G\u00f6lge: %95 g\u00fcven aral\u0131\u011f\u0131",
    x        = "\u015eikayet Oran\u0131 (%)",
    y        = "Ka\u00e7ak/Usuls\u00fcz Kullan\u0131m Oran\u0131 (%)",
    caption  = "Kaynak: EPDK (2015\u20132024)"
  ) +
  tema()

# -- Disa aktarim ------------------------------------------------
dir.create(dirname(CIKTI), showWarnings = FALSE, recursive = TRUE)
ggsave(CIKTI, p, width = W, height = H, dpi = RES, bg = "white")
message("Kaydedildi: ", CIKTI)
