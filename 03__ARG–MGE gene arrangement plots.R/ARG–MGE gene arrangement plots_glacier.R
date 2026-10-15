library(ggplot2)
library(gggenes)
library(ggfittext)
library(patchwork)

rm(list = ls())

# ---------------------------------------------------------
# Plot 1
# ---------------------------------------------------------

data1 <- read.csv(
  "ARG_MGE_glacier_contig.txt",
  header = TRUE,
  sep = "\t"
)

p1 <- ggplot(
  data1,
  aes(
    xmin = start,
    xmax = end,
    y = contig,
    fill = type,
    forward = orientation == 1,
    label = gene
  )
) +
  geom_gene_arrow(
    arrowhead_height = unit(3, "mm"),
    arrowhead_width = unit(1, "mm")
  ) +
  geom_gene_label(
    align = "left"
  ) +
  facet_wrap(
    ~ contig,
    scales = "free",
    ncol = 1
  ) +
  scale_fill_manual(
    values = c(
      ARG = "#D55E00",
      transposase = "#9BBB59",
      integrase = "#8064A2"
    )
  ) +
  geom_gene_label(
    aes(label = gene),
    size = 3,
    family = "Arial",
    fontface = "italic"
  ) +
  theme_genes()


# ---------------------------------------------------------
# Plot 2
# ---------------------------------------------------------

data2 <- read.csv(
  "ARG_MGE_glacier_contig2.txt",
  header = TRUE,
  sep = "\t"
)

p2 <- ggplot(
  data2,
  aes(
    xmin = start,
    xmax = end,
    y = contig,
    fill = type,
    forward = orientation == 1,
    label = gene
  )
) +
  geom_gene_arrow(
    arrowhead_height = unit(3, "mm"),
    arrowhead_width = unit(1, "mm")
  ) +
  geom_gene_label(
    align = "left"
  ) +
  facet_wrap(
    ~ contig,
    scales = "free",
    ncol = 1
  ) +
  scale_fill_manual(
    values = c(
      ARG = "#D55E00",
      transposase = "#9BBB59",
      integrase = "#8064A2"
    )
  ) +
  geom_gene_label(
    aes(label = gene),
    size = 3,
    family = "Arial",
    fontface = "italic"
  ) +
  theme_genes()


# ---------------------------------------------------------
# Plot 3
# ---------------------------------------------------------

data3 <- read.csv(
  "ARG_MGE_glacier_contig3.txt",
  header = TRUE,
  sep = "\t"
)

p3 <- ggplot(
  data3,
  aes(
    xmin = start,
    xmax = end,
    y = contig,
    fill = type,
    forward = orientation == 1,
    label = gene
  )
) +
  geom_gene_arrow(
    arrowhead_height = unit(3, "mm"),
    arrowhead_width = unit(1, "mm")
  ) +
  geom_gene_label(
    align = "left"
  ) +
  facet_wrap(
    ~ contig,
    scales = "free",
    ncol = 1
  ) +
  scale_fill_manual(
    values = c(
      ARG = "#D55E00",
      transposase = "#9BBB59",
      integrase = "#8064A2"
    )
  ) +
  geom_gene_label(
    aes(label = gene),
    size = 3,
    family = "Arial",
    fontface = "italic"
  ) +
  theme_genes()


# ---------------------------------------------------------
# Combine
# ---------------------------------------------------------

p_all <- wrap_plots(
  p1, p2, p3,
  ncol = 1,
  guides = "collect"
)

# 不建议直接运行 p_all 预览
# 直接导出

ggsave(
  "ARG_MGE_glacier_combined.pdf",
  plot = p_all,
  width = 10,
  height = 18,
  limitsize = FALSE
)
