# Item content diagram 
library(data.table)
library(gtools)


cbcl_general <- readxl::read_xlsx("/Users/mgell/Downloads/CBCL_models.xlsx", sheet = "General")
cbcl_specific <- readxl::read_xlsx("/Users/mgell/Downloads/CBCL_models.xlsx", sheet = "Specific")
cbcl_general <- as.data.table(cbcl_general)
cbcl_specific <- as.data.table(cbcl_specific)

# General factor 
cbcl_general_data <- cbcl_general
cbcl_general_data = melt(cbcl_general_data, id.vars="Items", variable.name="Models", value.name="Factor") #Transform to long format
cbcl_general_data <- cbcl_general_data %>% dplyr::filter(Factor==1); cbcl_general_data <- as.data.table(cbcl_general_data) #Keep the Models in which the symptoms are 1 (general)
cbcl_general_data[, Factor := factor(Factor, labels=c("General factor"))]
cbcl_general_data[, count := .N, by="Items"]

## Symptom order
sympt.order = cbcl_general_data[, .N, by="Items"][order(N)][, Items] #Replace by order
cbcl_general_data[, Items := factor(Items, levels = sympt.order)]

## Scale order by frequency
scale.order = cbcl_general_data[, .N, by=Models][order(N)][, Models]
cbcl_general_data[, Models := factor(Models, levels = scale.order)]
cbcl_general_data[, Models2 := as.numeric(Models)]

## Order items alphabetically
#cbcl_general_data[, Items := factor(Items, levels = sort(unique(Items)))]

## Plot
# cbcl_general_data[, Items := factor(
#   Items,
#   levels = mixedsort(unique(as.character(Items)))
# )]

# ---- REPLACE your "## Plot" section for the GENERAL plot with this ----
cbcl_general_plot <- ggplot(cbcl_general_data,
                            aes(x = Items, y = Models2, group = Items,
                                color = as.factor(Models), shape = Factor)) +
  geom_line(alpha = .6, linewidth = .3, colour = "grey55") +   # thinner, lighter
  xlab(NULL) + ylab(NULL) +
  geom_hline(yintercept = 1:11, colour = "grey85", linewidth = .2, linetype = "dotted") +
  geom_vline(xintercept = 1:119, colour = "grey90", linewidth = .15) +
  geom_point(size = 2.2, stroke = .6) +
  coord_polar(direction = -1) +
  scale_shape_manual(values = c(1)) +
  scale_color_viridis_d(option = "C", end = .9) +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    #legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = unit(rep(.5, 4), "lines"),
    legend.position = "right",                     # move to the side
    legend.text = element_text(size = 11),         # increase text size
    legend.key.size = unit(0.8, "lines")           # optional: bigger keys
  ) +
  scale_y_continuous(limits = c(-4, 11), expand = c(0, 0),
                     breaks = 1:11, labels = cbcl_general_data[, levels(Models)]) +
  ggtitle("CBCL items with general-factor loadings across models")
#ggsave(plot=cbcl_general_plot,filename="/Users/mgell/Downloads/Figure1A.jpg", width=10, height=10)
#ggsave(plot=cbcl_general_plot,filename="/Users/mgell/Downloads/Figure1A.pdf", width=15, height=11)
ggsave(plot=cbcl_general_plot,filename="/Users/mgell/Downloads/Figure1A.pdf", width=13, height=10)



# Specific factors
cbcl_specific_data <- cbcl_specific
cbcl_specific_data = melt(cbcl_specific_data, id.vars="Items", variable.name="Models", value.name="Factor") #Transform to long format
cbcl_specific_data <- cbcl_specific_data %>% dplyr::filter(Factor>=1); cbcl_specific_data <- as.data.table(cbcl_specific_data) #Keep the Models in which the symptoms are 1 (specific)
cbcl_specific_data[, Factor := factor(Factor, labels=c("Internalizing", "Anxious-dep.", "Withdraw-dep.", "Somatic", "Externalizing", "Rule-breaking", "Aggressive", "Social", "Thought", "Attention"))]
cbcl_specific_data[, count := .N, by="Items"]

## Symptom order
sympt.order = cbcl_specific_data[, .N, by="Items"][order(N)][, Items] #Replace by order
cbcl_specific_data[, Items := factor(Items, levels = sympt.order)]

## Scale order by frequency
scale.order = cbcl_specific_data[, .N, by=Models][order(N)][, Models]
cbcl_specific_data[, Models := factor(Models, levels = scale.order)]
cbcl_specific_data[, Models2 := as.numeric(Models)]

## Order items alphabetically
# cbcl_specific_data[, Items := factor(
#   Items,
#   levels = mixedsort(unique(as.character(Items)))
# )]

## Plot
cbcl_specific_plot <- ggplot(cbcl_specific_data, aes(x = Items, y = Models2, group = Items, color = as.factor(Models), shape=Factor, rev=F)) +
  geom_line(alpha = .6, linewidth = .3, colour = "grey55") +
  geom_point(size = 2.2, stroke = .6) +
  xlab(NULL) + ylab(NULL) +
  geom_hline(yintercept = 1:11, colour = "grey85", linewidth = .2, linetype = "dotted") +
  geom_vline(xintercept = 1:119, colour = "grey90", linewidth = .15) +
  coord_polar(direction = -1) +
  scale_shape_manual(values = c(21, 2, 5, 0, 19, 15, 17, 10, 9, 8)) +  # keep your factor shapes
  scale_color_viridis_d(option = "C", end = .9) +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    #legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = unit(rep(.5, 4), "lines"),
    legend.position = "right",                     # move to the side
    legend.text = element_text(size = 12),         # increase text size
    legend.key.size = unit(0.8, "lines")           # optional: bigger keys
  ) +
  scale_y_continuous(
    limits = c(-4, 11), expand = c(0, 0),
    breaks = 1:11,
    labels = cbcl_specific_data[, levels(Models)]
  ) +
  ggtitle("CBCL items with specific-factor loadings across models")
#ggsave(plot=cbcl_specific_plot,filename="Figure1B.pdf", width=14, height=10, useDingbats=FALSE)
#ggsave(plot=cbcl_specific_plot,filename="/Users/mgell/Downloads/Figure1B.jpg", width=14, height=10)
#ggsave(plot=cbcl_specific_plot,filename="/Users/mgell/Downloads/Figure1B.pdf", width=15, height=11)
ggsave(plot=cbcl_specific_plot,filename="/Users/mgell/Downloads/Figure1B.pdf", width=13, height=10)
