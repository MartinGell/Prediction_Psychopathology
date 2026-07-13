# install.packages(c("ggraph","tidygraph"))
library(ggraph)
library(tidygraph)

corr_matrix = read_table('/Users/mgell/Work/pfactor/res/beh_cors_cmat_within_only.txt', col_names = FALSE)


corr_matrix <- as.matrix(corr_matrix)
#colnames(corr_matrix) <-c("P","E","I","sum T","sum E","sum I")
colnames(corr_matrix) <-c("P-factors","Extern factors","Intern factors","Total problems","Extern sum score","Intern sum score")
rownames(corr_matrix) <- colnames(corr_matrix)

# corr_matrix = corr_matrix[1:3,1:3]
# 
# 
# g1 <- graph_from_adjacency_matrix(corr_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
# 
# 
# g_nonneg <- graph_from_adjacency_matrix(abs(corr_matrix), mode = "undirected", weighted = TRUE, diag = FALSE)
# 
# # plot(
# #   g_nonneg, layout = lay,
# #   edge.width = scales::rescale(abs(E(g1)$weight), to = c(0.5, 3)), # use signed graph for width
# #   edge.color = ifelse(E(g1)$weight >= 0, "steelblue3", "tomato3"), # keep sign coloring
# #   vertex.size = 22, vertex.label.cex = 0.9
# # )
# 
# g_nonneg <- as_tbl_graph(abs(corr_matrix))
# 
# xy <- create_layout(g_nonneg,layout = "fr", weights = weight)

# 
# library(igraph)
# library(ggraph)
# library(tidygraph)
# library(dplyr)
# 
# # # ---- Start from your correlation matrix ----
# # R <- as.matrix(corr_matrix)
# # diag(R) <- 0
# R = corr_matrix
# # 
# # # (Optional) zero cross-group edges to force two triangles
# # # e.g., first 3 vars = group A, last 3 vars = group B
# # grp <- setNames(rep(c("A","B"), each = ncol(R)/2), colnames(R))
# # R[outer(grp, grp, `!=`)] <- 0
# # 
# # # (Optional) threshold small |r| to sparsify
# # thr <- 0.3
# # R[abs(R) < thr] <- 0
# 
# # Build signed graph and a positive-weight copy for layout
# g_signed <- graph_from_adjacency_matrix(R, mode = "undirected", weighted = TRUE)
# E(g_signed)$sign <- ifelse(E(g_signed)$weight >= 0, "pos", "neg")
# 
# g_pos <- g_signed
# E(g_pos)$weight <- pmax(abs(E(g_pos)$weight), 1e-6)  # FR needs >0
# 
# # Component labels (triangles)
# V(g_signed)$comp <- factor(components(g_signed)$membership)
# 
# tbl_signed <- as_tbl_graph(g_signed)
# xy <- create_layout(as_tbl_graph(g_pos), layout = "fr", weights = weight)
# 
# # Plot: two separate panels, one per triangle
# ggraph(tbl_signed, layout = xy) +
#   geom_edge_link(aes(width = abs(weight), color = sign), alpha = 0.7) +
#   scale_edge_width(range = c(0.5, 2.5)) +
#   scale_edge_color_manual(values = c(neg = "tomato3", pos = "steelblue3")) +
#   geom_node_point(size = 4) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   theme_graph() #+
# #facet_nodes(~comp, ncol = 2, scales = "free")
# 
# 
# 
# 
# 
# 
# 
# 
# library(igraph)
# library(ggraph)
# library(tidygraph)
# 
# R <- corr_matrix
# 
# ## 1) Choose a correlation→distance transform
# ##    Signed (negatives far, positives close):
# D <- sqrt(2 * (1 - pmin(pmax(R, -0.999999), 0.999999)))
# diag(D) <- 0
# 
# ## 2) Sparse edges for visibility (keep only strong |r|)
# A <- R; diag(A) <- 0
# # thr <- 0.35
# # A[abs(A) < thr] <- 0
# g <- graph_from_adjacency_matrix(A, mode="undirected", weighted=TRUE, diag=FALSE)
# E(g)$sign <- ifelse(E(g)$weight >= 0, "pos","neg")
# 
# ## 3) Get 2-D coordinates whose Euclidean distances ≈ D
# coords <- cmdscale(as.dist(D), k = 2)
# xy <- create_layout(as_tbl_graph(g), layout = "manual",
#                     x = coords[,1], y = coords[,2])
# 
# ## 4) Plot: now **edge length** encodes similarity (via the embedding)
# ggraph(xy) +
#   geom_edge_link(aes(color = sign), alpha = 0.7) +
#   scale_edge_color_manual(values = c(neg = "tomato3", pos = "steelblue3")) +
#   geom_node_point(size = 4) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   theme_graph()
# 
# 
# # Jitter it a little so that stuff doesnt get plotted over other stuff
# xy2 <- xy
# xy2$x[xy2$name == "sum E"] <- xy2$x[xy2$name == "sum E"] + 0.03
# xy2$y[xy2$name == "sum E"] <- xy2$y[xy2$name == "sum E"] + 0.02
# xy2$x[xy2$name == "sum T"] <- xy2$x[xy2$name == "sum T"] - 0.03
# xy2$y[xy2$name == "sum T"] <- xy2$y[xy2$name == "sum T"] - 0.02
# 
# ggraph(tbl_signed, layout = xy2) +
#   geom_edge_link(aes(color = sign), alpha = 0.7) +
#   geom_node_point(size = 4) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   theme_graph()
# 
# 







###
# ---- Packages ----
library(igraph)
library(tidygraph)
library(ggraph)
library(ggforce)   # for convex hulls (nice soft group backgrounds)
library(dplyr)
library(scales)
library(grid)      # for unit()
set.seed(42)

# ---- 1) From correlations -> distances (for meaningful edge LENGTHS) ----
R <- as.matrix(corr_matrix)

# Signed distance: strong positive = close, strong negative = far
R <- pmin(pmax(R, -0.999999), 0.999999)
D <- sqrt(2 * (1 - R))
diag(D) <- 0

# ---- 2) Sparsify edges (so the plot isn't a hairball) ----
A <- R; diag(A) <- 0
#thr <- 0.35                     # <- tune for your figure
#A[abs(A) < thr] <- 0

g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)
E(g)$sign <- ifelse(E(g)$weight >= 0, "pos", "neg")

# ---- 3) 2D coords whose Euclidean distances ≈ D (edge length encodes similarity) ----
coords <- cmdscale(as.dist(D), k = 2)

tbl <- as_tbl_graph(g)
xy  <- create_layout(tbl, layout = "manual",
                     x = coords[,1], y = coords[,2])

# ---- 4) Tiny manual nudge for the overlapping nodes you mentioned ----
# nudges <- tibble(name = c("sum E","sum T"),
#                  dx   = c( +0.0, +0.02),
#                  dy   = c( +0.0, -0.0))
# xy <- xy %>% left_join(nudges, by = "name") %>%
#   mutate(x = x + coalesce(dx,0), y = y + coalesce(dy,0)) %>%
#   select(-dx,-dy)

# Component labels (so we can add soft hulls around the two triangles)
xy$comp <- factor(components(g)$membership[xy$.ggraph.orig_index])

# ---- 5) Publication-ready plot ----
edge_cols <- c(pos = "darkred",  # Okabe–Ito blue (CB-friendly)
               neg = "navyblue")  # Okabe–Ito vermilion

p <- ggraph(xy) +
  geom_edge_link2(aes(color = sign, width = abs(weight)), alpha = 0.7,
                  start_cap = circle(2.2, "mm"), end_cap = circle(2.2, "mm")) +
  scale_edge_width(range = c(0.4, 2.4), guide = "none") +
  geom_node_point(shape = 21, size = 3.8, stroke = 0.6, fill = "white") +
  ggrepel::geom_text_repel(
    aes(x = x, y = y, label = name),
    box.padding = 0.4, point.padding = 0.2, max.overlaps = Inf, size = 3
  ) +
  scale_edge_color_manual(values = edge_cols, labels = c(neg = "Negative", pos = "Positive"),
                          name = "Correlation") +
  coord_equal() + theme_graph()

p

# ggraph(xy) +
# # soft background hulls for each triangle; remove this layer if you don't want it
# ggforce::geom_mark_hull(aes(x = x, y = y, group = comp),
#                         expand = unit(2, "mm"), concavity = 3, radius = unit(2, "mm"),
#                         fill = "grey95", color = "grey85", alpha = 0.6, inherit.aes = FALSE) +
# # edges: length already reflects similarity via the MDS coords;
# # width emphasizes magnitude; color encodes sign
# geom_edge_link2(aes(width = abs(weight), alpha = abs(weight), color = sign),
#                 lineend = "round",
#                 start_cap = circle(2.2, "mm"), end_cap = circle(2.2, "mm")) +
# scale_edge_color_manual(values = edge_cols, labels = c(neg = "Negative", pos = "Positive"),
#                         name = "Correlation") +
# scale_edge_width(range = c(0.4, 2.4), guide = "none") +
# scale_edge_alpha(range = c(0.4, 0.9), guide = "none") +
# # nodes + labels
# geom_node_point(shape = 21, size = 3.8, stroke = 0.6, fill = "white", color = "black") +
# geom_node_label(aes(label = name), size = 3, label.padding = unit(1.1, "mm"),
#                 label.r = unit(0.8, "mm"), label.size = 0, fill = "white", repel = TRUE) +
# coord_equal() +
# theme_graph(base_size = 10, base_family = "Helvetica") +
# theme(
#   legend.position = c(0.88, 0.2),
#   legend.background = element_rect(fill = "white", color = NA),
#   plot.margin = margin(5, 8, 5, 8)  # tighter margins
# )

p

# ---- 6) Export (vector + high-DPI raster) ----
ggsave("network_pub.pdf", p, width = 85, height = 70, units = "mm", device = cairo_pdf)  # vector, embeds fonts
ggsave("network_pub.png", p, width = 85, height = 70, units = "mm", dpi = 600)





library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)
library(ggrepel)

R <- as.matrix(corr_matrix)

# Correlation -> target distance (signed)
clip <- function(x) pmin(pmax(x, -0.999999), 0.999999)
Dfull <- sqrt(2 * (1 - clip(R))); diag(Dfull) <- 0

# Sparse adj (use your zeros)
A <- R; diag(A) <- 0
g <- graph_from_adjacency_matrix(A != 0, mode = "undirected", diag = FALSE)

# Embed each connected component *separately* (no rescaling)
labs <- colnames(R)
comp <- components(g)$membership
coords_list <- lapply(split(labs, comp), function(nodes) {
  D <- as.dist(Dfull[nodes, nodes])
  C <- cmdscale(D, k = 2)                   # exact for a triangle
  C <- scale(C, center = TRUE, scale = FALSE)  # just center, no rescale
  tibble(name = nodes, x = C[,1], y = C[,2],
         comp = as.integer(comp[nodes][1]))
})
xy <- bind_rows(coords_list)

# Translate components side-by-side without changing size/shape
gap <- 2
for (k in sort(unique(xy$comp))) {
  idx <- xy$comp == k
  shift <- if (k == min(xy$comp)) 0 else {
    prev <- xy$comp == (k - 1)
    max(xy$x[prev]) - min(xy$x[idx]) + gap
  }
  xy$x[idx] <- xy$x[idx] + ifelse(k == min(xy$comp), 0, shift)
}

# Signed graph for plotting
g_signed <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
E(g_signed)$sign <- ifelse(E(g_signed)$weight >= 0, "pos", "neg")

edge_cols <- c(pos = "red3",  # Okabe–Ito blue (CB-friendly)
               neg = "blue4")  # Okabe–Ito vermilion

# Plot (edge length now reflects the intended distances within each triangle)
p = ggraph(as_tbl_graph(g_signed), layout = xy) +
  geom_edge_link2(aes(color = sign, width = abs(weight)), alpha = 0.9,
                  start_cap = circle(2, "mm"), end_cap = circle(2, "mm")) +
  scale_edge_width(range = c(0.4, 2.4), guide = "none") +
  scale_edge_color_manual(values = edge_cols, labels = c(neg = "Negative", pos = "Positive"),
                          name = "Correlation") +
  geom_node_point(shape = 21, size = 3.6, stroke = 0.6, fill = "white") +
  geom_text_repel(aes(x = x, y = y, label = name), size = 4) +
  coord_equal() + theme_graph()

ggsave("/Users/mgell/Work/pfactor/plots/network_pub_beh.pdf", p, width = 85, height = 70, units = "mm", device = cairo_pdf)  # vector, embeds fonts
ggsave("/Users/mgell/Work/pfactor/plots/network_pub_beh.png", p, width = 4.5, height = 3, dpi = 600)



# 
# 
# 
# 
# 
# tri_coords <- function(nodes, D) {
#   stopifnot(length(nodes) == 3)
#   a <- D[nodes[1], nodes[2]]
#   b <- D[nodes[1], nodes[3]]
#   c <- D[nodes[2], nodes[3]]
#   x3 <- (b^2 + a^2 - c^2) / (2*a)
#   y3 <- sqrt(max(b^2 - x3^2, 0))
#   tibble::tibble(
#     name = nodes,
#     x = c(0, a, x3),
#     y = c(0, 0, y3)
#   )
# }
# 
# # build both triangles, then translate the second one to the right
# tri1 <- tri_coords(c("P","E","I"), Dfull)
# tri2 <- tri_coords(c("sum T","sum E","sum I"), Dfull) %>%
#   dplyr::mutate(x = x + max(tri1$x) + 2)
# 
# xy <- dplyr::bind_rows(tri1, tri2)
# 
# 
# 
# # --- pkgs ---
# library(igraph)
# library(tidygraph)
# library(ggraph)
# library(dplyr)
# library(ggrepel)
# 
# # --- 0) Input: your correlation matrix ---
# R <- as.matrix(corr_matrix)           # must be a base matrix
# diag(R) <- 0
# 
# # --- 1) Correlation -> target distances (signed) ---
# clip  <- function(x) pmin(pmax(x, -0.999999), 0.999999)
# Dfull <- sqrt(2 * (1 - clip(R)))      # Euclidean distance in correlation space
# diag(Dfull) <- 0
# 
# # Graph just to get components & for plotting sign
# A <- (R != 0); diag(A) <- 0
# g_bool   <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
# g_signed <- graph_from_adjacency_matrix(R, mode = "undirected",
#                                         weighted = TRUE, diag = FALSE)
# E(g_signed)$sign <- ifelse(E(g_signed)$weight >= 0, "pos", "neg")
# 
# labs <- colnames(R)
# comps <- split(labs, components(g_bool)$membership)
# 
# # --- 2) Global 2-D MDS (for overall placement only) ---
# Cglob <- cmdscale(as.dist(Dfull), k = 2)
# xyglob <- tibble(name = rownames(Cglob), x = Cglob[,1], y = Cglob[,2])
# 
# # --- 3) Exact triangle coordinates + align (rotation + translation, no scaling) ---
# # closed-form triangle from three side lengths
# tri_coords <- function(nodes, D) {
#   stopifnot(length(nodes) == 3)
#   a <- D[nodes[1], nodes[2]]
#   b <- D[nodes[1], nodes[3]]
#   c <- D[nodes[2], nodes[3]]
#   x3 <- (b^2 + a^2 - c^2) / (2*a)
#   y3 <- sqrt(max(b^2 - x3^2, 0))
#   tibble(name = nodes, x = c(0, a, x3), y = c(0, 0, y3))
# }
# 
# # align X (exact triangle) to Y (global) with rotation + translation only
# align_to_global <- function(nodes, D, xyglob) {
#   tri <- tri_coords(nodes, D)
#   X   <- as.matrix(tri[, c("x","y")])
#   Y   <- as.matrix(xyglob[match(nodes, xyglob$name), c("x","y")])
#   
#   # center both
#   Xc <- scale(X, center = TRUE, scale = FALSE)
#   Yc <- scale(Y, center = TRUE, scale = FALSE)
#   
#   # Kabsch (optimal rotation): SVD of X^T Y
#   sv <- svd(t(Xc) %*% Yc)
#   Rrot <- sv$u %*% t(sv$v)              # 2x2 rotation matrix (no scaling)
#   
#   # rotate X to match Y's orientation, then translate to Y's centroid
#   Xaligned <- Xc %*% Rrot
#   cenY <- colMeans(Y)
#   tri$x <- Xaligned[,1] + cenY[1]
#   tri$y <- Xaligned[,2] + cenY[2]
#   tri
# }
# 
# xy_list <- lapply(comps, align_to_global, D = Dfull, xyglob = xyglob)
# xy <- bind_rows(xy_list)
# 
# # --- 4) Plot (edge length now respects the intended ordering within each component) ---
# p <- ggraph(as_tbl_graph(g_signed), layout = xy) +
#   geom_edge_link2(aes(color = sign), alpha = 0.9,
#                   start_cap = circle(2, "mm"), end_cap = circle(2, "mm")) +
#   scale_edge_color_manual(values = c(pos = "#0072B2", neg = "#D55E00"),
#                           name = "Correlation") +
#   geom_node_point(shape = 21, size = 3.6, stroke = 0.6, fill = "white") +
#   geom_text_repel(aes(x = x, y = y, label = name), size = 3,
#                   box.padding = 0.3, point.padding = 0.2, max.overlaps = Inf) +
#   coord_equal() + theme_graph()
# p
# 
# # --- 5) Quick sanity check for your example ---
# # expect d(sum I, sum T) < d(sum I, sum E) because 0.479 > 0.284
# pd <- as.matrix(dist(as.matrix(xy[, c("x","y")])))
# pd[xy$name == "sum I", xy$name == "sum T"]
# pd[xy$name == "sum I", xy$name == "sum E"]
# 
