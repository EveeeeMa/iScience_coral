## Data visualization

### 1. Network plot

if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}

##### Co-regulatory Network
Ptn_edges <- data.frame(
  Ptn1 = c("A", "A", "B", "C"),
  Ptn2 = c("B", "C", "C", "D"),
  Rho = c(0.1, 0.5, 0.6, 0.15))

g <- graph_from_data_frame(Ptn_edges, directed = F)

### Layout
layout <- layout_with_fr(g)

### Edge width
edge_widths <- edges$Rho

### Node color
vertex_color <- c("red", "blue", "grey", "pink")
names(vertex_color) <- c("Group1", "Group2", "Group3", "Group4")

### Plot
plot(g,
     vertex.label = V(g)$name,
     vertex.size = 15,
     vertex.color = vertex_color,
     layout = layout,
     edge.width = edge_widths)

### Legend of Protein group
legend("topleft", 
       legend = names(vertex_color), 
       fill = vertex_color, 
       title = "Protein Group",
       cex = 0.4,
       x.intersp = 0.65,
       y.intersp = 0.65)

### Legend of Rho
min_width <- min(edge_widths)
max_width <- max(edge_widths)
Rho_widths <- seq(min_width, max_width, length.out = 3)

legend("topleft", 
       legend = paste("Width:", Rho_widths), 
       lwd = Rho_widths, 
       lty = 1, 
       title = "Rho",
       cex = 0.4,
       x.intersp = 0.65,
       y.intersp = 0.65,
       inset = c(0, 0.6))


### 2. Heatmap
Ptn_edges <- data.frame(
  Ptn1 = c("A", "A", "B", "C", "D", "B", "A", "D", "B", "C", "D", "A", "C"),
  Ptn2 = c("B", "C", "C", "D", "A", "D", "A", "B", "B", "C", "D", "D", "A"),
  Rho = c(0.1, 0.5, 0.6, 0.15, 0.2, -0.35, 1, -0.35, 1, 1, 1, 0.2, 0.5))

y_labels <- data.frame(x = -0.005, y = c("A", "B", "C", "D"))

ggplot() +
  geom_tile(Ptn_edges, mapping = aes(x = Ptn1, y = Ptn2, fill = Rho), color = "grey") +
  geom_text(y_labels, mapping = aes(x = x, y = y, label = y)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "black") +
  labs(x = "", y = "", title = "Protein Correlation") +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), panel.background = element_blank()) +
  expand_limits(y = -2, x = c(-0.05, 4)) +
  coord_polar()

