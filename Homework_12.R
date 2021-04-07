# Isaac Racine
# HW12
# 07 Apr 2021
#------------------------------------------------
library(datasets)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(cowplot)
library(colorspace)
library(ggsci)
library(wesanderson)
library(TeachingDemos)

char2seed("Fianl homework already?")
d <- diamonds
#############################################  

#-------------- Plot 1 ---------------
#basic box plot
box_plot_cols <- c("lemonchiffon", "lightcyan", "mistyrose",
                   "palegreen", "peachpuff2")

p1 <- ggplot(d, aes(x = as.factor(cut), y = carat, fill = cut)) +
  geom_boxplot() +
  plot_annotation("Amount of Carats for Each Diamond Quality",
                  theme = theme(plot.title = element_text(size = 16, colour = "grey5"), text = element_text(family = "serif"))) +
  labs(y = "Carat Amount") +
  scale_fill_manual(values = box_plot_cols, name = "Type of Cut") +
  theme_bw(base_family = "serif") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) 

print(p1)


#-------------- Plot 2---------------
#Will demonstrate adding text annonations
#was going to do a geom_point but
# this dataset has over 50000 data points so it isn't very clear
linear_colors <- c("yellowgreen", "slateblue3", "seagreen3",
                   "salmon4" , "orangered3") 

p2 <- ggplot(d, aes(x = carat, y = depth, color = cut)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = linear_colors, name = "Type of Cut") +
  plot_annotation("Linear Regression Plot Comparing Diamond Depth to
                  Carat and Groupped by Cut",
                  theme = theme(plot.title = element_text(size = 16, colour = "grey5"), text = element_text(family = "serif"))) +
  labs(y = "Diamond Depth", 
       x = "Diamond Carat Amount") + 
  annotate("text", x = 4, y = 67, label = "Fair", color = "yellowgreen") +
  annotate("text", x = 4.5, y = 61.2, label = "Premium", color = "salmon4") +
  annotate("text", x = 4.5, y = 62, label = "Very Good", color = "seagreen3") +
  annotate("text", x = 0.95, y = 62.5, label = "Good", color = "slateblue3") +
  annotate("text", x = 1, y = 61.55, label = "Ideal", color = "orangered3") +
  theme_bw(base_family = "serif")

print(p2)

#-------------- Plot 3 ---------------
#Stacked hisogram
histo_colors = c("lavenderblush2", "cornsilk", "seagreen1", "powderblue",
                 "tan", "khaki3", "tomato")

p3 <- ggplot(d, aes(x = price)) +
  geom_histogram(aes(fill = color), bins = 10) +
  scale_fill_manual(values = histo_colors, name = "Type of Color") +
  labs(y = "Count",
       x = "Price") +
  plot_annotation("Histogram of Diamonds by Price and Colored by Diamonds Color",
                  theme = theme(plot.title = element_text(size = 16, colour = "grey5"), text = element_text(family = "serif"))) +
  theme_minimal(base_family = "serif")

print(p3)

#-------------- Plot 4 ---------------
#first faceting and changing the facet label colors
p4 <- ggplot(d, aes( y = y,
                     fill = cut)) +
  geom_boxplot() + 
  ylim(0,12) +
  plot_annotation("Amount of Carats for Each Diamond Quality",
                  theme = theme(plot.title = element_text(size = 16, colour = "grey5"), text = element_text(family = "serif"))) +
  labs(y = "Variable Y (height of diamond)") +
  scale_fill_manual(values = box_plot_cols, name = "Type of Cut") +
  theme_test(base_family = "serif") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank())

p4 <- p4 + facet_grid(.~color) +
  theme(
    strip.background = element_rect(
      color="black", fill="darkolivegreen1", size=1.5, linetype="solid"))

print(p4)

#-------------- Plot 5 ---------------
#will demonstrate changing the facet names, or adding the variable #name to each type of outcome type 
#also demonstrates adding an arrow to the plot
p5 <- ggplot(d, aes(y = depth,
                    x = carat)) +
  geom_point() +
  plot_annotation("Amount of Carats by Depth of Diamond",
                  theme = theme(plot.title = element_text(size = 16, colour = "grey5"), text = element_text(family = "serif"))) +
  labs(y = "Diamond Depth",
       x = "Diamond Carat") +
  scale_color_manual(values = box_plot_cols, name = "Type of Cut") +
  theme_test(base_family = "serif") 
  

p5 <- p5 + facet_grid(clarity~color, labeller = label_both)  +
  theme(
    strip.background = element_rect(
      color="black", fill="lightyellow", size=1.5, linetype="solid")) +
  geom_segment(aes(x = 5, y = 60, xend = 3.5, yend = 25),
               arrow = arrow(length = unit(0.5, "cm")))

## an example of saving my plot with ggsave

ggsave("facet_wrap_of_carat_depth_by_color_clarity.pdf", plot = p5,width = 5, height = 8, units = "in")
print(p5)
