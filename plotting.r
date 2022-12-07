## Function to plot a boxplot
plot_flipper_figure <- function(penguins_flippers){
  penguins_flippers %>% 
    ggplot(aes(x = species, y = flipper_length_mm)) +
    geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
    geom_jitter(aes(color = species), alpha = 0.3, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_x_discrete(labels=c("Adelie","Chinstrap","Gentoo")) +
    labs(x = "Penguin Species",
         y = "Flipper length (mm)") +
    theme_bw()
}

## Function to save the plot as .png
save_flipper_plot_png <- function(penguins_flippers, filename, size, res, scaling){
  agg_png(filename, width = size, 
          height = size, 
          units = "cm", 
          res = res, 
          scaling = scaling)
  flipper_boxplot <- plot_flipper_figure(penguins_flippers)
  print(flipper_boxplot)
  dev.off()
}


## Function to save the plot as .svg
save_flipper_plot_svg <- function(penguin_flippers, filename, size, res, scaling){
  svglite(filename, 
          width = size, height = size)
  flipper_boxplot <- plot_flipper_figure(penguins_flippers)
  plot(flipper_boxplot)
  dev.off()
}

## Function to plot the body mass and culmen lenfth data by species

plot_by_species <- function(penguins_clean){
  penguins_clean %>% 
    ggplot(aes(x=body_mass_g, 
               y=culmen_length_mm,
               colour = species,
               shape = species)) +
    geom_point(size=3, alpha=0.8)+
    theme_bw() +
    labs(title="Body Mass vs Culmen Length", 
         subtitle = "Culmen length and body mass of different penguins",
         x="Body mass (g)",
         y = "Culmen length (mm)")+
    geom_smooth(method="lm")
}


## Function to plot the body mass and culmen length data for Adelie and Gentoo penguins only
plot_M_vs_CL_figure<- function(penguins_AG){
  penguins_AG %>%
    ggplot(aes(x=body_mass_g, 
               y=culmen_length_mm)) +
    geom_point(size=3, alpha=0.8)+
    theme_bw() +
    labs(title="Body Mass vs Culmen Length", 
         subtitle = "Culmen length and body mass for Adelie and Gentoo penguins",
         x="Body mass (g)",
         y = "Culmen length (mm)")+
    geom_smooth(method="lm")
}

## Function to save the penguins_AG_plot as a .png
save_LM_plot_png <- function(penguins_AG, filename, size, res, scaling){
  agg_png(filename, width = size, 
          height = size, 
          units = "cm", 
          res = res, 
          scaling = scaling)
  penguins_AG_plot <- plot_M_vs_CL_figure(penguins_AG)
  print(penguins_AG_plot)
  dev.off()
}

