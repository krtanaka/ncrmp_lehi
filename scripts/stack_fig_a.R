# Load the plotly package
library(plotly)

# Set parameters for the grid and planes
num_rectangles <- 12    # Number of planes (months)
grid_size <- 3          # 5x5 grid
rect_size <- 3         # Size of each plane (along x and y axes)
z_offset <- 0.2         # Vertical offset between planes

# Initialize the plot
fig <- plot_ly()

# Generate grid coordinates for x and y
x_seq <- seq(-rect_size/2, rect_size/2, length.out = grid_size + 1)
y_seq <- seq(-rect_size/2, rect_size/2, length.out = grid_size + 1)

# Compute the center positions of the grid cells
x_centers <- (x_seq[-(grid_size + 1)] + x_seq[-1]) / 2
y_centers <- (y_seq[-(grid_size + 1)] + y_seq[-1]) / 2

# Create a grid of x and y centers
grid <- expand.grid(x = x_centers, y = y_centers)

# Set a base gradient direction (e.g., 45 degrees)
base_angle <- pi / 4  # 45 degrees in radians

# Loop over each plane
for (i in 1:num_rectangles) {
  # Set the z position for each plane
  z_center <- i * z_offset
  
  # Slightly vary the gradient direction for each plane
  angle_variation <- runif(1, -pi/2, pi/2)  # Variation up to +/- 10 degrees
  angle <- base_angle + angle_variation
  dx <- cos(angle)
  dy <- sin(angle)
  
  # Compute gradient values for each cell
  gradient_values <- dx * grid$x + dy * grid$y
  
  # Normalize gradient values to range from 0 to 1
  gradient_values <- gradient_values - min(gradient_values)
  gradient_values <- gradient_values / max(gradient_values)
  
  # Map gradient values to colors between blue and red
  colors <- colorRampPalette(c("blue", "red"))(100)
  color_indices <- round(gradient_values * 99) + 1
  cell_colors <- colors[color_indices]
  
  # Convert cell_colors to a matrix matching the grid
  plane_colors <- matrix(cell_colors, nrow = grid_size, ncol = grid_size, byrow = TRUE)
  
  # Loop over the grid cells to create small rectangles (cells)
  for (xi in 1:grid_size) {
    for (yi in 1:grid_size) {
      # Define the corners of the cell
      x_coords <- c(x_seq[xi], x_seq[xi+1], x_seq[xi+1], x_seq[xi])
      y_coords <- c(y_seq[yi], y_seq[yi], y_seq[yi+1], y_seq[yi+1])
      z_coords <- rep(z_center, 4)
      
      # Indices for the two triangles forming the cell (indices start from 0)
      i_indices <- c(0, 0)
      j_indices <- c(1, 2)
      k_indices <- c(2, 3)
      
      # Get the color for the current cell
      cell_color <- plane_colors[xi, yi]
      
      # Add the cell to the plot
      fig <- fig %>% add_trace(
        type = 'mesh3d',
        x = x_coords,
        y = y_coords,
        z = z_coords,
        i = i_indices,
        j = j_indices,
        k = k_indices,
        facecolor = rep(cell_color, 2),
        opacity = 0.5,
        showscale = FALSE,  # Hide individual color scales
        flatshading = TRUE,
        line = list(color = 'white', width = 1)  # Add white grid outlines
      )
    }
  }
}

# Define the z-axis tick positions and labels
z_positions <- (1:num_rectangles) * z_offset
month_labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# Set axes labels and layout with black background and no background grid lines
fig <- fig %>% layout(
  scene = list(
    xaxis = list(
      title = 'Longitude', 
      titlefont = list(color = 'white'), 
      showgrid = FALSE,
      zeroline = FALSE,
      showbackground = FALSE,
      showticklabels = FALSE,    # Remove x-axis tick labels
      ticks = '',                # Remove x-axis tick marks
      tickvals = list(),         # Remove x-axis tick values
      ticktext = list(),
      showline = FALSE           # Remove x-axis line
    ),
    yaxis = list(
      title = 'Latitude', 
      titlefont = list(color = 'white'), 
      showgrid = FALSE,
      zeroline = FALSE,
      showbackground = FALSE,
      showticklabels = FALSE,    # Remove y-axis tick labels
      ticks = '',                # Remove y-axis tick marks
      tickvals = list(),         # Remove y-axis tick values
      ticktext = list(),
      showline = FALSE           # Remove y-axis line
    ),
    zaxis = list(
      title = '',
      tickvals = z_positions,
      ticktext = month_labels,
      titlefont = list(color = 'white'), 
      tickfont = list(color = 'white'),
      showgrid = FALSE,
      zeroline = FALSE,
      showbackground = FALSE
    ),
    bgcolor = 'black'  # Set scene background color to black
  ),
  paper_bgcolor = 'black',  # Set paper background to black
  plot_bgcolor = 'black'    # Set plot background to black
)

# Display the plot
fig