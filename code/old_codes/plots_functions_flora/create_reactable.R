

library(reactable)
library(dplyr)

# Function to create a reactable table with exact match filtering
create_reactable <- function(data) {
  
  # Get column names of the dataframe
  column_names <- colnames(data)
  
  # Create columns list for reactable
  columns_list <- lapply(column_names, function(col_name) {
    
    # Check if the column is numeric or factor/character
    if (is.factor(data[[col_name]]) || is.character(data[[col_name]])) {
      # Apply exact matching filter for factor/character columns
      colDef(
        filterable = TRUE,
        filterMethod = JS("
          (rows, columnId, filterValue) => {
            return rows.filter(row => {
              const cellValue = row.values[columnId];
              return cellValue === filterValue;  // Only return exact matches
            });
          }
        ")
      )
    } else {
      # Apply default filter for numeric columns
      colDef(
        filterable = TRUE,
        align = "center"
      )
    }
  })
  
  # Convert list to a named list for reactable
  columns_named <- setNames(columns_list, column_names)
  
  # Create and return the reactable table
  reactable(
    data,  # Your dataset
    defaultPageSize = nrow(data),  # Show all rows in a scrollable table
    resizable = TRUE,              # Allow resizing columns
    filterable = TRUE,             # Enable filtering for each column
    searchable = TRUE,             # Add a global search bar
    bordered = TRUE,               # Add borders to cells
    striped = TRUE,                # Alternate row colors
    highlight = TRUE,              # Highlight rows on hover
    style = list(fontSize = "12px"), # Adjust text size
    fullWidth = TRUE,              # Stretch the table to fit the container
    height = "auto",               # Adjust the height automatically
    columns = columns_named        # Dynamically generated columns list
  )
}


