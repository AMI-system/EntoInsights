# EntoInsights
The EntoInsights R package provides functions for wrangling, analysing, and visualising entomological datasets generated from AI-based insect identifications, such as those produced by AMI (Automated Monitoring of Insects). 

## Explanation of the package directory structure
- **`DESCRIPTION`**: Contains metadata about the EntoInsights R package, such as the package name, description, version, authors, and dependencies.
  
- **`NAMESPACE`**: Defines the functions that are available/exported to the user when they load the package, and specifies any external packages that are used.
  
- **`R/`**: Contains the R scripts that define the functions in EntoInsights. These functions are divided into scripts by their purpose:
  - `data_wrangling_functions.R`: Contains functions for wrangling data (e.g., filtering, cleaning).
  - `data_analysis_functions.R`: Contains functions for data analysis (e.g., summary statistics, statistical tests, models).
  - `data_visualisation_functions.R`: Contains functions for data visualisation (e.g., creating plots).

- **`man/`**: Automatically generated documentation for the EntoInsights functions. The `.Rd` files are created by roxygen2 based on function documentation.

- **`README.md`**: The main overview document that describes the package’s functionality, installation instructions, and usage.

<br>

# Instructions for contributing to the package
1. Cloning the repository from github to your computer
    * Check Git is installed on your local computer
    ```bash
    git --version
    ```
    * Run the following command in the terminal (I use Git Bash terminal)
    ```bash
    git clone https://github.com/AMI-system/EntoInsights.git
    ```
    * Navigate to the package directory with cd
    * Check the local repository is up-to-date by pulling the latest changes from GitHub
    ```bash
    git pull origin main
    ```
2. Adding a new function
    * Ensure you have devtools, roxygen2, and usethis packages installed. To install these:
    ```r
    install.packages(c("devtools", "roxygen2", "usethis"))
    ```
    * Open the appropriate R script in RStudio. For example:
        * If your function will be used for data wrangling, add it to R/data_wrangling_functions.R
        * If your function will be used for data analysis, add it to R/data_analysis_functions.R
        * If your function will be used for data visualisation, add it to R/data_visualisation_functions.R
    * Naming your function: Use clear, descriptive names for your functions that reflect their purpose. Use all lowercase letters and separate words with underscores (_). For example:
        * keep_only_moths() for a function that filters out non-moths from a dataset
    * Write your function and document with roxygen2 comments including purpose, parameters, return value, examples, any imports, and the export comment (@export makes sure the function is available to users). See the keep_only_moths() function in the R/data_wrangling_functions.R for an example.   
        ```r
        #' Function Title
        #'
        #' A short description of what the function does.
        #'
        #' @param param_name Description of the parameter. Can have multiple.
        #' @return Description of what the function returns.
        #' @examples
        #' # Example of how to use the function
        #' @importFrom package function
        #' @export
        new_function <- function(parameters) {
            # Function body
        }
        ```
    * If your new function uses external packages that are not already imported, you’ll need to add them to the DESCRIPTION file under the Imports section. For example:
        * If your function uses dplyr and ggplot2, add them to the Imports section like this:
        ```
        Imports:
            dplyr,
            ggplot2
        ```
    * Once you've added your function and its documentation, run the following command in R to update the documentation (generates .Rd documentation files and updates the NAMESPACE file):
        ```r
        devtools::document()
        ```
    * You can test your function by running the following 
        ```r
        devtools::load_all(".") # This loads the entire package locally
        ```
        * You will need to ensure you have loaded any required libraries with library(). 
        * You can now use the function you have written as you would normally use an R function. 
        * View function documentation with ?<function_name> e.g., ?keep_only_moths
3. Committing and pushing the changes to the GitHub repo. In a terminal / Git Bash terminal:
* Check the status of Git to see which files have been created/deleted/edited
    ```bash
    git status
    ```
* Add the files you want to include in the next commit
    * You can add everything with
    ```bash
    git add .
    ```
    * Or individual files with
    ```bash
    git add <file_name>
    ```
* Commit the changes (including a commit message describing changes that have been made). For example:
    ```bash
    git commit -m "Added wrangling function to filter out non-moths from a dataset"
    ```
* Push the changes to the GitHub repository
    ```bash
    git push origin main
    ```
<br>

# To do
* Add unit tests using the `testthat` framework
* Start adding new functions e.g.,
    * extract_date_from_timestamp()
    * extract_time_from_timestamp()
    * create_night_of_column() # Assigns the 'night of' date based on the timestamp: if the time is between 18:00 and 23:59, it keeps the same date; if the time is between 00:00 and 05:59, it assigns the previous date. 
    * Functions for reading in and wrangling weather data. 
    * Function to summarise the data by night/hour. 