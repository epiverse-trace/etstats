#!/usr/bin/env Rscript

# GitHub Contributions Analysis for Epiverse-TRACE Members
# Run with: Rscript github_contributions.R [token]

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  cat("Usage: Rscript github_contributions.R <github_token>\n")
  cat("Example: Rscript github_contributions.R ghp_xxxxxxxxxxxx\n")
  quit(status = 1)
}

GITHUB_TOKEN <- args[1]

# Load required libraries
required_packages <- c("httr", "jsonlite", "dplyr", "lubridate")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cran.r-project.org")
}

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
})

# Target organizations
orgs <- c('ropensci', 'r-lib', 'ropensci-review-tools', 'carpentries')

# Function to get GitHub API headers
get_headers <- function(token) {
  return(c("User-Agent" = "R-GitHub-Contributions-Script",
           "Authorization" = paste("Bearer", token)))
}

# Function to safely make API calls with rate limiting
safe_api_call <- function(url, headers, max_retries = 3) {
  for (attempt in 1:max_retries) {
    response <- GET(url, do.call(add_headers, as.list(headers)))

    if (status_code(response) == 200) {
      return(response)
    } else if (status_code(response) == 403) {
      rate_limit_remaining <- headers(response)[["x-ratelimit-remaining"]]
      if (!is.null(rate_limit_remaining) && as.numeric(rate_limit_remaining) == 0) {
        reset_time <- as.numeric(headers(response)[["x-ratelimit-reset"]])
        wait_time <- reset_time - as.numeric(Sys.time()) + 5
        cat("Rate limit exceeded. Waiting", wait_time, "seconds...\n")
        Sys.sleep(max(wait_time, 0))
      } else {
        cat("Access forbidden for:", url, "\n")
        return(NULL)
      }
    } else if (status_code(response) == 404) {
      cat("Resource not found:", url, "\n")
      return(NULL)
    } else {
      cat("API call failed with status", status_code(response), "for:", url, "\n")
      if (attempt == max_retries) return(NULL)
      Sys.sleep(2^attempt)
    }
  }
  return(NULL)
}

# Function to get all epiverse-trace org members
get_epiverse_members <- function(headers) {
  cat("Fetching epiverse-trace organization members...\n")

  members <- c()

  # Get regular members
  members_url <- "https://api.github.com/orgs/epiverse-trace/members"
  page <- 1

  while (TRUE) {
    response <- safe_api_call(paste0(members_url, "?page=", page, "&per_page=100"), headers)
    if (is.null(response)) break

    data <- fromJSON(content(response, "text"))
    if (length(data) == 0) break

    # Handle both data frame and list formats
    if (is.data.frame(data)) {
      page_members <- data$login
    } else if (is.list(data)) {
      page_members <- sapply(data, function(x) x$login)
    } else {
      cat("Unexpected data format for members\n")
      break
    }

    members <- c(members, page_members)

    cat("  Found", length(page_members), "members on page", page, "\n")

    page <- page + 1
    if (length(data) < 100 || (is.data.frame(data) && nrow(data) < 100)) break

    Sys.sleep(0.1)
  }

  # Get outside collaborators
  cat("Fetching epiverse-trace outside collaborators...\n")
  collaborators_url <- "https://api.github.com/orgs/epiverse-trace/outside_collaborators"
  page <- 1

  while (TRUE) {
    response <- safe_api_call(paste0(collaborators_url, "?page=", page, "&per_page=100"), headers)
    if (is.null(response)) break

    data <- fromJSON(content(response, "text"))
    if (length(data) == 0) break

    # Handle both data frame and list formats
    if (is.data.frame(data)) {
      page_collaborators <- data$login
    } else if (is.list(data)) {
      page_collaborators <- sapply(data, function(x) x$login)
    } else {
      cat("Unexpected data format for collaborators\n")
      break
    }

    members <- c(members, page_collaborators)

    cat("  Found", length(page_collaborators), "outside collaborators on page", page, "\n")

    page <- page + 1
    if (length(data) < 100 || (is.data.frame(data) && nrow(data) < 100)) break

    Sys.sleep(0.1)
  }

  # Remove duplicates
  members <- unique(members)
  cat("Total unique epiverse-trace members and collaborators:", length(members), "\n")

  return(members)
}

# Function to get issues/PRs for a user in target organizations
get_org_contributions <- function(username, orgs, headers) {
  cat("Fetching org contributions for", username, "...\n")

  all_contributions <- data.frame()

  for (org in orgs) {
    cat("  Checking", org, "...\n")

    # URL encode the search query
    search_query <- paste0("org:", org, " author:", username)
    encoded_query <- URLencode(search_query)
    search_url <- paste0("https://api.github.com/search/issues?q=", encoded_query)

    page <- 1
    while (TRUE) {
      response <- safe_api_call(paste0(search_url, "&page=", page, "&per_page=100"), headers)
      if (is.null(response)) break

      data <- fromJSON(content(response, "text"))

      if (data$total_count == 0 || length(data$items) == 0) break

      # Extract relevant fields - handle both list and data frame formats
      items <- data$items

      if (is.data.frame(items) && nrow(items) > 0) {
        # Data frame format
        page_contributions <- data.frame(
          username = username,
          source_type = "org",
          source = org,
          html_url = items$html_url,
          state = items$state,
          created_at = items$created_at,
          closed_at = ifelse(is.null(items$closed_at) || length(items$closed_at) == 0,
                            rep(NA, nrow(items)), items$closed_at),
          repository = sapply(strsplit(items$repository_url, "/"), function(x) paste(tail(x, 2), collapse = "/")),
          title = items$title,
          type = ifelse(is.null(items$pull_request), "issue", "pull_request"),
          stringsAsFactors = FALSE
        )

        all_contributions <- dplyr::bind_rows(all_contributions, page_contributions)

      } else if (is.list(items) && length(items) > 0) {
        # List format - process each item
        for (item in items) {
          if (is.list(item) && !is.null(item$html_url)) {
            # Extract repository name from repository_url
            repo_name <- "unknown"
            if (!is.null(item$repository_url)) {
              repo_parts <- strsplit(item$repository_url, "/")[[1]]
              if (length(repo_parts) >= 2) {
                repo_name <- paste(tail(repo_parts, 2), collapse = "/")
              }
            }

            page_contribution <- data.frame(
              username = username,
              source_type = "org",
              source = org,
              html_url = item$html_url,
              state = ifelse(is.null(item$state), "unknown", item$state),
              created_at = ifelse(is.null(item$created_at), NA, item$created_at),
              closed_at = ifelse(is.null(item$closed_at), NA, item$closed_at),
              repository = repo_name,
              title = ifelse(is.null(item$title), "Unknown", item$title),
              type = ifelse(is.null(item$pull_request), "issue", "pull_request"),
              stringsAsFactors = FALSE
            )

            all_contributions <- dplyr::bind_rows(all_contributions, page_contribution)
          }
        }
      }

      page <- page + 1
      # Check break condition
      if (is.data.frame(items)) {
        if (nrow(items) < 100) break
      } else {
        if (length(items) < 100) break
      }
      if (page > 10) break  # GitHub search API limit

      Sys.sleep(0.2)  # Rate limiting for search API
    }
  }

  cat("  Found", nrow(all_contributions), "org contributions for", username, "\n")
  return(all_contributions)
}

# Function to get epidemiology task view repositories
get_epi_repositories <- function(headers) {
  cat("Fetching epidemiology task view repositories...\n")

  csv_url <- "https://raw.githubusercontent.com/cran-task-views/Epidemiology/refs/heads/main/data/source_repositories.csv"

  tryCatch({
    response <- GET(csv_url, do.call(add_headers, as.list(headers)))
    if (status_code(response) == 200) {
      csv_content <- content(response, "text")
      epi_data <- read.csv(text = csv_content, stringsAsFactors = FALSE)

      # Keep only repositories with valid github_repo values (owner/repo format)
      if ("github_repo" %in% names(epi_data)) {
        valid_repos <- epi_data$github_repo[!is.na(epi_data$github_repo) &
                                           epi_data$github_repo != "" &
                                           grepl("/", epi_data$github_repo) & !grepl("epiverse-trace", epi_data$github_repo) & !grepl("epiforecasts", epi_data$github_repo)]
        cat("Found", length(valid_repos), "valid GitHub repositories in epidemiology task view\n")
        return(valid_repos)
      } else {
        cat("No 'github_repo' column found in epidemiology CSV\n")
        return(character(0))
      }
    } else {
      cat("Failed to fetch epidemiology CSV (status:", status_code(response), ")\n")
      return(character(0))
    }
  }, error = function(e) {
    cat("Error fetching epidemiology CSV:", e$message, "\n")
    return(character(0))
  })
}

# Function to get issues/PRs for a user in epidemiology repositories
get_repo_contributions <- function(username, repos, headers) {
  cat("Fetching repo contributions for", username, "...\n")

  all_contributions <- data.frame()

  for (repo in repos) {
    cat("  Checking", repo, "...\n")

    # Split owner/repo
    repo_parts <- strsplit(repo, "/")[[1]]
    if (length(repo_parts) != 2) {
      cat("    Skipping invalid repo format:", repo, "\n")
      next
    }

    owner <- repo_parts[1]
    repo_name <- repo_parts[2]

    # Query repository issues/PRs created by user
    repo_url <- paste0("https://api.github.com/repos/", owner, "/", repo_name, "/issues")
    page <- 1

    while (TRUE) {
      query_params <- list(
        creator = username,
        state = "all",
        page = page,
        per_page = 100
      )

      response <- safe_api_call(paste0(repo_url, "?page=", page, "&per_page=100&creator=", username, "&state=all"), headers)

      if (status_code(response) != 200) {
        if (status_code(response) == 404) {
          cat("    Repository not found or private:", repo, "\n")
        } else {
          cat("    Failed to fetch issues for", repo, "- Status:", status_code(response), "\n")
        }
        break
      }

      data <- fromJSON(content(response, "text"))
      if (length(data) > 0) cat(sprintf("Found %s contributions\n", length(data)))

      if (length(data) == 0 || (is.data.frame(data) && nrow(data) == 0)) break

      # Extract relevant fields - handle both list and data frame formats
      if (is.data.frame(data) && nrow(data) > 0) {
        # Data frame format
        page_contributions <- data.frame(
          username = username,
          source_type = "repo",
          source = repo,
          html_url = data$html_url,
          state = data$state,
          created_at = data$created_at,
          closed_at = ifelse(is.null(data$closed_at) || length(data$closed_at) == 0,
                            rep(NA, nrow(data)), data$closed_at),
          repository = repo,
          title = data$title,
          type = ifelse(is.null(data$pull_request), "issue", "pull_request"),
          stringsAsFactors = FALSE
        )

        all_contributions <- dplyr::bind_rows(all_contributions, page_contributions)

      } else if (is.list(data) && length(data) > 0) {
        # List format - process each item
        for (item in data) {
          if (is.list(item) && !is.null(item$html_url)) {
            page_contribution <- data.frame(
              username = username,
              source_type = "repo",
              source = repo,
              html_url = item$html_url,
              state = ifelse(is.null(item$state), "unknown", item$state),
              created_at = ifelse(is.null(item$created_at), NA, item$created_at),
              closed_at = ifelse(is.null(item$closed_at), NA, item$closed_at),
              repository = repo,
              title = ifelse(is.null(item$title), "Unknown", item$title),
              type = ifelse(is.null(item$pull_request), "issue", "pull_request"),
              stringsAsFactors = FALSE
            )

            all_contributions <- dplyr::bind_rows(all_contributions, page_contribution)
          }
        }
      }

      page <- page + 1
      # Check break condition based on data format
      if (is.data.frame(data)) {
        if (nrow(data) < 100) break
      } else {
        if (length(data) < 100) break
      }

      Sys.sleep(0.1)  # Rate limiting
    }
  }

  cat("  Found", nrow(all_contributions), "repo contributions for", username, "\n")
  return(all_contributions)
}

# Main execution
main <- function() {
  cat("GitHub Contributions Analysis for Epiverse-TRACE Members\n")
  cat("=======================================================\n")
  cat("Target organizations:", paste(orgs, collapse = ", "), "\n\n")

  # Set up headers
  headers <- get_headers(GITHUB_TOKEN)

  # Get epiverse-trace members
  members <- get_epiverse_members(headers)

  if (length(members) == 0) {
    cat("No members found. Exiting.\n")
    return()
  }

  # Get epidemiology repositories
  epi_repos <- get_epi_repositories(headers)

  # Initialize results
  all_contributions <- data.frame()

  # Process each member
  for (i in seq_along(members)) {
    username <- members[i]
    cat("\n[", i, "/", length(members), "] Processing", username, "...\n")

    # Get contributions to target organizations
    org_contributions <- get_org_contributions(username, orgs, headers)

    # Get contributions to epidemiology repositories
    repo_contributions <- data.frame()
    if (length(epi_repos) > 0) {
      repo_contributions <- get_repo_contributions(username, epi_repos, headers)
    }
    # Combine contributions for this user
    user_contributions <- dplyr::bind_rows(org_contributions, repo_contributions)

    if (nrow(user_contributions) > 0) {
      all_contributions <- dplyr::bind_rows(all_contributions, user_contributions)
      cat("  Total contributions for", username, ":", nrow(user_contributions), "\n")
    } else {
      cat("  No contributions found for", username, "\n")
    }

    # Progress checkpoint every 10 users
    if (i %% 10 == 0) {
      cat("\nProgress checkpoint: Processed", i, "of", length(members), "members\n")
      cat("Total contributions so far:", nrow(all_contributions), "\n")
    }
  }

  # Generate summary
  cat("\n=== FINAL SUMMARY ===\n")
  cat("Total members processed:", length(members), "\n")
  cat("Total contributions found:", nrow(all_contributions), "\n")

  if (nrow(all_contributions) > 0) {
    # Convert dates
    all_contributions$created_at <- as.POSIXct(all_contributions$created_at, format = "%Y-%m-%dT%H:%M:%SZ")
    all_contributions$closed_at <- as.POSIXct(all_contributions$closed_at, format = "%Y-%m-%dT%H:%M:%SZ")
    # https://data.org/news/wellcome-rockefeller-fund-new-data-org-initiative-epiverse-analyzing-emergence-and-spread-of-pandemics/
    all_contributions <- all_contributions[all_contributions$created_at > "2021-09-14 00:00:00", ]

    # Summary by type
    type_summary <- all_contributions %>%
      group_by(type) %>%
      summarise(count = n(), .groups = 'drop')

    cat("\n=== CONTRIBUTIONS BY TYPE ===\n")
    print(type_summary)

    # Summary by source
    source_summary <- all_contributions %>%
      group_by(source_type, source) %>%
      summarise(
        contributions = n(),
        contributors = n_distinct(username),
        .groups = 'drop'
      ) %>%
      arrange(desc(contributions))

    cat("\n=== CONTRIBUTIONS BY SOURCE ===\n")
    print(source_summary)

    # Top contributors
    contributor_summary <- all_contributions %>%
      group_by(username) %>%
      summarise(
        total_contributions = n(),
        issues = sum(type == "issue"),
        pull_requests = sum(type == "pull_request"),
        repositories = n_distinct(repository),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_contributions))

    cat("\n=== TOP CONTRIBUTORS ===\n")
    print(head(contributor_summary, 20))

    # Save results
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Save all contributions
    contributions_file <- paste0("data/contributions/epiverse_contributions_", timestamp, ".csv")
    write.csv(all_contributions, contributions_file, row.names = FALSE)
    cat("\nAll contributions saved to:", contributions_file, "\n")

    # Save summaries
    source_summary_file <- paste0("data/contributions/source_summary_", timestamp, ".csv")
    write.csv(source_summary, source_summary_file, row.names = FALSE)
    cat("Source summary saved to:", source_summary_file, "\n")

    contributor_summary_file <- paste0("data/contributions/contributor_summary_", timestamp, ".csv")
    write.csv(contributor_summary, contributor_summary_file, row.names = FALSE)
    cat("Contributor summary saved to:", contributor_summary_file, "\n")

  } else {
    cat("No contributions found.\n")
  }

  cat("\nAnalysis complete!\n")
}

# Run the main function
main()
