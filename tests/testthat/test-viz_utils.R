library(testthat)
library(igraph)
library(cli)

# Test setup helper function
create_test_graph <- function(n_edges = 3, add_weight_attr = FALSE, weight_values = NULL) {
    g <- igraph::make_empty_graph(n = 4, directed = FALSE)
    g <- igraph::add_edges(g, c(1,2, 2,3, 3,4))
    
    if (add_weight_attr) {
        if (is.null(weight_values)) {
            weight_values <- c(0.1, 0.5, 0.2)
        }
        igraph::edge_attr(g, "weight") <- weight_values
    }
    
    return(g)
}

test_that("explicit NA returns NA silently", {
    g <- create_test_graph()
    expect_silent(result <- process_weights_for_layout(g, network_layout = "fr", edge_weights = NA))
    expect_true(is.na(result))
})

test_that("NULL behavior: extracts weight attribute or warns if missing", {
    # With weight attribute - should extract and process
    g_with_weights <- create_test_graph(add_weight_attr = TRUE, weight_values = c(0.1, 0.5, 0.2))
    result <- process_weights_for_layout(g_with_weights, network_layout = "fr", edge_weights = NULL)
    expect_true(is.numeric(result))
    expect_length(result, 3)
    
    # Without weight attribute - should return NA
    g_no_weights <- create_test_graph(add_weight_attr = FALSE)
    result <- process_weights_for_layout(g_no_weights, network_layout = "fr", edge_weights = NULL)
    expect_true(is.na(result))
})

test_that("attribute name handling: valid vs invalid", {
    g <- create_test_graph()
    igraph::edge_attr(g, "custom_weight") <- c(0.3, 0.7, 0.1)
    
    # Valid attribute name
    result <- process_weights_for_layout(g, network_layout = "kk", edge_weights = "custom_weight")
    expect_equal(result, c(0.3, 0.7, 0.1))
    
    # Invalid attribute name - should return NA
    result <- process_weights_for_layout(g, network_layout = "fr", edge_weights = "nonexistent")
    expect_true(is.na(result))
})

test_that("weight validation catches major errors", {
    g <- create_test_graph()
    
    # Non-numeric weights - should return NA
    result <- process_weights_for_layout(g, network_layout = "fr", edge_weights = c("a", "b", "c"))
    expect_true(is.na(result))
    
    # Non-positive weights - should return NA
    result <- process_weights_for_layout(g, network_layout = "fr", edge_weights = c(-0.1, 0.5, 0.2))
    expect_true(is.na(result))
    
    # Length mismatch - should return NA
    result <- process_weights_for_layout(g, network_layout = "fr", edge_weights = c(0.1, 0.5))
    expect_true(is.na(result))
})

test_that("weight inversion works correctly by layout type", {
    g <- create_test_graph()
    weights <- c(0.1, 0.5, 1.0)
    
    # FR layout (attraction) should invert weights - smaller becomes larger
    result_fr <- process_weights_for_layout(g, edge_weights = weights, network_layout = "fr")
    expect_true(result_fr[1] > result_fr[2])  # 0.1 inverted > 0.5 inverted
    expect_true(result_fr[2] > result_fr[3])  # 0.5 inverted > 1.0 inverted
    
    # KK layout (repulsion) should not invert weights
    result_kk <- process_weights_for_layout(g, edge_weights = weights, network_layout = "kk")
    expect_equal(result_kk, weights)
})

test_that("handles extreme edge weight cases gracefully", {
    # Very small weights should still produce positive finite results
    g <- create_test_graph()
    very_small_weights <- c(1e-10, 1e-8, 1e-6)
    
    result <- process_weights_for_layout(g, edge_weights = very_small_weights, network_layout = "fr")
    expect_true(all(result > 0))
    expect_true(all(is.finite(result)))
})

test_that("edge filtering for visualization works", {
    
    edges <- tibble::tibble(
        from = c("A", "B", "C", "D"),
        to = c("B", "C", "D", "A"),
        weight = c(0.2, 0.6, 0.8, 0.3),
        correlation = c(0.95, 0.4, 0.7, 0.85)
    )
    
    spec <- list(
        weight = list(cutoff = 0.32, retain = "above"),
        correlation = list(cutoff = 0.9, retain = "below")
    )
    
    filtered_edges <- apply_edge_filters(edges, spec)
    expect_equal(nrow(filtered_edges), 2)
})

test_that("edge filtering handles NA values correctly", {
    
    edges_with_na <- tibble::tibble(
        from = c("A", "B", "C", "D", "E", "F"),
        to = c("B", "C", "D", "A", "F", "E"),
        weight = c(0.2, 0.6, 0.8, 0.3, NA, 0.1),        # NA in weight
        correlation = c(0.95, 0.4, 0.7, 0.85, 0.5, NA)  # NA in correlation
    )
    
    spec <- list(
        weight = list(cutoff = 0.32, retain = "above"),
        correlation = list(cutoff = 0.9, retain = "below")
    )
    
    # Capture warnings about NA values
    filtered_edges <- apply_edge_filters(edges_with_na, spec)
    
    # Check that NA rows are retained
    # Original logic: weight >= 0.32 OR weight is NA, AND correlation <= 0.9 OR correlation is NA
    # Row 1: weight=0.2 (fails), correlation=0.95 (fails) -> filtered out
    # Row 2: weight=0.6 (passes), correlation=0.4 (passes) -> kept
    # Row 3: weight=0.8 (passes), correlation=0.7 (passes) -> kept  
    # Row 4: weight=0.3 (fails), correlation=0.85 (passes) -> filtered out
    # Row 5: weight=NA (passes), correlation=0.5 (passes) -> kept
    # Row 6: weight=0.1 (fails), correlation=NA (passes) -> filtered out (fails weight filter)
    
    expect_equal(nrow(filtered_edges), 3)
    
    # Verify specific rows that should be kept
    expect_true("B" %in% filtered_edges$from)  # Row 2: B->C
    expect_true("C" %in% filtered_edges$from)  # Row 3: C->D  
    expect_true("E" %in% filtered_edges$from)  # Row 5: E->F (has NA weight)
    
    # Verify that NA values are preserved
    expect_true(any(is.na(filtered_edges$weight)))
    
    # Check that rows with weight < 0.32 AND defined correlation are filtered out
    expect_false("A" %in% filtered_edges$from)  # Row 1: weight=0.2, correlation=0.95
    expect_false("D" %in% filtered_edges$from)  # Row 4: weight=0.3, correlation=0.85
    expect_false("F" %in% filtered_edges$from)  # Row 6: weight=0.1, correlation=NA
})
