# Package index

## Initialization

Configure a `napistu_list` object containing a database and
network-based representation of a pathway and bindings to the Napistu
Python package.

### Configuration

object- and yaml-level configuration of the core `napistu_list`

- [`setup_napistu_list()`](https://napistu.github.io/napistu-r/reference/setup_napistu_list.md)
  : Setup Napistu List
- [`create_napistu_config()`](https://napistu.github.io/napistu-r/reference/create_napistu_config.md)
  : Create Napistu Configuration
- [`load_napistu_config()`](https://napistu.github.io/napistu-r/reference/load_napistu_config.md)
  : Load Napistu Configuration from YAML

### Assets

loading the core Napistu objects - an `sbml_dfs`, `napistu_graph`,
`species_identifiers` and optionally, `precomputed_distances`

- [`convert_napistu_graph()`](https://napistu.github.io/napistu-r/reference/convert_napistu_graph.md)
  : Convert Napistu Graph
- [`convert_sbml_dfs()`](https://napistu.github.io/napistu-r/reference/convert_sbml_dfs.md)
  : Convert SBML DFs
- [`load_assets()`](https://napistu.github.io/napistu-r/reference/load_assets.md)
  : Load Assets
- [`load_single_asset()`](https://napistu.github.io/napistu-r/reference/load_single_asset.md)
  : Load Single Asset File

### Default Python Environment

Setting up and tearing down a conda environment with Napistu installed
improve usability and support CI/CD

- [`cleanup_napistu()`](https://napistu.github.io/napistu-r/reference/cleanup_napistu.md)
  : Clean Up Napistu Environment
- [`create_conda_environment()`](https://napistu.github.io/napistu-r/reference/create_conda_environment.md)
  : Create Conda Environment
- [`create_default_conda_env()`](https://napistu.github.io/napistu-r/reference/create_default_conda_env.md)
  : Create Default Conda Environment

## Network Visualization

Approaches for querying and visualizing Napistu graphs.

### Subgraphs

Creating an induced subgraph containing a set of vertices of interest
and visualizing the resulting weaklyconnected components

- [`define_subgraphs()`](https://napistu.github.io/napistu-r/reference/define_subgraphs.md)
  : Create Subgraph
- [`plot_subgraph()`](https://napistu.github.io/napistu-r/reference/plot_subgraph.md)
  : Plot Subgraphs
- [`plot_one_component()`](https://napistu.github.io/napistu-r/reference/plot_one_component.md)
  : Plot One Component

### Neighborhoods

Creating and plotting the molecular neighborhood of a focal species

- [`create_neighborhood_summary_table()`](https://napistu.github.io/napistu-r/reference/create_neighborhood_summary_table.md)
  : Create Neighborhood Summary Table
- [`create_neighborhood_table()`](https://napistu.github.io/napistu-r/reference/create_neighborhood_table.md)
  : Create Neighborhood Table
- [`plot_one_neighborhood()`](https://napistu.github.io/napistu-r/reference/plot_one_neighborhood.md)
  : Plot One Neighborhood
- [`plot_neighborhoods()`](https://napistu.github.io/napistu-r/reference/plot_neighborhoods.md)
  : Plot Neighborhoods

### Shortest Paths

Find and plot the shortest network paths between two species

- [`find_all_shortest_paths()`](https://napistu.github.io/napistu-r/reference/find_all_shortest_paths.md)
  : Find All Shortest Reaction Paths
- [`plot_shortest_path_network()`](https://napistu.github.io/napistu-r/reference/plot_shortest_path_network.md)
  : Plot Shortest Path Network
- [`summarize_shortest_paths()`](https://napistu.github.io/napistu-r/reference/summarize_shortest_paths.md)
  : Summarize Shortest Paths

## Shiny Modules

Interactive Shiny modules for exploring Napistu networks

### Shiny Module - Basic Info

- [`shiny_basicInfo_test()`](https://napistu.github.io/napistu-r/reference/shiny_basicInfo_test.md)
  : Shiny Basic Info Test
- [`basicInfoInput()`](https://napistu.github.io/napistu-r/reference/basicInfoInput.md)
  : Basic Info Input
- [`basicInfoServer()`](https://napistu.github.io/napistu-r/reference/basicInfoServer.md)
  : Basic Info Server

### Shiny Module - Select Entity

- [`shiny_selectEntity_test()`](https://napistu.github.io/napistu-r/reference/shiny_selectEntity_test.md)
  : Shiny Select Entity Test
- [`selectEntityInput()`](https://napistu.github.io/napistu-r/reference/selectEntityInput.md)
  : Select Entity Input
- [`selectEntityServer()`](https://napistu.github.io/napistu-r/reference/selectEntityServer.md)
  : Select Entity Server

### Shiny Module - Neighborhoods

- [`shiny_neighborhood_test()`](https://napistu.github.io/napistu-r/reference/shiny_neighborhood_test.md)
  : Shiny Neighborhood Test
- [`neighborhoodInput()`](https://napistu.github.io/napistu-r/reference/neighborhoodInput.md)
  : Neighborhood Input
- [`neighborhoodServer()`](https://napistu.github.io/napistu-r/reference/neighborhoodServer.md)
  : Neighborhood Server

### Shiny Module - Shortest Paths

- [`shiny_shortestPaths_test()`](https://napistu.github.io/napistu-r/reference/shiny_shortestPaths_test.md)
  : Shiny Shortest Paths Test
- [`shortestPathsInput()`](https://napistu.github.io/napistu-r/reference/shortestPathsInput.md)
  : Shortest Paths Input
- [`shortestPathsServer()`](https://napistu.github.io/napistu-r/reference/shortestPathsServer.md)
  : Shortest Paths Server

## R-Based Functionality Supporting Napistu Pathway Transformations

These functions are used to pull data and metadata into the Napistu
framework. Many of these functions are accessed through the rpy2 module
of the Napistu Python package or through its CLI. With time hopefully
some or all of these R calls from Python can be replaced with pure
Python.

### Gene Metadata

Many of the functions in Napistu depend on genic metadata which is
currently pulled from specialized species-level Bioconductor packages.

- [`bioconductor_org_function()`](https://napistu.github.io/napistu-r/reference/bioconductor_org_function.md)
  : Bioconductor Org Function
- [`bioconductor_org_library()`](https://napistu.github.io/napistu-r/reference/bioconductor_org_library.md)
  : Bioconductor Org Library
- [`bioconductor_org_package_prefix()`](https://napistu.github.io/napistu-r/reference/bioconductor_org_package_prefix.md)
  : Bioconductor Org Package Prefix

## Additional Functionality

Lightly supported functionality which is loosly related to Napistu

### Open Targets

Functions for pulling data from the Open Targets graphQL API

- [`query_open_targets_targets()`](https://napistu.github.io/napistu-r/reference/query_open_targets_targets.md)
  : Query Open Targets - Targets
- [`query_open_targets_indications()`](https://napistu.github.io/napistu-r/reference/query_open_targets_indications.md)
  : Query Open Targets - Indications
- [`shiny_opentargets_test()`](https://napistu.github.io/napistu-r/reference/shiny_opentargets_test.md)
  : Shiny Open Targets Test
- [`summarize_open_targets_targets()`](https://napistu.github.io/napistu-r/reference/summarize_open_targets_targets.md)
  : Summarize Open Targets - Targets
- [`summarize_diseases()`](https://napistu.github.io/napistu-r/reference/summarize_diseases.md)
  : Summarize Diseases
- [`summarize_indication()`](https://napistu.github.io/napistu-r/reference/summarize_indication.md)
  : Summarize Indications

## Data

Data which is bundled with `napistu-r`

- [`efo_diseases`](https://napistu.github.io/napistu-r/reference/efo_diseases.md)
  : EFO Diseases
