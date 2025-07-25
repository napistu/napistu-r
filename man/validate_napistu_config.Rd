% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validators.R
\name{validate_napistu_config}
\alias{validate_napistu_config}
\title{Validate Napistu Configuration Object}
\usage{
validate_napistu_config(napistu_config)
}
\arguments{
\item{napistu_config}{A napistu configuration object of class `napistu_config`
containing all necessary configuration settings for the loading required
assets and configuring and/or using an exiting appropriate Python environment.
This config can be read from a yaml file using \link{load_napistu_config} or
directly specified in R using \link{create_napistu_config}. Contains:
  
\describe{
    \item{python_config}{
        A shallow list defining the Python environment.
        See \link{validate_python_config}
    }
    \item{assets_config}{
        A shallow list defining the Napistu assets to use.
        See \link{validate_assets_config}
    }
    \item{created_at}{Time stamp}
}}
}
\value{
Invisible TRUE if valid, throws error if invalid
}
\description{
Validate Napistu Configuration Object
}
\keyword{internal}
