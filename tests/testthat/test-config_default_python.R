test_that("process_required_modules_spec extracts package names correctly", {
    # Test with version constraints
    modules_with_versions <- c("napistu" = "0.3.3", "pandas" = "1.5.0")
    result <- process_required_modules_spec(modules_with_versions)
    expect_equal(result, c("napistu>=0.3.3", "pandas>=1.5.0"))
    
    # Test with NA version (no minimum version)
    modules_with_na <- c("napistu" = "0.3.3", "pandas" = NA_character_)
    result_na <- process_required_modules_spec(modules_with_na)
    expect_equal(result_na, c("napistu>=0.3.3", "pandas"))
    
    # Test with empty string version
    modules_with_empty <- c("napistu" = "0.3.3", "pandas" = "")
    result_empty <- process_required_modules_spec(modules_with_empty)
    expect_equal(result_empty, c("napistu>=0.3.3", "pandas"))
})

test_that("process_required_modules_spec validates input", {
    expect_error(
        process_required_modules_spec(c("napistu", "pandas")),
        "Assertion on 'required_modules' failed: Must have Object"
    )
})