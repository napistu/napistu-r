test_that("ensure_required_keys passes when all required keys are present", {
    # All required keys present
    expect_no_error(
        ensure_required_keys(
            keys = c("a", "b", "c"),
            required_keys = c("a", "b"),
            object_name = "test_config"
        )
    )
    
    # Exact match
    expect_no_error(
        ensure_required_keys(
            keys = c("x", "y"),
            required_keys = c("x", "y"),
            object_name = "config"
        )
    )
    
    # Empty required keys should always pass
    expect_no_error(
        ensure_required_keys(
            keys = c("a", "b"),
            required_keys = character(0),
            object_name = "anything"
        )
    )
})

test_that("ensure_required_keys fails with informative errors when keys are missing", {
    # Single missing key
    expect_error(
        ensure_required_keys(
            keys = c("a", "b"),
            required_keys = c("a", "b", "missing_key"),
            object_name = "my_config"
        ),
        "my_config.*malformed.*missing_key"
    )
    
    # Multiple missing keys
    expect_error(
        ensure_required_keys(
            keys = c("present"),
            required_keys = c("missing1", "missing2", "present"),
            object_name = "test_object"
        ),
        "test_object.*malformed.*missing1.*missing2"
    )
    
    # Invalid input types should error
    expect_error(
        ensure_required_keys(
            keys = c(1, 2),
            required_keys = c("a"),
            object_name = "test"
        )
    )
})