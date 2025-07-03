test_that("load_optional_list_value returns value if present, NULL if missing", {
    test_list <- list(a = "value_a", b = 42, c = TRUE)
    
    # Returns value when key exists
    expect_equal(load_optional_list_value(test_list, "a"), "value_a")
    expect_equal(load_optional_list_value(test_list, "b"), 42)
    expect_equal(load_optional_list_value(test_list, "c"), TRUE)
    
    # Returns NULL when key doesn't exist
    expect_null(load_optional_list_value(test_list, "missing"))
    expect_null(load_optional_list_value(test_list, "d"))
    
    # Edge case: empty list
    expect_null(load_optional_list_value(list(), "anything"))
})

test_that("ensure_absolute_path correctly handles paths", {
    skip_on_os("windows")
    
    # Absolute path should remain unchanged
    expect_equal(ensure_absolute_path("/usr/local/bin"), "/usr/local/bin")
    
    expect_equal(substr(ensure_absolute_path("my_file"), 1, 1), "/")
})