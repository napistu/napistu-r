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