-- A more complex Vehicle specification for testing
-- Uses multiple functions, parameters, and tensor operations

@parameter
threshold : Real

@parameter  
weight : Real

-- Test function with multiple arguments
add_weighted : Real -> Real -> Real
add_weighted x y = x + weight * y

-- Test function with conditional logic
check_threshold : Real -> Bool  
check_threshold x = x >= threshold

-- Test function combining multiple operations
complex_computation : Real -> Real
complex_computation x = 
  let doubled = x + x in
  let weighted = add_weighted doubled threshold in
  weighted * weight

-- Test boolean function with multiple conditions
safety_check : Real -> Real -> Bool
safety_check x y = 
  let sum_val = add_weighted x y in
  check_threshold sum_val

@property
main_property : Bool
main_property = 
  let test_val = complex_computation threshold in
  safety_check test_val weight