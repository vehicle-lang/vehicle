# Vehicle Error Example Files

This folder contains multiple error examples in the Vehicle language, used to test and demonstrate the error reporting functionality of Vehicle, especially the JSON format error output.

## Error File Descriptions

### Syntax Errors

1. **01_syntax_error_missing_semicolon.vcl**
   - Error: Missing semicolon
   - Correct Usage: Add a semicolon at the end of the statement
   - Command: `cabal exec vehicle -- --json check -s error_examples/01_syntax_error_missing_semicolon.vcl`

2. **02_syntax_error_unfinished_expr.vcl**
   - Error: Unfinished expression
   - Correct Usage: Complete the expression
   - Command: `cabal exec vehicle -- --json check -s error_examples/02_syntax_error_unfinished_expr.vcl`

3. **03_syntax_error_unbalanced_paren.vcl**
   - Error: Unmatched parentheses
   - Correct Usage: Add the missing parentheses
   - Command: `cabal exec vehicle -- --json check -s error_examples/03_syntax_error_unbalanced_paren.vcl`

### Reference Errors

4. **04_circular_reference.vcl**
   - Error: Circular reference
   - Correct Usage: Break the circular dependency
   - Command: `cabal exec vehicle -- --json check -s error_examples/04_circular_reference.vcl`

### Function Errors

5. **05_function_redefinition.vcl**
   - Error: Function redefinition
   - Correct Usage: Ensure function names are unique
   - Command: `cabal exec vehicle -- --json check -s error_examples/05_function_redefinition.vcl`

### Type Errors

6. **06_type_mismatch.vcl**
   - Error: Type mismatch
   - Correct Usage: Use the correct type
   - Command: `cabal exec vehicle -- --json check -s error_examples/06_type_mismatch.vcl`

7. **07_missing_arguments.vcl**
   - Error: Missing arguments
   - Correct Usage: Provide all required arguments
   - Command: `cabal exec vehicle -- --json check -s error_examples/07_missing_arguments.vcl`

8. **08_too_many_arguments.vcl**
   - Error: Too many arguments
   - Correct Usage: Provide only the necessary arguments
   - Command: `cabal exec vehicle -- --json check -s error_examples/08_too_many_arguments.vcl`

9. **09_undefined_type.vcl**
   - Error: Undefined type
   - Correct Usage: Use a defined type or define a new type
   - Command: `cabal exec vehicle -- --json check -s error_examples/09_undefined_type.vcl`

10. **10_invalid_annotation.vcl**
    - Error: Invalid annotation
    - Correct Usage: Use the correct annotation format
    - Command: `cabal exec vehicle -- --json check -s error_examples/10_invalid_annotation.vcl`

11. **11_missing_type_declaration.vcl**
    - Error: Missing type declaration
    - Correct Usage: Add the necessary type declaration
    - Command: `cabal exec vehicle -- --json check -s error_examples/11_missing_type_declaration.vcl`

12. **12_inconsistent_vector.vcl**
    - Error: Inconsistent vector element types
    - Correct Usage: Use elements of the same type
    - Command: `cabal exec vehicle -- --json check -s error_examples/12_inconsistent_vector.vcl`

13. **13_float_as_int.vcl**
    - Error: Using float as integer
    - Correct Usage: Use the correct numeric type
    - Command: `cabal exec vehicle -- --json check -s error_examples/13_float_as_int.vcl`

14. **14_property_not_bool.vcl**
    - Error: Property is not a boolean
    - Correct Usage: Ensure the property is of boolean type
    - Command: `cabal exec vehicle -- --json check -s error_examples/14_property_not_bool.vcl`

15. **15_function_type_mismatch.vcl**
    - Error: Function type mismatch
    - Correct Usage: Correct the function type or usage
    - Command: `cabal exec vehicle -- --json check -s error_examples/15_function_type_mismatch.vcl`

16. **16_wrong_argument_type.vcl**
    - Error: Wrong argument type
    - Correct Usage: Use the correct type of argument
    - Command: `cabal exec vehicle -- --json check -s error_examples/16_wrong_argument_type.vcl`

17. **17_vector_size_mismatch.vcl**
    - Error: Vector size mismatch
    - Correct Usage: Use vectors of the same size
    - Command: `cabal exec vehicle -- --json check -s error_examples/17_vector_size_mismatch.vcl`

### Other Errors

18. **18_invalid_tokens.vcl**
    - Error: Invalid tokens
    - Correct Usage: Use valid syntax tokens
    - Command: `cabal exec vehicle -- --json check -s error_examples/18_invalid_tokens.vcl`

19. **19_duplicate_definition.vcl**
    - Error: Duplicate definition
    - Correct Usage: Ensure each identifier is defined only once
    - Command: `cabal exec vehicle -- --json check -s error_examples/19_duplicate_definition.vcl`

20. **20_custom_type_mismatch.vcl**
    - Error: Custom type mismatch
    - Correct Usage: Follow the custom type definition
    - Command: `cabal exec vehicle -- --json check -s error_examples/20_custom_type_mismatch.vcl`

21. **21_type_error_operation.vcl**
    - Error: Type error in operation
    - Correct Usage: Use a type suitable for the operation
    - Command: `cabal exec vehicle -- --json check -s error_examples/21_type_error_operation.vcl`

22. **22_invalid_index.vcl**
    - Error: Invalid index
    - Correct Usage: Use a valid index value
    - Command: `cabal exec vehicle -- --json check -s error_examples/22_invalid_index.vcl`

23. **23_duplicate_annotation.vcl**
    - Error: Duplicate annotation
    - Correct Usage: Avoid duplicate annotations
    - Command: `cabal exec vehicle -- --json check -s error_examples/23_duplicate_annotation.vcl`

24. **24_index_out_of_bounds.vcl**
    - Error: Index out of bounds
    - Correct Usage: Use an index within the range
    - Command: `cabal exec vehicle -- --json check -s error_examples/24_index_out_of_bounds.vcl`

25. **25_non_numeric_index.vcl**
    - Error: Non-numeric index
    - Correct Usage: Use a numeric index
    - Command: `cabal exec vehicle -- --json check -s error_examples/25_non_numeric_index.vcl`

26. **26_invalid_quantifier.vcl**
    - Error: Invalid quantifier
    - Correct Usage: Use valid quantifier syntax
    - Command: `cabal exec vehicle -- --json check -s error_examples/26_invalid_quantifier.vcl`

## Usage

To view the JSON error output of a single error file, use the following command format:

```bash
cabal exec vehicle -- --json check -s error_examples/filename.vcl
```

To format the JSON output for readability, you can use:

```bash
cabal exec vehicle -- --json check -s error_examples/filename.vcl 2>/tmp/error.json && cat /tmp/error.json | python3 -m json.tool
```

## Error Output Parsing

The JSON error output contains the following main fields:
- `error`: The complete error message, including the filename, location, and detailed error description
- `provenance`: Error location information, containing the following subfields:
  - `file`: Filename
  - `range`: Error range, including `start` and `end` positions, each with `line` and `column` values

Example Output:
```json
{
  "error": "Error in file 'test.vcl' at Line 1, Columns 18-19: the value '5' is too big to be used as an index of size '3'.",
  /* Error message field: Contains the complete error description, formatted as "Error in file '[filename]' at Line [line number], Columns [column range]: [specific error description]" */

  "provenance": {
    /* Error location information: Contains precise location data where the error occurred */
    "file": "test.vcl",  /* Filename where the error is located */
    "range": {
      /* Error range information: Determines the exact location of the error in the code */
      "end": {
        /* Error end position */
        "column": 19,  /* End column number: counting starts from 1 */
        "line": 1      /* End line number: counting starts from 1 */
      },
      "start": {
        /* Error start position */
        "column": 18,  /* Start column number: counting starts from 1 */
        "line": 1      /* Start line number: counting starts from 1 */
      }
    }
  }
}
```

### Error Field Parsing Details

1. **Main Error Information**
   - The `error` field contains a human-readable complete error description
   - Includes filename, line number, column number, and specific error explanation
   - Used for quickly understanding the nature and location of the error

2. **Location Information Structure**
   - `provenance` contains machine-parsable location data
   - Facilitates precise error location in IDEs and other tools
   - Can be used for automatic navigation to the error location or highlighting the error code segment

3. **Range Notation**
   - `range` provides the precise range of the error
   - `start` and `end` together define the code range to be highlighted
   - For single-character errors, `start` and `end` line and column values may be the same
   - For errors spanning multiple lines, line numbers will differ

4. **Application Example**
   - In the example above, the error is located at columns 18-19 of line 1
   - The error type is index out of bounds: attempting to access an index beyond the vector size
   - The index value '5' exceeds the vector size of '3'
