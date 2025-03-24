#!/bin/bash

# Vehicle Error Testing Script
# A simple script for testing all Vehicle error examples

# Clear result file
echo "# Vehicle Error Test Results" > error_examples/test_results.txt
echo "Generated on `date`" >> error_examples/test_results.txt
echo "" >> error_examples/test_results.txt

# Process each VCL file
for file in error_examples/*.vcl
do
  # Get the base filename
  filename=`basename $file`
  echo "Testing: $filename"
  
  # Add to result file
  echo "" >> error_examples/test_results.txt
  echo "## Testing: $filename" >> error_examples/test_results.txt
  echo '```json' >> error_examples/test_results.txt
  
  # Execute test and capture output
  output=$(cabal exec vehicle -- --json-error check -s "$file" 2>&1)
  
  # Add results to output file
  if [ -n "$output" ]; then
    echo "$output" >> error_examples/test_results.txt
  else
    echo "No error output generated" >> error_examples/test_results.txt
  fi
  
  echo '```' >> error_examples/test_results.txt
done

# Add statistics
echo "" >> error_examples/test_results.txt
echo "# Error Statistics" >> error_examples/test_results.txt
echo '```' >> error_examples/test_results.txt
echo "Total files tested: `ls -1 error_examples/*.vcl | wc -l`" >> error_examples/test_results.txt
echo '```' >> error_examples/test_results.txt

echo "All tests completed."
echo "Results saved to error_examples/test_results.txt"
