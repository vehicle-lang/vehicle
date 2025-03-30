#!/bin/bash

# Vehicle Error Testing Script
# A simple script for testing all Vehicle error examples

# Initialize JSON array
echo "{" > error_examples/test_results.json
echo "  \"timestamp\": \"`date`\"," >> error_examples/test_results.json
echo "  \"tests\": [" >> error_examples/test_results.json

# Process each VCL file
first=true
for file in error_examples/*.vcl
do
  # Get the base filename
  filename=`basename $file`
  echo "Testing: $filename"

  # Add comma if not first entry
  if [ "$first" = true ]; then
    first=false
  else
    echo "    ," >> error_examples/test_results.json
  fi

  # Execute test and capture output
  output=$(cabal exec vehicle -- --json check -s "$file" 2>&1)

  # Add results to output file
  echo "    {" >> error_examples/test_results.json
  echo "      \"file\": \"$filename\"," >> error_examples/test_results.json
  if [ -n "$output" ]; then
    echo "      \"result\": $output" >> error_examples/test_results.json
  else
    echo "      \"result\": null" >> error_examples/test_results.json
  fi
  echo "    }" >> error_examples/test_results.json
done

# Add statistics and close JSON
echo "  ]," >> error_examples/test_results.json
echo "  \"statistics\": {" >> error_examples/test_results.json
echo "    \"total_files\": `ls -1 error_examples/*.vcl | wc -l`" >> error_examples/test_results.json
echo "  }" >> error_examples/test_results.json
echo "}" >> error_examples/test_results.json

echo "All tests completed."
echo "Results saved to error_examples/test_results.json"
