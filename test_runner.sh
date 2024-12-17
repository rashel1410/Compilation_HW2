#!/bin/bash

# Directories
input_dir="More_tests/input"
expected_dir="More_tests/expected"
output_dir="output"
program="./hw2"

# Create output directory if it doesn't exist
mkdir -p "$output_dir"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Iterate over each input file
for input_file in "$input_dir"/*.in; do
    # Get the base name of the test (e.g., 1_valid)
    test_name=$(basename "$input_file" .in)
    
    # Expected and output file paths
    expected_file="$expected_dir/$test_name.exp"
    output_file="$output_dir/$test_name.out"
    
    # Run the program and redirect output
    "$program" < "$input_file" > "$output_file"
    
    # Check if the expected file exists
    if [[ -f "$expected_file" ]]; then
        # Compare the output file with the expected file
        if diff -q "$output_file" "$expected_file" > /dev/null; then
            echo -e "${GREEN}PASS${NC} - $test_name"
        else
            echo -e "${RED}FAIL${NC} - $test_name"
        fi
    else
        echo -e "${RED}Expected file not found for test: $test_name${NC}"
    fi
done

