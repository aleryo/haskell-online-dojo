#!/bin/bash


while true; do
  sleep 10
  ( stack test --test-arguments="-m Kata" && git add . && git commit -m "tests pass" ) || ( git reset --hard HEAD && git clean -f )
done
