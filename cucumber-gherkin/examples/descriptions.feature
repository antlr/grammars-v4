Feature: Descriptions everywhere
  This is a single line description

  Scenario: two lines
  This description
  has two lines and indented with two spaces
    Given the minimalism

Scenario: without indentation
This is a description without indentation
  Given the minimalism

  Scenario: empty lines in the middle
  This description

  has an empty line in the middle
    Given the minimalism

  Scenario: empty lines around

  This description
  has an empty lines around

    Given the minimalism

  Scenario: comment after description
  This description
  has a comment after

# this is a comment
    Given the minimalism

  Scenario: comment right after description
  This description
  has a comment right after
    #  this is another comment

    Given the minimalism

  Scenario: description with escaped docstring separator
  This description has an \"\"\" (escaped docstring sparator)

    Given the minimalism

  Scenario Outline: scenario outline with a description
This is a scenario outline description
    Given the minimalism

  Examples: examples with description
This is an examples description
    | foo |
    | bar |
