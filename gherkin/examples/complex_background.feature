Feature: Complex background
  We want to ensure PickleStep all have different IDs

  Background: a simple background
    Given the minimalism inside a background

  Scenario: minimalistic
    Given the minimalism

  Scenario: also minimalistic
    Given the minimalism

  Rule: My Rule

    Background:
      Given a rule background step

    Scenario: with examples
      Given the <value> minimalism

      Examples:
      | value |
      | 1     |
      | 2     |
