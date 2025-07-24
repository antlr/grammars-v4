Feature: Incomplete scenario outlines

  Background: Adding a background won't make a pickle
    * a step

  Scenario Outline: steps, no examples
    Given a step

  Scenario Outline: no steps, no examples

  Scenario Outline: no steps, no table

    Examples:

  Scenario Outline: no steps, only table header

    Examples:
    | what |

  Scenario Outline: no steps, one example header

    Examples:
    | nope |
    | nada |
