Feature: Scenario Outline with a docstring

Scenario Outline: Greetings come in many forms
    Given this file:
    """<type>
    Greeting:<content>
    """

Examples:
  | type  | content |
  | en    | Hello   |
  | fr    | Bonjour |
