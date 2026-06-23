Feature: Example tokens everywhere

  Scenario Outline: the <one>
    Given the <two>:
      """
      <three>
      """
    Given the <four>:
      | <five> |

    Examples:
      | one | two  | three | four   | five  |
      | un  | deux | trois | quatre | cinq  |
      | uno | dos  | tres  | quatro | cinco |
