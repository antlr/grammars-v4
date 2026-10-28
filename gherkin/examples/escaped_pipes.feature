Feature: Escaped pipes
    The \-character will be considered as an escape in table cell
    iff it is followed by a |-character, a \-character or an n.

  Scenario: They are the future
    Given they have arrived
      | æ | o |
      | a | ø |
    Given they have arrived
      | \|æ\\n     | \o\no\  |
      | \\\|a\\\\n | ø\\\nø\\|