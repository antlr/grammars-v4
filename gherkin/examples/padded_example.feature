Feature: test

  Scenario: test
    Given a <color> ball with:
      | type     | diameter |
      | football |       69 |
      |   pool   |      5.6 |


    # The "red" cell below has the following whitespace characters on each side:
    # - U+00A0 (non-breaking space)
    # - U+0020 (space)
    # - U+0009 (tab)
    # This is generated with `ruby -e 'STDOUT.write "\u00A0\u0020\u0009".encode("utf-8")' | pbcopy`
    # and pasted. 
    Examples:
      | color   |
      |  	red  	|
