-- Removing the ALL rule from the rule cache and DBC.dbqlrulestbl
END QUERY LOGGING ON ALL;

-- Remove all DBQL rules in effect from DBC.DBQLRuleTbl
END QUERY LOGGING ON ALL RULES;

-- Specify an account explicitly to discontinue request logging
END QUERY LOGGING ON user_1 ACCOUNT = ('order_entry');

-- Removing the rule for user_1 and user_2 from the rule cache and DBC.DBQLRulesTbl
END QUERY LOGGING ON user_1, user_2;
