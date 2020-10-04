@IsTest
private class TestRunAs {
    public static testMethod void testRunAs() {
        String uniqueUserName = 'tester' + DateTime.now().getTime() + '@testing.org';
        Profile standard = [SELECT Id FROM Profile WHERE Name = 'Standard User'];
        User testUser = new User();
        testUser.Alias = 'tester';
        testUser.Email = 'testUser123@example.com';
        testUser.EmailEncodingKey = 'UTF-8';
        testUser.LastName = 'Tester';
        testUser.LanguageLocaleKey = 'en_US';
        testUser.LocaleSidKey = 'en_US';
        testUser.ProfileId = standard.Id;
        testUser.TimeZoneSidKey = 'America/Los_Angeles';
        testUser.UserName = uniqueUserName;

        System.runAs(testUser) {
            System.debug('Current User: ' + UserInfo.getUserName());
            System.debug('Current Profile: ' + UserInfo.getProfileId());
        }
    }
}