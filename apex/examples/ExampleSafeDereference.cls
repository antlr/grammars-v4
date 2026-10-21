class ExampleSafeDereference {
    
    public void printTestString() {
        String testStr;
        System.debug(testStr?.toUpperCase());
    }
}