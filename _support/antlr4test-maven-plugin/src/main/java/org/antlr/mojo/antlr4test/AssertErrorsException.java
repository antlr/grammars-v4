package org.antlr.mojo.antlr4test;

public class AssertErrorsException extends Exception {

    public AssertErrorsException(String message) {
        super(message);
    }

    public AssertErrorsException(String message, Throwable cause) {
        super(message, cause);
    }

}
