package org.antlr.mojo.antlr4test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

public class AssertErrorsErrorListener extends BaseErrorListener {

    protected static final String CHAR_CR_PLACEHOLDER_REGEXP = "\\\\r"; // to match \r
    protected static final String CHAR_CR = String.valueOf((char)13);
    protected static final String CHAR_LF_PLACEHOLDER_REGEXP = "\\\\n"; // to match \n
    protected static final String CHAR_LF = String.valueOf((char)10);

    protected static final String LITERAL_BACKSLASH_R_PLACEHOLDER = "literal-backslash-r";
    protected static final String LITERAL_BACKSLASH_R = "\\\\r";
    protected static final String LITERAL_BACKSLASH_N_PLACEHOLDER = "literal-backslash-n";
    protected static final String LITERAL_BACKSLASH_N = "\\\\n";

    protected List<String> errorMessages = new ArrayList<String>();

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol
            , int line, int charPositionInLine, String msg, RecognitionException e) {
        final String errorMessage = String.format("line %d:%d %s", line, charPositionInLine, msg);
        errorMessages.add(errorMessage);
    }

    public void assertErrors(File errorMessagesFile) throws AssertErrorsException {
        if (!errorMessages.isEmpty()) {
            List<String> expectedErrorMessages = null;
            try {
                expectedErrorMessages = FileUtil.getNonEmptyLines(errorMessagesFile);
            } catch (FileNotFoundException ex) {
                throw new AssertErrorsException(String.format("found %d errors, but missing file %s",
                        errorMessages.size(), errorMessagesFile.getName()), ex);
            } catch (IOException ex) {
                throw new AssertErrorsException(String.format("found %d errors, unable to read file %s",
                        errorMessages.size(), errorMessagesFile.getName()), ex);
            }
            expectedErrorMessages = replacePlaceholders(expectedErrorMessages);

            final Object[] expectedErrors = expectedErrorMessages.toArray();
            final Object[] errors = errorMessages.toArray();
            if (expectedErrors.length != errors.length) {
                throw new AssertErrorsException(String.format("%s : expected %d errors, but was %d errors",
                        errorMessagesFile.getName(), expectedErrors.length, errors.length));
            }
            for (int i = 0; i < expectedErrors.length; i = i + 1) {
                final Object expectedError = expectedErrors[i];
                final Object error = errors[i];
                if (!expectedError.equals(error)) {
                    throw new AssertErrorsException(String.format("%s : expected (%s), but was (%s)",
                            errorMessagesFile.getName(), expectedError, error));
                }
            }
        }
        else
        {
            if (errorMessagesFile.exists()) {
                throw new AssertErrorsException(String.format("no errors found, but errors file exists %s",
                        errorMessagesFile.getAbsolutePath()));
            }
        }
    }

    protected List<String> replacePlaceholders(List<String> stringList) {
        List<String> replacedStringList = new ArrayList<String>();
        for (String vString : stringList) {
            replacedStringList.add(replacePlaceholders(vString));
        }
        return replacedStringList;
    }

    protected String replacePlaceholders(String pString) {
        String result = pString;
        if (result != null) {
            result = result.replaceAll(CHAR_CR_PLACEHOLDER_REGEXP, CHAR_CR);
            result = result.replaceAll(CHAR_LF_PLACEHOLDER_REGEXP, CHAR_LF);
            result = result.replaceAll(LITERAL_BACKSLASH_R_PLACEHOLDER, LITERAL_BACKSLASH_R);
            result = result.replaceAll(LITERAL_BACKSLASH_N_PLACEHOLDER, LITERAL_BACKSLASH_N);
        }
        return result;
    }
}
