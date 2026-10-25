
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;

public abstract class PythonParserBase extends Parser
{
    public PythonVersion Version = PythonVersion.Autodetect;

    protected PythonParserBase(TokenStream input) {
        super(input);
    }

    protected boolean CheckVersion(int version) {
        return Version == PythonVersion.Autodetect || version == Version.getValue();
    }

    protected void SetVersion(int requiredVersion) {
        if (requiredVersion == 2) {
            Version = PythonVersion.Python2;
        } else if (requiredVersion == 3) {
            Version = PythonVersion.Python3;
        }
    }
}

