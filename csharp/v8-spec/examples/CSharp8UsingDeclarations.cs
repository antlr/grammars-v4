// CSharp8UsingDeclarations.cs
// Tests: using declarations (non-block using var)

using System;
using System.IO;
using System.Text;

public class CSharp8UsingDeclarations
{
    static string ReadAll(string path)
    {
        using var reader = new StreamReader(path, Encoding.UTF8);
        return reader.ReadToEnd();
        // reader disposed here at end of enclosing scope
    }

    static void WriteLines(string path, string[] lines)
    {
        using var writer = new StreamWriter(path, append: false, Encoding.UTF8);
        foreach (var line in lines)
            writer.WriteLine(line);
        // writer disposed here
    }

    static (int lines, int chars) Analyse(string path)
    {
        using var fs = new FileStream(path, FileMode.Open, FileAccess.Read);
        using var sr = new StreamReader(fs);

        int lineCount = 0, charCount = 0;
        string line;
        while ((line = sr.ReadLine()) != null)
        {
            lineCount++;
            charCount += line.Length;
        }
        return (lineCount, charCount);
        // sr and fs both disposed here, in reverse declaration order
    }

    static void Main()
    {
        string tmp = Path.GetTempFileName();
        WriteLines(tmp, new[] { "hello", "world", "from C# 8" });

        var (lines, chars) = Analyse(tmp);
        Console.WriteLine($"lines={lines} chars={chars}");

        string content = ReadAll(tmp);
        Console.Write(content);

        File.Delete(tmp);
    }
}
