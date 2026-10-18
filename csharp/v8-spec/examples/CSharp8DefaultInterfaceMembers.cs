// CSharp8DefaultInterfaceMembers.cs
// Tests: default interface method implementations

using System;

public interface ILogger
{
    void Log(string message);

    // Default implementation
    void LogError(string message) => Log($"[ERROR] {message}");
    void LogWarning(string message) => Log($"[WARN]  {message}");
    void LogInfo(string message)  => Log($"[INFO]  {message}");

    // Default static member
    static string FormatTimestamp(DateTime dt) => dt.ToString("HH:mm:ss");
}

public interface IFormattedLogger : ILogger
{
    // Override default from base interface
    void ILogger.LogError(string message) =>
        Log($"[ERROR] {ILogger.FormatTimestamp(DateTime.Now)} {message}");
}

public class ConsoleLogger : ILogger
{
    public void Log(string message) => Console.WriteLine(message);
    // LogError, LogWarning, LogInfo are inherited from the interface defaults
}

public class TimestampedLogger : IFormattedLogger
{
    public void Log(string message) =>
        Console.WriteLine($"{ILogger.FormatTimestamp(DateTime.Now)} {message}");
}

public class CSharp8DefaultInterfaceMembers
{
    static void Main()
    {
        ILogger a = new ConsoleLogger();
        a.LogInfo("server started");
        a.LogWarning("low memory");
        a.LogError("disk full");

        Console.WriteLine();

        ILogger b = new TimestampedLogger();
        b.LogInfo("ready");
        b.LogError("connection lost");
    }
}
