using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.Json;

public static class VsWhereJson
{
	public static string FindLatestClExe()
	{
		string vswhere = FindVsWhere();

		string json = Run(
				  vswhere,
				  "-products * " +
				  "-requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 " +
				  "-format json"
				 );

		using var doc = JsonDocument.Parse(json);

		var latest = doc.RootElement
			     .EnumerateArray()
			     .OrderByDescending(e =>
			Version.Parse(e.GetProperty("installationVersion").GetString()!))
			.First();

		string installPath = latest.GetProperty("installationPath").GetString()!;

		return ResolveClExe(installPath);
	}

	private static string ResolveClExe(string vsInstall)
	{
		var msvcRoot = Path.Combine(vsInstall, "VC", "Tools", "MSVC");

		var latestMsvc = Directory.EnumerateDirectories(msvcRoot)
				 .OrderByDescending(d => Version.Parse(Path.GetFileName(d)))
				 .First();

		return Path.Combine(latestMsvc, "bin", "Hostx64", "x64", "cl.exe");
	}

	private static string FindVsWhere()
	{
		var pf86 = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86);
		var path = Path.Combine(pf86, "Microsoft Visual Studio", "Installer", "vswhere.exe");
		if (File.Exists(path)) return path;
		throw new FileNotFoundException("vswhere.exe not found");
	}

	private static string Run(string exe, string args)
	{
		var psi = new ProcessStartInfo(exe, args)
		{
			RedirectStandardOutput = true,
			UseShellExecute = false,
			CreateNoWindow = true
		};
		using var p = Process.Start(psi)!;
		string output = p.StandardOutput.ReadToEnd();
		p.WaitForExit();
		return output;
	}
}
