import 'dart:io';
import 'package:antlr4/antlr4.dart';

abstract class CLexerBase extends Lexer {
  CLexerBase(CharStream input) : super(runGccAndMakeStream(input));

  static CharStream runGccAndMakeStream(CharStream input) {
    var isWindows = Platform.isWindows;

    // Get options from command line args
    var args = Platform.executableArguments + Platform.script.pathSegments;

    var vsc = args.any((a) => a.toLowerCase().contains("--vsc"));
    var gcc = args.any((a) => a.toLowerCase().contains("--gcc"));
    var clang = args.any((a) => a.toLowerCase().contains("--clang"));
    var nopp = args.any((a) => a.toLowerCase().contains("--nopp"));

    if (!(vsc || gcc || clang)) {
      gcc = true;
    }

    // Extract preprocessor options (-D and -I)
    var ppOptions = extractPreprocessorOptions(args);

    // Get the source name from the CharStream
    var sourceName = input.sourceName;
    var inputText = input.getText(Interval(0, input.size - 1));

    // If source name is empty or not a .c file, we need to write to a temp file
    if (sourceName.isEmpty || !sourceName.endsWith(".c")) {
      // Create a temp file for preprocessing
      sourceName = "${Directory.systemTemp.path}/antlr4_temp_${DateTime.now().millisecondsSinceEpoch}.c";
      File(sourceName).writeAsStringSync(inputText);
    }

    var outputName = "$sourceName.p";

    if (nopp) {
      try {
        File(outputName).writeAsStringSync(inputText);
      } catch (e) {
        // Ignore
      }
      return InputStream.fromString(inputText);
    }

    if (gcc) {
      var output = "";
      try {
        var gccCommand = isWindows ? "gcc.exe" : "gcc";
        var gccArgs = ["-std=c2x", "-E", "-C"];
        gccArgs.addAll(ppOptions);
        gccArgs.add(sourceName);
        var result = Process.runSync(gccCommand, gccArgs);
        output = result.stdout as String;
      } catch (e) {
        // Failed to run gcc preprocessor
      }
      File(outputName).writeAsStringSync(output);
      return InputStream.fromString(output);
    }
    if (clang) {
      var output = "";
      try {
        var clangCommand = isWindows ? "clang.exe" : "clang";
        var clangArgs = ["-std=c2x", "-E", "-C"];
        clangArgs.addAll(ppOptions);
        clangArgs.add(sourceName);
        var result = Process.runSync(clangCommand, clangArgs);
        output = result.stdout as String;
      } catch (e) {
        // Failed to run clang preprocessor
      }
      File(outputName).writeAsStringSync(output);
      return InputStream.fromString(output);
    }

    throw Exception("No preprocessor specified.");
  }

  static List<String> extractPreprocessorOptions(List<String> args) {
    var options = <String>[];
    for (var arg in args) {
      // Match --D and --I options
      if (arg.startsWith("--D")) {
        options.add("-D${arg.substring(3)}");
      }
      else if (arg.startsWith("--I")) {
        options.add("-I${arg.substring(3)}");
      }
    }
    return options;
  }
}
