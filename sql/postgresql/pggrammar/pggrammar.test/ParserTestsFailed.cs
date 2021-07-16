using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime.Tree;
using NUnit.Framework;

namespace pggrammartest
{
    internal class FailedParserTests
    {
    
        [TestCaseSource(nameof(GetFailedTestCases), Category = "ParserTests")]
        public void RunFailedParserTests(string fname)
        {
            DoParseFile(fname);
        }

        private static IEnumerable<TestCaseData> GetFailedTestCases()
        {
            var files = Directory.GetFiles(@"parsertest.failed", "*.sql");
            foreach (var file in files)
            {
                yield return new TestCaseData(file) { TestName = Path.GetFileNameWithoutExtension(file) };
            }
        }

        private static void DoParseFile(string fname)
        {
            string text = File.ReadAllText(fname);
            var grammar = pggrammar.getpggrammar(text);
            IParseTree tree = grammar.Root;
            Assert.Multiple(() =>
                {
                    foreach (var e in grammar.ParseErrors)
                    {
                        Assert.Fail($"Found unparsed at {e.Line}:{e.Column}");
                    }
                }
            );
        }

    }
}