using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime.Tree;
using NUnit.Framework;

namespace pggrammartest
{
    internal class ParserTests
    {

        [TestCaseSource(nameof(GetValidTestCases),  Category = "ParserTests")]
        public void RunValidParserTests(string fname)
        {
            DoParseFile(fname);
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
                        Assert.Fail($"Found unparsed at {e.Line}:{e.Column} {e.Message}");
                    }
                }
            );
        }

        private static IEnumerable<TestCaseData> GetValidTestCases()
        {
            var files = Directory.GetFiles(@"parsertest", "*.sql");
            foreach (var file in files)
            {
                yield return new TestCaseData(file) { TestName = Path.GetFileNameWithoutExtension(file) };
            }
        }
    }
}