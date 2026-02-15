using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Trash;

public class MyATN
{
	public MyATN(Parser parser, MyATNState[] start, HashSet<MyATNState> stop, HashSet<MyATNState> all_states)
	{
        this.Parser = parser;
		this.start = start;
        this.stop = stop;
		this.all_states = all_states;
    }

    private Parser Parser;
    public MyATNState[] start;
    public HashSet<MyATNState> stop;
    public int MaxTokenType { get; private set; }
    public HashSet<MyATNState> all_states { get; private set; }

    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine("digraph ATN {");
        sb.AppendLine("rankdir=LR;");
        // Output dot representation.
        foreach (var n in this.stop)
        {
            sb.AppendLine("s" + n.stateNumber + "[fontsize=11,label=\"" +
                          n.stateNumber + "\", shape=doublecircle, fixedsize=true, witth=.6];");

        }
        foreach (var n in this.start)
        {
            if (this.stop.Contains(n)) continue;
            sb.AppendLine("s" + n.stateNumber + "[fontsize=11,label=\"" +
                          n.stateNumber + "\", shape=circle, fixedsize=true, witth=.6];");
        }
        foreach (var n in this.all_states)
        {
            if (this.stop.Contains(n)) continue;
            if (this.start.Contains(n)) continue;
            sb.AppendLine("s" + n.stateNumber + "[fontsize=11,label=\"" +
                          n.stateNumber + "\", shape=circle, fixedsize=true, witth=.6];");
        }

        foreach (var s in this.all_states)
        {
            foreach (var tr in s.TransitionsArray)
            {
                sb.Append("s" + s.stateNumber + " -> ");
                sb.Append("s" + tr.target.stateNumber);
                switch (tr)
                {
                    case MyActionTransition act:
                        {
                            sb.Append("[fontsize=11, fontname=\"Courier\", arrowsize=.7, label = \"action " + act.ruleIndex + ":" + act.actionIndex + "\", arrowhead = normal]");
                            break;
                        }
                    case MyAtomTransition ato:
                        {
                            var t = ato.token;
                            var dn = this.Parser.Vocabulary.GetDisplayName(t);
                            sb.Append("[fontsize=11, fontname=\"Courier\", arrowsize=.7, label = \"" + dn + "/" + ato.token + "\", arrowhead = normal]");
                            break;
                        }
                    case MyEpsilonTransition _:
                        break;
                    case MyRangeTransition r:
                        {
                            //r.from, r.to;
                            break;
                        }
                    case MyRuleTransition rul:
                        {
                            var name = this.Parser.RuleNames[rul.ruleIndex];
                            sb.Append(
                                "[fontsize=11, fontname=\"Courier\", arrowsize=.7, label = \"<" + name + ">\", arrowhead = normal]");
                            break;
                        }
                    case MySetTransition st:
                        {
                            var items = st.Label.ToString(Parser.Vocabulary);
                            sb.Append(
                                "[fontsize= 11, fontname = \"Courier\", arrowsize=.7, label=\"" + items + "\", arrowhead = normal]");
                            break;
                        }
                    case MyWildcardTransition _:
                        {
                            sb.Append(
                                "[fontsize= 11, fontname = \"Courier\", arrowsize=.7, label=\".\", arrowhead = normal]");
                            break;
                        }
                    case MyPrecedencePredicateTransition ppt:
                        {
                            sb.Append(
                                "[fontsize= 11, fontname = \"Courier\", arrowsize=.7, label=\"" + ppt.ToString() + "\", arrowhead = normal]");
                            break;
                        }
                    case MyPredicateTransition pred:
                        {
                            //pred.ruleIndex, pred.predIndex, pred.isCtxDependent;
                            break;
                        }
                    default:
                        break;

                }

                sb.AppendLine(";");
            }
        }

        sb.AppendLine("}");
        return sb.ToString();
    }
}

