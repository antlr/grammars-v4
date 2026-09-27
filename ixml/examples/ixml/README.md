# ixml

<h1>Invisible XML</h1>

<p>Data is an abstraction: there is no essential difference between the JSON</p>
<pre>{"temperature": {"scale": "C", "value": 21}}</pre>

<p>and an equivalent XML</p>
<pre>&lt;temperature scale="C" value="21"/&gt;</pre>

<p>or</p>
<pre>&lt;temperature&gt;
   &lt;scale&gt;C&lt;/scale&gt;
   &lt;value&gt;21&lt;/value&gt;
&lt;/temperature&gt;</pre>

<p>since the underlying abstractions being represented are the same. </p>

<p>We choose which representations of our data to use, JSON, CSV, XML, or
whatever, depending on habit, convenience, or the context we want to use that
data in. On the other hand, having an interoperable generic toolchain such as
that provided by XML to process data is of immense value. How do we resolve the
conflicting requirements of convenience, habit, and context, and still enable a
generic toolchain? </p>

<p>Invisible XML (ixml) is a method for treating non-XML documents as if they
were XML, enabling authors to write documents and data in a format they prefer
while providing XML for processes that are more effective with XML content. For
example, it can turn CSS code like</p>
<pre>body {color: blue; font-weight: bold}</pre>

<p>into XML like</p>
<pre>&lt;css&gt;
   &lt;rule&gt;
      &lt;simple-selector name="body"/&gt;
      &lt;block&gt;
         &lt;property&gt;
            &lt;name&gt;color&lt;/name&gt;
            &lt;value&gt;blue&lt;/value&gt;
         &lt;/property&gt;
         &lt;property&gt;
            &lt;name&gt;font-weight&lt;/name&gt;
            &lt;value&gt;bold&lt;/value&gt;
         &lt;/property&gt;
      &lt;/block&gt;
   &lt;/rule&gt;
&lt;/css&gt;</pre>

<p>or</p>
<pre>&lt;css&gt;
   &lt;rule&gt;
      &lt;selector&gt;body&lt;/selector&gt;
      &lt;block&gt;
         &lt;property name="color" value="blue"/&gt;
         &lt;property name="font-weight" value="bold"/&gt;
      &lt;/block&gt;
   &lt;/rule&gt;
&lt;/css&gt;</pre>

<p>depending on choice.</p>

<p>This is an ongoing project to provide software that lets you treat any
parsable format as if it were XML, without the need for markup. </p>

<h2>Resources</h2>
<ul>
<li>The (draft) <a href="https://invisiblexml.org/1.0/">Specification for Invisible XML</a> formally describes the language.</li>
<li>The <a href="http://www.cwi.nl/~steven/ixml/tutorial/">hands-on tutorial</a> shows you how to use it.</li>
<li>The ixml grammar <a href="https://invisiblexml.org/1.0/ixml.ixml.html">in ixml serialization</a>.</li>
<li>The ixml grammar <a href="https://invisiblexml.org/1.0/ixml.xml.html">in XML serialization</a>.</li>
</ul>

<p>There are five papers on the development:</p>
<ul>
  <li><a
    href="http://www.cwi.nl/~steven/Talks/2013/08-07-invisible-xml/invisible-xml-3.html">Invisible
    XML</a>: Introduces the concepts, and develops a notation to support them.
  </li>
  <li><a href="http://www.cwi.nl/~steven/Talks/2016/02-12-prague/data.html">Data just wants to be
    (format) neutral</a>:
    Discusses issues with automatic serialisation, and the relationship
    between Invisible XML grammars and data schemas.
  </li>
  <li><a href="http://www.cwi.nl/~steven/Talks/2016/06-05-london/xml-london.html">Parse Earley, Parse
    Often: How to parse anything to XML</a>:
    Discusses issues around grammar design, and in particular parsing
    algorithms used to recognise any document, and converting the resultant
    parse-tree into XML, and gives a new perspective on a classic algorithm.
  </li>
  <li><a
    href="http://archive.xmlprague.cz/2017/files/xmlprague-2017-proceedings.pdf#page=155">On
    the Descriptions of Data: The Usability of Notations</a>:
    Discusses changes to the design following experience with using it,
    giving examples of its use to develop data descriptions, and in passing,
    suggests other output formats.
  </li>
  <li><a
    href="https://archive.xmlprague.cz/2019/files/xmlprague-2019-proceedings.pdf#page=425">On
    the Specification of Invisible XML</a>:
    Describes decisions made during the production of the specification of
    ixml.
  </li>
</ul>

<p>Software to support ixml will be made available at a later date, but it
      is already usable via a link; see <a href="http://www.cwi.nl/~steven/ixml/tutorial/">the tutorial</a> for details.</p>
