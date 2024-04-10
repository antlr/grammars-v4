// Test that Javascript keywords aren't matched in JSX children
function TestJavascriptKeywordInJsxChild(props) {
   return (<div>from</div>);
}

// Test that the regular expressions /.../ does not match JSX elements
// See https://github.com/antlr/grammars-v4/issues/3925
function TestNestedOneLineDivs() {
    return (<div><div></div></div>);
}

function TestHtmlTextWithApostrophe(props) {
 return (<p>string'.</p>);
}

function TestJsxAttributeValue(props) {
   return (<div><span attribute=">hello" attribute2='there'>from here</span></div>);
}

function TestJsxAttributeExpressionValue(props) {
   return (<div><span attribute={run()}>from here</span></div>);
}

function TestJsxhildrenOfTextAndExpressions(props) {
   return (<div><span>from {here} to {there} distance { here - there }</span></div>);
}

function TestJsxOpeningElementWithComment(props) {
 return (<Component
           attribute="5"> // Should this be 6?
        </Component>);
}

