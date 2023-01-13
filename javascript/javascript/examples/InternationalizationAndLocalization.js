"use strict";
//------------------------------------------------------------------------------
// Collation
// Sorting a set of strings and searching within a set of strings. Collation is
//   parameterized by locale and aware of Unicode.
// http://es6-features.org/#Collation
//------------------------------------------------------------------------------

// in German,  "ä" sorts with "a"
// in Swedish, "ä" sorts after "z"
var list = [ "ä", "a", "z" ]
var l10nDE = new Intl.Collator("de")
var l10nSV = new Intl.Collator("sv")
l10nDE.compare("ä", "z") === -1
l10nSV.compare("ä", "z") === +1
console.log(list.sort(l10nDE.compare)) // [ "a", "ä", "z" ]
console.log(list.sort(l10nSV.compare)) // [ "a", "z", "ä" ]

//------------------------------------------------------------------------------
// Number Formatting
// Format numbers with digit grouping and localized separators.
// http://es6-features.org/#NumberFormatting
//------------------------------------------------------------------------------

var l10nEN = new Intl.NumberFormat("en-US")
var l10nDE = new Intl.NumberFormat("de-DE")
l10nEN.format(1234567.89) === "1,234,567.89"
l10nDE.format(1234567.89) === "1.234.567,89"

//------------------------------------------------------------------------------
// Currency Formatting
// Format numbers with digit grouping, localized separators and attached currency symbol.
// http://es6-features.org/#CurrencyFormatting
//------------------------------------------------------------------------------

var l10nUSD = new Intl.NumberFormat("en-US", { style: "currency", currency: "USD" })
var l10nGBP = new Intl.NumberFormat("en-GB", { style: "currency", currency: "GBP" })
var l10nEUR = new Intl.NumberFormat("de-DE", { style: "currency", currency: "EUR" })
l10nUSD.format(100200300.40) === "$100,200,300.40"
l10nGBP.format(100200300.40) === "£100,200,300.40"
l10nEUR.format(100200300.40) === "100.200.300,40 €"

//------------------------------------------------------------------------------
// Date/Time Formatting
// Format date/time with localized ordering and separators.
// http://es6-features.org/#DateTimeFormatting
//------------------------------------------------------------------------------

var l10nEN = new Intl.DateTimeFormat("en-US")
var l10nDE = new Intl.DateTimeFormat("de-DE")
l10nEN.format(new Date("2015-01-02")) === "1/2/2015"
l10nDE.format(new Date("2015-01-02")) === "2.1.2015"