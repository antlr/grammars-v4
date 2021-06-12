import Foundation

extension String {
	/**
	Returns a new string where the first match is replaced with the template.

	You can use template variables like `$` and `$0` in the template. [More info.](https://developer.apple.com/documentation/foundation/nsregularexpression#1965591)

	```
	import Regex

	"123🦄456".replacingFirstMatch(of: Regex(#"\d+"#), with: "")
	//=> "🦄456"
	```
	*/
	public func replacingFirstMatch(
		of regex: Regex,
		with template: String
	) -> Self {
		guard let match = regex.firstMatch(in: self) else {
			return self
		}

		let replacement = regex
			.nsRegex
			.replacementString(
				for: match.checkingResult,
				in: self,
				offset: 0,
				template: template
			)

		return replacingSubrange(match.range, with: replacement)
	}

	/**
	Returns a new string where the first match is replaced with the template.

	You can use template variables like `$` and `$0` in the template. [More info.](https://developer.apple.com/documentation/foundation/nsregularexpression#1965591)

	```
	import Regex

	"123🦄456".replacingFirstMatch(of: #"\d+"#, with: "")
	//=> "🦄456"
	```
	*/
	public func replacingFirstMatch(
		of regexPattern: StaticString,
		with template: String
	) -> Self {
		replacingFirstMatch(of: Regex(regexPattern), with: template)
	}

	/**
	Returns a new string where all matches are replaced with the template.

	You can use template variables like `$` and `$0` in the template. [More info.](https://developer.apple.com/documentation/foundation/nsregularexpression#1965591)

	```
	import Regex

	"123🦄456".replacingAllMatches(of: Regex(#"\d+"#), with: "")
	//=> "🦄"
	```
	*/
	public func replacingAllMatches(
		of regex: Regex,
		with template: String,
		options: Regex.MatchingOptions = []
	) -> Self {
		regex.nsRegex.stringByReplacingMatches(
			in: self,
			options: options,
			range: nsRange,
			withTemplate: template
		)
	}

	/**
	Returns a new string where all matches are replaced with the template.

	You can use template variables like `$` and `$0` in the template. [More info.](https://developer.apple.com/documentation/foundation/nsregularexpression#1965591)

	```
	import Regex

	"123🦄456".replacingAllMatches(of: #"\d+"#, with: "")
	//=> "🦄"
	```
	*/
	public func replacingAllMatches(
		of regexPattern: StaticString,
		with template: String,
		options: Regex.MatchingOptions = []
	) -> Self {
		replacingAllMatches(of: Regex(regexPattern), with: template, options: options)
	}
}
