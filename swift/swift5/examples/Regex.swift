import Foundation

/**
Create a regular expression from a pattern string and options.

```
import Regex

let regex = Regex(#"^[a-z]+$"#, options: .caseInsensitive)

regex.isMatched(by: "Unicorn")
//=> true
```
*/
public struct Regex: Hashable {
	let nsRegex: NSRegularExpression

	// MARK: Initializers

	/**
	Create a `Regex` from a static pattern string and options.

	Tip: Wrap the pattern string in `#` to reduce the need for escaping. For example: `#"\d+"#`.

	[Supported regex syntax.](https://developer.apple.com/documentation/foundation/nsregularexpression#1661061)
	*/
	public init(
		_ pattern: StaticString,
		options: Options = [],
		file: StaticString = #fileID,
		line: Int = #line
	) {
		do {
			try self.init(pattern.string, options: options)
		} catch {
			fatalError("Invalid regular expression: \(error.localizedDescription)", file: file, line: UInt(line))
		}
	}

	/**
	Create a `Regex` from a pattern string and options.

	Tip: Wrap the pattern string in `#` to reduce the need for escaping. For example: `#"\d+"#`.

	[Supported regex syntax.](https://developer.apple.com/documentation/foundation/nsregularexpression#1661061)
	*/
	@_disfavoredOverload
	public init(
		_ pattern: String,
		options: Options = []
	) throws {
		self.init(
			try NSRegularExpression(pattern: pattern, options: options)
		)
	}

	/**
	Create a `Regex` from a `NSRegularExpression`.
	*/
	@_disfavoredOverload
	public init(_ regularExpression: NSRegularExpression) {
		self.nsRegex = regularExpression
	}
}

// MARK: Methods

extension Regex {
	/**
	Returns whether there is a match in the given string.

	```
	import Regex

	Regex(#"^\d+$"#).isMatched(by: "123")
	//=> true
	```
	*/
	public func isMatched(by string: String) -> Bool {
		firstMatch(in: string) != nil
	}

	/**
	Returns the first match in the given string.

	```
	import Regex

	Regex(#"\d+"#).firstMatch(in: "123-456")?.value
	//=> "123"
	```
	*/
	public func firstMatch(in string: String) -> Match? {
		nsRegex.firstMatch(in: string).map {
			Match(checkingResult: $0, string: string)
		}
	}

	/**
	Returns all the matches in the given string.

	```
	import Regex

	Regex(#"\d+"#).allMatches(in: "123-456").map(\.value)
	//=> ["123", "456"]
	```
	*/
	public func allMatches(in string: String) -> [Match] {
		nsRegex.matches(in: string).map {
			Match(checkingResult: $0, string: string)
		}
	}
}

// TODO: This needs to include the options too.
//extension Regex: CustomStringConvertible {
//	public var description: String { regex.pattern }
//}

extension Regex {
	// MARK: Properties

	/**
	The regular expression pattern.
	*/
	public var pattern: String { nsRegex.pattern }

	/**
	The regular expression options.
	*/
	public var options: Options { nsRegex.options }
}

// MARK: Types

extension Regex {
	public typealias Options = NSRegularExpression.Options
	public typealias MatchingOptions = NSRegularExpression.MatchingOptions

	/**
	A regex match.
	*/
	public struct Match: Hashable {
		/**
		A regex match capture group.
		*/
		public struct Group: Hashable {
			/**
			The  capture group string.
			*/
			public let value: String

			/**
			The range of the capture group string in the original string.
			*/
			public let range: Range<String.Index>

			fileprivate init(originalString: String, range: NSRange) {
				self.range = Range(range, in: originalString)!
				self.value = String(originalString[self.range])
			}
		}

		fileprivate let originalString: String
		let checkingResult: NSTextCheckingResult

		/**
		The matched string.
		*/
		public let value: String

		/**
		The range of the matched string in the original string.
		*/
		public let range: Range<String.Index>

		/**
		All the match groups.
		*/
		public let groups: [Group]

		/**
		Get a match group by its name.

		```
		import Regex

		Regex(#"(?<number>\d+)"#).firstMatch(in: "1a-2b")?.group(named: "number")?.value
		//=> "1"
		```
		*/
		public func group(named name: String) -> Group? {
			let range = checkingResult.range(withName: name)

			guard range.length > 0 else {
				return nil
			}

			return Group(originalString: originalString, range: range)
		}

		fileprivate init(checkingResult: NSTextCheckingResult, string: String) {
			self.checkingResult = checkingResult
			self.originalString = string
			self.value = string[nsRange: checkingResult.range]!.string
			self.range = Range(checkingResult.range, in: string)!

			// The first range is the full range, so we ignore that.
			self.groups = (1..<checkingResult.numberOfRanges).map {
				let range = checkingResult.range(at: $0)
				return Group(originalString: string, range: range)
			}
		}
	}
}

// MARK: Operators

extension Regex {
	/**
	Enables using a regex for pattern matching.

	```
	import Regex

	switch "foo123" {
	case Regex(#"^foo\d+$"#):
		print("Match!")
	default:
		break
	}
	```
	*/
	public static func ~= (string: String, regex: Self) -> Bool {
		regex.isMatched(by: string)
	}

	/**
	Enables using a regex for pattern matching.

	```
	import Regex

	switch Regex(#"^foo\d+$"#) {
	case "foo123":
		print("Match!")
	default:
		break
	}
	```
	*/
	public static func ~= (regex: Self, string: String) -> Bool {
		regex.isMatched(by: string)
	}
}

// MARK: Helpers

extension Regex {
	/**
	Returns a string by adding backslash escapes as necessary to protect any characters that would match as pattern metacharacters.
	*/
	public static func escapingPattern(for string: String) -> String {
		NSRegularExpression.escapedPattern(for: string)
	}
}
