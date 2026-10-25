import XCTest
@testable import Regex

final class RegexTests: XCTestCase {
	func testInit() throws {
		_ = Regex(#"\d+"#)

		let regex = #"\d+"#
		_ = try Regex(regex)
	}

	func testEquality() {
		XCTAssertEqual(Regex(#"\d+"#), Regex(#"\d+"#))
		XCTAssertEqual(Regex(#"\d+"#, options: .caseInsensitive), Regex(#"\d+"#, options: .caseInsensitive))
		XCTAssertNotEqual(Regex(#"\d+"#, options: .caseInsensitive), Regex(#"\d+"#))
	}

	func testIsMatched() {
		XCTAssertTrue(Regex(#"^\d+$"#).isMatched(by: "123"))
		XCTAssertFalse(Regex(#"^\d+$"#).isMatched(by: "foo"))
	}

	func testFirstMatch() {
		XCTAssertEqual(
			Regex(#"\d+"#).firstMatch(in: "123-456")?.value,
			"123"
		)
	}

	func testAllMatches() {
		XCTAssertEqual(
			Regex(#"\d+"#).allMatches(in: "123-456").map(\.value),
			["123", "456"]
		)
	}

	func testMatchRange() {
		let string = "foo-456"
		let match = Regex(#"\d+"#).firstMatch(in: string)!

		XCTAssertEqual(
			String(string[match.range]),
			"456"
		)
	}


	func testMatchGroup() {
		XCTAssertEqual(
			Regex(#"(foo)(bar)"#).firstMatch(in: "-foobar-")?.groups.map(\.value),
			["foo", "bar"]
		)

		XCTAssertEqual(
			Regex(#"(?<number>\d+)"#).firstMatch(in: "1a-2b")?.group(named: "number")?.value,
			"1"
		)
	}

	func testMatchGroupRange() {
		let string = "foo-456"
		let groups = Regex(#"([a-z]+)-(\d+)"#).firstMatch(in: string)!.groups

		XCTAssertEqual(
			string[groups[0].range],
			"foo"
		)

		XCTAssertEqual(
			string[groups[1].range],
			"456"
		)
	}

	func testPatternMatching() {
		XCTAssertTrue(Regex(#"^foo\d+$"#) ~= "foo123")
		XCTAssertTrue("foo123" ~= Regex(#"^foo\d+$"#))
	}

	func testMultilineOption() {
		let regex = Regex(
			#"""
			^
			[a-z]+  # Match the word
			\d+     # Match the number
			$
			"""#,
			options: .allowCommentsAndWhitespace
		)

		XCTAssertTrue(regex.isMatched(by: "foo123"))
	}
}
