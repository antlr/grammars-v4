import XCTest
@testable import Regex

final class StringExtensionTests: XCTestCase {
	func testReplacingFirstMatch() {
		XCTAssertEqual(
			"123foo456bar".replacingFirstMatch(of: Regex(#"[a-z]+"#), with: "🦄"),
			"123🦄456bar"
		)
	}

	func testReplacingAllMatches() {
		XCTAssertEqual(
			"123foo456bar".replacingAllMatches(of: Regex(#"[a-z]+"#), with: "🦄"),
			"123🦄456🦄"
		)
	}
}
