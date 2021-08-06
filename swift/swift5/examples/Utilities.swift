import Foundation


extension StringProtocol {
	var string: String { String(self) }
}


extension StaticString {
	var string: String { "\(self)" }
}


extension String {
	var range: Range<Index> { startIndex..<endIndex }
	var nsRange: NSRange { NSRange(location: 0, length: utf16.count) }

	subscript(nsRange nsRange: NSRange) -> Substring? {
		guard let range = Range(nsRange, in: self) else {
			return nil
		}

		return self[range]
	}
}


/// Convenience wrappers that make the `range` parameter optional and type-safe.
extension NSRegularExpression {
	/// Returns an array containing all the matches of the regular expression in the string.
	func matches(
		in string: String,
		options: MatchingOptions = [],
		range: Range<String.Index>? = nil
	) -> [NSTextCheckingResult] {
		matches(
			in: string,
			options: options,
			range: NSRange(range ?? string.range, in: string)
		)
	}

	/// Returns the first match of the regular expression within the given range of the string.
	func firstMatch(
		in string: String,
		options: MatchingOptions = [],
		range: Range<String.Index>? = nil
	) -> NSTextCheckingResult? {
		firstMatch(
			in: string,
			options: options,
			range: NSRange(range ?? string.range, in: string)
		)
	}
}


extension RangeReplaceableCollection {
	/**
	Returns a new collection by replacing the given subrange with the new elements.
	*/
	func replacingSubrange<C>(_ subrange: Range<Index>, with newElements: C) -> Self where C: Collection, Element == C.Element {
		var copy = self
		copy.replaceSubrange(subrange, with: newElements)
		return copy
	}
}
