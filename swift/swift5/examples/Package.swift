// swift-tools-version:5.4
import PackageDescription

let package = Package(
	name: "Regex",
	platforms: [
		.macOS(.v10_13),
		.iOS(.v11),
		.tvOS(.v11),
		.watchOS(.v4)
	],
	products: [
		.library(
			name: "Regex",
			targets: [
				"Regex"
			]
		)
	],
	targets: [
		.target(
			name: "Regex"
		),
		.testTarget(
			name: "RegexTests",
			dependencies: [
				"Regex"
			]
		)
	]
)
