#include <iostream>
#include <string_view>
#include <cassert>
#include <utility>
#include <string>
#include <string.h>

#include "Exceptions.h"
#include "antlr4-common.h"
#include "misc/Interval.h"
#include "IntStream.h"

#include "support/Utf8.h"
#include "support/CPPUtils.h"

#include "EncodingInputStream.h"

EncodingInputStream::EncodingInputStream() {
  InitializeInstanceFields();
}

EncodingInputStream::EncodingInputStream(std::ifstream &stream, std::string file_encoding)
    : EncodingInputStream()
{
    load(stream, file_encoding);
}

uint32_t decode_utf8_char(std::istream& in) {
	unsigned char first = in.get();
	if (in.eof()) return 0;

	uint32_t codepoint = 0;
	if (first < 0x80) {
	// 1-byte ASCII
		codepoint = first;
	} else if ((first >> 5) == 0x6) {
	// 2-byte sequence
		unsigned char second = in.get();
		codepoint = ((first & 0x1F) << 6) | (second & 0x3F);
	} else if ((first >> 4) == 0xE) {
	// 3-byte sequence
		unsigned char second = in.get();
		unsigned char third = in.get();
		codepoint = ((first & 0x0F) << 12) |
			    ((second & 0x3F) << 6) |
			    (third & 0x3F);
	} else if ((first >> 3) == 0x1E) {
	// 4-byte sequence
		unsigned char second = in.get();
		unsigned char third = in.get();
		unsigned char fourth = in.get();
		codepoint = ((first & 0x07) << 18) |
			    ((second & 0x3F) << 12) |
			    ((third & 0x3F) << 6) |
			    (fourth & 0x3F);
	} else {
	// Invalid start byte
		codepoint = 0xFFFD; // Replacement character
	}

	return codepoint;
}

void EncodingInputStream::load(std::ifstream &stream, std::string file_encoding)
{
    if (!stream.good() || stream.eof()) // No fail, bad or EOF.
    {
	  return;
    }
    _data.clear();
    std::vector<uint32_t> vec;
    if (file_encoding == "latin1")
    {
	    char c;
	    while (stream.get(c)) {
		    vec.push_back(static_cast<uint32_t>(static_cast<unsigned char>(c)));
	    }
    } else {
	    while (stream.peek() != EOF) {
		    uint32_t codepoint = decode_utf8_char(stream);
		    vec.push_back(codepoint);
	    }
    }
    _data = vec;
    p = 0;
}

//void EncodingInputStream::load(std::istream &stream, bool lenient) {
//	if (!stream.good() || stream.eof()) // No fail, bad or EOF.
//	{
//		std::cout << "No good." << std::endl;
//		return;
//	}
//	_data.clear();
//	std::string s((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());
//	load(s.data(), s.length(), lenient);
//}

void EncodingInputStream::reset() {
  p = 0;
}

void EncodingInputStream::consume()
{
  if (p >= _data.size()) {
    assert(LA(1) == IntStream::EOF);
    throw antlr4::IllegalStateException("cannot consume EOF");
  }

  if (p < _data.size()) {
    p++;
  }
}

size_t EncodingInputStream::LA(ssize_t i) {
  if (i == 0) {
    return 0; // undefined
  }
  ssize_t position = static_cast<ssize_t>(p);
  if (i < 0) {
    i++; // e.g., translate LA(-1) to use offset i=0; then _data[p+0-1]
    if ((position + i - 1) < 0) {
      return IntStream::EOF; // invalid; no char before first char
    }
  }
  if ((position + i - 1) >= static_cast<ssize_t>(_data.size())) {
    return IntStream::EOF;
  }
  return _data[static_cast<size_t>((position + i - 1))];
}

size_t EncodingInputStream::LT(ssize_t i) {
  return LA(i);
}

size_t EncodingInputStream::index() {
  return p;
}

size_t EncodingInputStream::size() {
  return _data.size();
}

ssize_t EncodingInputStream::mark() {
  return -1;
}

void EncodingInputStream::release(ssize_t /* marker */) {
}

void EncodingInputStream::seek(size_t index) {
  if (index <= p) {
    p = index; // just jump; don't update stream state (line, ...)
    return;
  }
  // seek forward, consume until p hits index or n (whichever comes first)
  index = std::min(index, _data.size());
  while (p < index) {
    consume();
  }
}

std::string encode_utf8(const std::vector<uint32_t>& vec, size_t start, size_t end) {
	std::string result;

	if (start >= vec.size() || end > vec.size() || start >= end) {
	// Out of bounds, return empty string
		return result;
	}

	for (size_t i = start; i < end; ++i) {
		uint32_t cp = vec[i];

		if (cp <= 0x7F) {
	    // 1-byte
			result.push_back(static_cast<char>(cp));
		}
		else if (cp <= 0x7FF) {
	    // 2-byte
			result.push_back(static_cast<char>(0xC0 | (cp >> 6)));
			result.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
		}
		else if (cp <= 0xFFFF) {
	    // 3-byte
			result.push_back(static_cast<char>(0xE0 | (cp >> 12)));
			result.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
			result.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
		}
		else if (cp <= 0x10FFFF) {
	    // 4-byte
			result.push_back(static_cast<char>(0xF0 | (cp >> 18)));
			result.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
			result.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
			result.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
		}
		else {
	    // Invalid code point, you can insert replacement char U+FFFD if you want
			result.push_back(static_cast<char>(0xEF));
			result.push_back(static_cast<char>(0xBF));
			result.push_back(static_cast<char>(0xBD));
		}
	}

	return result;
}

std::string EncodingInputStream::getText(const antlr4::misc::Interval &interval)
{
  if (interval.a < 0 || interval.b < 0) {
    return "";
  }
  size_t start = static_cast<size_t>(interval.a);
  size_t stop = static_cast<size_t>(interval.b);
  if (stop >= _data.size()) {
    stop = _data.size() - 1;
  }
  size_t count = stop - start + 1;
  if (start >= _data.size()) {
    return "";
  }
  return encode_utf8(_data, start, stop + 1);
}

std::string EncodingInputStream::getSourceName() const {
  if (name.empty()) {
    return IntStream::UNKNOWN_SOURCE_NAME;
  }
  return name;
}

std::string EncodingInputStream::toString() const {
    return "";
}

void EncodingInputStream::InitializeInstanceFields() {
  p = 0;
}
