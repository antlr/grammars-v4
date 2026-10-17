#pragma once

#include <string>
#include <cstddef>
#include <string_view>

#include "antlr4-common.h"
#include "misc/Interval.h"
#include "CharStream.h"

class EncodingInputStream : public antlr4::CharStream {
  protected:
    std::vector<int32_t> _data;
    size_t p;

  public:
    std::string name;
    EncodingInputStream();
//    EncodingInputStream(std::string_view input);
//    EncodingInputStream(const char *data, size_t length);
    EncodingInputStream(std::ifstream &stream, std::string file_encoding);
    virtual void reset();
    void consume() override;
    size_t LA(ssize_t i) override;
    virtual size_t LT(ssize_t i);
    size_t index() override;
    size_t size() override;
    ssize_t mark() override;
    void release(ssize_t marker) override;
    void seek(size_t index) override;
    std::string getText(const antlr4::misc::Interval &interval) override;
    std::string getSourceName() const override;
    std::string toString() const override;

  private:
    void InitializeInstanceFields();
    void load(std::ifstream &stream, std::string file_encoding);
  };
