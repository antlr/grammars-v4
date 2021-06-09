// Template generated code from Antlr4BuildTasks.dotnet-antlr <version>

#include \<string>
#include \<iostream>
#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "tree/ParseTree.h"
#include "tree/TerminalNode.h"
#include "tree/TerminalNodeImpl.h"
#include "misc/Interval.h"
#include "ConsoleErrorListener.h"
#include "CharStream.h"
#include "CaseChangingCharStream.h"
#include \<iostream>
#include \<string>
#include \<cctype>
#include \<cwctype>
#include \<stdexcept>

/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */
antlr4::runtime::CaseChangingCharStream::CaseChangingCharStream(antlr4::CharStream* stream, bool upper)
{
    this->stream = stream;
    this->upper = upper;
}

size_t antlr4::runtime::CaseChangingCharStream::index()
{
    return stream->index();
}

size_t antlr4::runtime::CaseChangingCharStream::size()
{
    return stream->size();
}

std::string antlr4::runtime::CaseChangingCharStream::getSourceName() const
{
    return stream->getSourceName();
}

void antlr4::runtime::CaseChangingCharStream::consume()
{
    stream->consume();
}

std::string antlr4::runtime::CaseChangingCharStream::getText(const antlr4::misc::Interval& interval)
{
    return stream->getText(interval);
}

size_t antlr4::runtime::CaseChangingCharStream::LA(ssize_t i)
{
    ssize_t c = stream->LA(i);
    if (c \<= 0)
    {
	    return c;
    }
    if (upper)
    {
	    return (ssize_t)toupper(c);
    }
    return (ssize_t)tolower(c);
}

ssize_t antlr4::runtime::CaseChangingCharStream::mark()
{
    return stream->mark();
}

void antlr4::runtime::CaseChangingCharStream::release(ssize_t marker)
{
    stream->release(marker);
}

void antlr4::runtime::CaseChangingCharStream::seek(size_t index)
{
    stream->seek(index);
}

std::string antlr4::runtime::CaseChangingCharStream::toString() const
{
	return stream->toString();
}
