#include "Fortran90LexerBase.h"

bool Fortran90LexerBase::IsColumnZero()
{
    return this->getCharPositionInLine() == 0;
}

bool Fortran90LexerBase::VerifyNotOperator()
{
    auto c1 = this->getInputStream()->LA(1);
    if (c1 == 'a')
    {
        auto c2 = this->getInputStream()->LA(2);
        if (c2 == 'n')
        {
            auto c3 = this->getInputStream()->LA(3);
            if (c3 == 'd')
            {
                auto c4 = this->getInputStream()->LA(4);
                if (c4 == '.')
                {
                    return false;
                }
            }
        }
    }
    else if (c1 == 'o')
    {
        auto c2 = this->getInputStream()->LA(2);
        if (c2 == 'r')
        {
            auto c3 = this->getInputStream()->LA(3);
            if (c3 == '.')
            {
                return false;
            }
        }
    }
    return true;
}
