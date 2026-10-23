
#pragma once

#include <string_view>
#include <memory>

#define YSTD_NOEXCEPT noexcept

namespace ystd
{
    //static_pointer_cast overload for std::shared_ptr
    using std::static_pointer_cast;

    //dynamic_pointer_cast overload for std::shared_ptr
    using std::dynamic_pointer_cast;

    //const_pointer_cast overload for std::shared_ptr
    using std::const_pointer_cast;

    //reinterpret_pointer_cast overload for std::shared_ptr
    template<class T, class U> std::shared_ptr<T> reinterpret_pointer_cast(const std::shared_ptr<U>& r) YSTD_NOEXCEPT
    {
        (void) reinterpret_cast<T*>(static_cast<U*>(0));

        typedef typename std::shared_ptr<T>::element_type E;

        E* p = reinterpret_cast<E*>(r.get());
        return std::shared_ptr<T>(r, p);
    }

    //static_pointer_cast overload for std::unique_ptr
    template<class T, class U> std::unique_ptr<T> static_pointer_cast(std::unique_ptr<U>&& r) YSTD_NOEXCEPT
    {
        (void) static_cast<T*>(static_cast<U*>(0));

        typedef typename std::unique_ptr<T>::element_type E;

        return std::unique_ptr<T>(static_cast<E*>(r.release()));
    }

    //dynamic_pointer_cast overload for std::unique_ptr
    template<class T, class U> std::unique_ptr<T> dynamic_pointer_cast(std::unique_ptr<U>&& r) YSTD_NOEXCEPT
    {
        (void) dynamic_cast<T*>(static_cast<U*>(0));

        //BOOST_STATIC_ASSERT_MSG(boost::has_virtual_destructor<T>::value, "The target of dynamic_pointer_cast must have a virtual destructor.");

        T* p = dynamic_cast<T*>(r.get());
        if (p) r.release();
        return std::unique_ptr<T>(p);
    }

    //const_pointer_cast overload for std::unique_ptr
    template<class T, class U> std::unique_ptr<T> const_pointer_cast(std::unique_ptr<U>&& r) YSTD_NOEXCEPT
    {
        (void) const_cast<T*>(static_cast<U*>(0));

        typedef typename std::unique_ptr<T>::element_type E;

        return std::unique_ptr<T>(const_cast<E*>(r.release()));
    }

    //reinterpret_pointer_cast overload for std::unique_ptr
    template<class T, class U> std::unique_ptr<T> reinterpret_pointer_cast(std::unique_ptr<U>&& r) YSTD_NOEXCEPT
    {
        (void) reinterpret_cast<T*>(static_cast<U*>(0));

        typedef typename std::unique_ptr<T>::element_type E;

        return std::unique_ptr<T>(reinterpret_cast<E*>(r.release()));
    }

    inline std::string& replace(std::string& string,
        const std::string_view& replaced_key,
        const std::string_view& replacing_key)
    {
        std::string::size_type pos = 0;
        while ((pos = string.find(replaced_key, pos)) != std::string::npos)
        {
            (void)string.replace(pos, replaced_key.length(), !replacing_key.empty() ? replacing_key.data() : "",
                replacing_key.length());
            pos += replacing_key.length();
        }
        return string;
    }

    template< class T >
    using decay_t = typename std::decay<T>::type;
    template< class T >
    using remove_const_t = typename std::remove_const<T>::type;
    namespace char_ranges
    { // allow get char type from char*, wchar_t*, std::string, std::wstring
        template <typename _Ty> struct value_type {
            using type = typename _Ty::value_type;
        };

        template <typename _Ty> struct value_type<_Ty&> {
            using type = remove_const_t<_Ty>;
        };

        template <typename _Ty> struct value_type<_Ty*> {
            using type = remove_const_t<_Ty>;
        };
    } // namespace char_ranges

    // starts_with(), since C++20:
    template <typename _CharT>
    inline bool starts_with(std::basic_string_view<_CharT> lhs,
        std::basic_string_view<_CharT> v) // (1)
    {
        return lhs.size() >= v.size() && lhs.compare(0, v.size(), v) == 0;
    }

    template <typename _T1, typename _T2>
    inline bool starts_with(_T1&& lhs, _T2&& v) // (2)
    {
        using char_type = typename char_ranges::value_type<decay_t<_T1>>::type;
        return starts_with(std::basic_string_view<char_type>{lhs}, std::basic_string_view<char_type>{v});
    }

    template <typename _CharT>
    inline bool starts_with(std::basic_string_view<_CharT> lhs, int c) // (3)
    {
        return !lhs.empty() && lhs.front() == c;
    }

    template <typename _Ty>
    inline bool starts_with(_Ty&& lhs, int c) // (4)
    {
        using char_type = typename char_ranges::value_type<decay_t<_Ty>>::type;
        return starts_with(std::basic_string_view<char_type>{lhs}, c);
    }

    // ends_with(), since C++20:
    template <typename _CharT>
    inline bool ends_with(std::basic_string_view<_CharT> lhs,
        std::basic_string_view<_CharT> v) // (1)
    {
        return lhs.size() >= v.size() && lhs.compare(lhs.size() - v.size(), lhs.npos, v) == 0;
    }

    template <typename _T1, typename _T2>
    inline bool ends_with(_T1&& lhs, _T2&& v) // (2)
    {
        using char_type = typename char_ranges::value_type<decay_t<_T1>>::type;
        return ends_with(std::basic_string_view<char_type>{lhs}, std::basic_string_view<char_type>{v});
    }

    template <typename _CharT>
    inline bool ends_with(std::basic_string_view<_CharT> lhs, int c) // (3)
    {
        return !lhs.empty() && lhs.back() == c;
    }

    template <typename _Ty> inline bool ends_with(_Ty&& lhs, int c) // (4)
    {
        using char_type = typename char_ranges::value_type<decay_t<_Ty>>::type;
        return ends_with(std::basic_string_view<char_type>{lhs}, c);
    }

    /// The case insensitive implementation of starts_with, ends_with
    namespace ic
    {
        template <typename _CharT>
        inline bool iequals(std::basic_string_view<_CharT> lhs, std::basic_string_view<_CharT> v);
#if defined(_MSC_VER)
        template <>
        inline bool iequals<char>(std::basic_string_view<char> lhs, std::basic_string_view<char> v)
        {
            return lhs.size() == v.size() && ::_strnicmp(lhs.data(), v.data(), v.size()) == 0;
        }
        template <>
        inline bool iequals<wchar_t>(std::basic_string_view<wchar_t> lhs,
            std::basic_string_view<wchar_t> v)
        {
            return lhs.size() == v.size() && ::_wcsnicmp(lhs.data(), v.data(), v.size()) == 0;
        }
#else
        template <>
        inline bool iequals<char>(std::basic_string_view<char> lhs, std::basic_string_view<char> v)
        {
            return lhs.size() == v.size() && ::strncasecmp(lhs.data(), v.data(), v.size()) == 0;
        }
        template <>
        inline bool iequals<wchar_t>(std::basic_string_view<wchar_t> lhs,
            std::basic_string_view<wchar_t> v)
        {
            return lhs.size() == v.size() && ::wcsncasecmp(lhs.data(), v.data(), v.size()) == 0;
        }
#endif
        template <typename _T1, typename _T2> inline bool iequals(_T1&& lhs, _T2&& v)
        {
            using char_type = typename char_ranges::value_type<decay_t<_T1>>::type;
            return iequals(std::basic_string_view<char_type>{lhs}, std::basic_string_view<char_type>{v});
        }
        // starts_with(), since C++20:
        template <typename _CharT>
        inline bool starts_with(std::basic_string_view<_CharT> lhs,
            std::basic_string_view<_CharT> v) // (1)
        {
            return lhs.size() >= v.size() && iequals(lhs.substr(0, v.size()), v);
        }

        template <typename _T1, typename _T2> inline bool starts_with(_T1&& lhs, _T2&& v) // (2)
        {
            using char_type = typename char_ranges::value_type<decay_t<_T1>>::type;
            return starts_with(std::basic_string_view<char_type>{lhs}, std::basic_string_view<char_type>{v});
        }

        template <typename _CharT>
        inline bool starts_with(std::basic_string_view<_CharT> lhs, int c) // (3)
        {
            return !lhs.empty() && ::tolower(lhs.front()) == ::tolower(c);
        }

        template <typename _Ty> inline bool starts_with(_Ty&& lhs, int c) // (4)
        {
            using char_type = typename char_ranges::value_type<decay_t<_Ty>>::type;
            return starts_with(std::basic_string_view<char_type>{lhs}, c);
        }

        // ends_with(), since C++20:
        template <typename _CharT>
        inline bool ends_with(std::basic_string_view<_CharT> lhs,
            std::basic_string_view<_CharT> v) // (1)
        {
            return lhs.size() >= v.size() && iequals(lhs.substr(lhs.size() - v.size(), lhs.npos), v);
        }

        template <typename _T1, typename _T2> inline bool ends_with(_T1&& lhs, _T2&& v) // (2)
        {
            using char_type = typename char_ranges::value_type<decay_t<_T1>>::type;
            return ends_with(std::basic_string_view<char_type>{lhs}, std::basic_string_view<char_type>{v});
        }

        template <typename _CharT>
        inline bool ends_with(std::basic_string_view<_CharT> lhs, int c) // (3)
        {
            return !lhs.empty() && ::tolower(lhs.back()) == ::tolower(c);
        }

        template <typename _Ty> inline bool ends_with(_Ty&& lhs, int c) // (4)
        {
            using char_type = typename char_ranges::value_type<decay_t<_Ty>>::type;
            return ends_with(std::basic_string_view<char_type>{lhs}, c);
        }
    } // namespace ic

    inline std::string& trim(std::string& s) {
        s.erase(0, s.find_first_not_of(" \n\r\t"));
        s.erase(s.find_last_not_of(" \n\r\t") + 1);
        return s;
    }
} // namespace ystd
