/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
  * This file is part of the LibreOffice project.
  *
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  *
  * This file incorporates work covered by the following license notice:
  *
  *   Licensed to the Apache Software Foundation (ASF) under one or more
  *   contributor license agreements. See the NOTICE file distributed
  *   with this work for additional information regarding copyright
  *   ownership. The ASF licenses this file to you under the Apache
  *   License, Version 2.0 (the "License"); you may not use this file
  *   except in compliance with the License. You may obtain a copy of
  *   the License at http://www.apache.org/licenses/LICENSE-2.0 .
  */
//#ifndef INCLUDED_CPPUHELPER_INTERFACECONTAINER_H
//#define INCLUDED_CPPUHELPER_INTERFACECONTAINER_H

//#include "sal/config.h"

//#include <cstddef>
//#include <functional>
//#include <vector>
//#include <utility>

//#include "osl/diagnose.h"
//#include "osl/mutex.hxx"
//#include "rtl/alloc.h"
//#include "com/sun/star/uno/Sequence.hxx"
//#include "com/sun/star/lang/EventObject.hpp"

//#include "com/sun/star/lang/DisposedException.hpp"
//#include "cppuhelper/cppuhelperdllapi.h"

namespace com
{
    namespace sun
    {
        namespace star
        {
            namespace uno
            {
                class XInterface;
            }
        } // namespace star
    }     // namespace sun
} // namespace com
  //for docpp
namespace cppu
{

    namespace detail
    {

        union element_alias
        {
            css::uno::Sequence<css::uno::Reference<css::uno::XInterface>> *pAsSequence;
            css::uno::XInterface *pAsInterface;
            element_alias() : pAsInterface(NULL) {}
        };

    } // namespace detail

    class OInterfaceContainerHelper;
    class /**/ OInterfaceIteratorHelper
    {
    public:
        OInterfaceIteratorHelper(OInterfaceContainerHelper &rCont);

        ~OInterfaceIteratorHelper();

        bool hasMoreElements() const
        {
            return nRemain != 0;
        }
        css::uno::XInterface *next();

        void remove();

    private:
        OInterfaceContainerHelper &rCont;
        sal_Bool bIsList;

        detail::element_alias aData;

        sal_Int32 nRemain;

        OInterfaceIteratorHelper(const OInterfaceIteratorHelper &);
        OInterfaceIteratorHelper &operator=(const OInterfaceIteratorHelper &);
    };

    class OInterfaceContainerHelper
    {
    public:
        // these are here to force memory de/allocation to sal lib.
        static void *operator new(size_t nSize)
        {
            return ::rtl_allocateMemory(nSize);
        }
        static void operator delete(void *pMem)
        {
            ::rtl_freeMemory(pMem);
        }
        static void *operator new(size_t, void *pMem)
        {
            return pMem;
        }
        static void operator delete(void *, void *)
        {
        }

        OInterfaceContainerHelper(::osl::Mutex &rMutex);
        ~OInterfaceContainerHelper();
        sal_Int32 getLength() const;

        css::uno::Sequence<css::uno::Reference<css::uno::XInterface>> getElements() const;

        sal_Int32 addInterface(const css::uno::Reference<css::uno::XInterface> &rxIFace);
        sal_Int32 removeInterface(const css::uno::Reference<css::uno::XInterface> &rxIFace);
        void disposeAndClear(const css::lang::EventObject &rEvt);
        void clear();

        template <typename ListenerT, typename FuncT>
        inline void forEach(FuncT const &func);

        template <typename ListenerT, typename EventT>
        inline void notifyEach(void (ListenerT::*NotificationMethod)(const EventT &), const EventT &Event);

    private:
        friend class OInterfaceIteratorHelper;
        detail::element_alias aData;
        ::osl::Mutex &rMutex;
        sal_Bool bInUse;
        sal_Bool bIsList;

        OInterfaceContainerHelper(const OInterfaceContainerHelper &);
        OInterfaceContainerHelper &operator=(const OInterfaceContainerHelper &);

        /*
       Duplicate content of the container and release the old one without destroying.
       The mutex must be locked and the memberbInUse must be true.
      */
        void copyAndResetInUse();

    private:
        template <typename ListenerT, typename EventT>
        class NotifySingleListener
        {
        private:
            typedef void (ListenerT::*NotificationMethod)(const EventT &);
            NotificationMethod m_pMethod;
            const EventT &m_rEvent;

        public:
            NotifySingleListener(NotificationMethod method, const EventT &event) : m_pMethod(method), m_rEvent(event) {}

            void operator()(const css::uno::Reference<ListenerT> &listener) const
            {
                (listener.get()->*m_pMethod)(m_rEvent);
            }
        };
    };

    template <typename ListenerT, typename FuncT>
    inline void OInterfaceContainerHelper::forEach(FuncT const &func)
    {
        OInterfaceIteratorHelper iter(*this);
        while (iter.hasMoreElements())
        {
            css::uno::Reference<ListenerT> const xListener(iter.next(), css::uno::UNO_QUERY);
            if (xListener.is())
            {
                try
                {
                    func(xListener);
                }
                catch (css::lang::DisposedException const &exc)
                {
                    if (exc.Context == xListener)
                        iter.remove();
                }
            }
        }
    }

    template <typename ListenerT, typename EventT>
    inline void OInterfaceContainerHelper::notifyEach(void (ListenerT::*NotificationMethod)(const EventT &), const EventT &Event)
    {
        forEach<ListenerT, NotifySingleListener<ListenerT, EventT>>(NotifySingleListener<ListenerT, EventT>(NotificationMethod, Event));
    }

    template <class key, class hashImpl = void, class equalImpl = std::equal_to<key>>
    class OMultiTypeInterfaceContainerHelperVar
    {
    public:
        // these are here to force memory de/allocation to sal lib.
        static void *operator new(size_t nSize)
        {
            return ::rtl_allocateMemory(nSize);
        }
        static void operator delete(void *pMem)
        {
            ::rtl_freeMemory(pMem);
        }
        static void *operator new(size_t, void *pMem)
        {
            return pMem;
        }
        static void operator delete(void *, void *)
        {
        }

        inline OMultiTypeInterfaceContainerHelperVar(::osl::Mutex &rMutex);
        inline ~OMultiTypeInterfaceContainerHelperVar();

        inline css::uno::Sequence<key> getContainedTypes() const;

        inline OInterfaceContainerHelper *getContainer(const key &) const;

        inline sal_Int32 addInterface(
            const key &rKey,
            const css::uno::Reference<css::uno::XInterface> &r);

        inline sal_Int32 removeInterface(
            const key &rKey,
            const css::uno::Reference<css::uno::XInterface> &rxIFace);

        inline void disposeAndClear(const css::lang::EventObject &rEvt);
        inline void clear();

        typedef key keyType;

    private:
        typedef ::std::vector<std::pair<key, void *>> InterfaceMap;
        InterfaceMap *m_pMap;
        ::osl::Mutex &rMutex;

        typename InterfaceMap::iterator find(const key &rKey) const
        {
            typename InterfaceMap::iterator iter = m_pMap->begin();
            typename InterfaceMap::iterator end = m_pMap->end();

            while (iter != end)
            {
                equalImpl equal;
                if (equal(iter->first, rKey))
                    break;
                ++iter;
            }
            return iter;
        }

        OMultiTypeInterfaceContainerHelperVar(const OMultiTypeInterfaceContainerHelperVar &);
        OMultiTypeInterfaceContainerHelperVar &operator=(const OMultiTypeInterfaceContainerHelperVar &);
    };

    template <class container, class keyType>
    struct OBroadcastHelperVar
    {
        ::osl::Mutex &rMutex;
        container aLC;
        sal_Bool bDisposed;
        sal_Bool bInDispose;

        OBroadcastHelperVar(::osl::Mutex &rMutex_)
            : rMutex(rMutex_), aLC(rMutex_), bDisposed(false), bInDispose(false)
        {
        }

        void addListener(
            const keyType &key,
            const css::uno::Reference<css::uno::XInterface> &r)
        {
            ::osl::MutexGuard guard(rMutex);
            OSL_ENSURE(!bInDispose, "do not add listeners in the dispose call");
            OSL_ENSURE(!bDisposed, "object is disposed");
            if (!bInDispose && !bDisposed)
                aLC.addInterface(key, r);
        }

        void removeListener(
            const keyType &key,
            const css::uno::Reference<css::uno::XInterface> &r)
        {
            ::osl::MutexGuard guard(rMutex);
            if (!bInDispose && !bDisposed)
                aLC.removeInterface(key, r);
        }

        OInterfaceContainerHelper *getContainer(const keyType &key) const
        {
            return aLC.getContainer(key);
        }
    };

    /*------------------------------------------
 *
 * In general, the above templates are used with a Type as key.
 * Therefore a default declaration is given ( OMultiTypeInterfaceContainerHelper and OBroadcastHelper )
 *
 *------------------------------------------*/

    // helper function call class
    struct hashType_Impl
    {
        size_t operator()(const css::uno::Type &s) const
        {
            return static_cast<size_t>(s.getTypeName().hashCode());
        }
    };

    class OMultiTypeInterfaceContainerHelper
    {
    public:
        // these are here to force memory de/allocation to sal lib.
        static void *operator new(size_t nSize)
        {
            return ::rtl_allocateMemory(nSize);
        }
        static void operator delete(void *pMem)
        {
            ::rtl_freeMemory(pMem);
        }
        static void *operator new(size_t, void *pMem)
        {
            return pMem;
        }
        static void operator delete(void *, void *)
        {
        }

        OMultiTypeInterfaceContainerHelper(::osl::Mutex &rMutex);
        ~OMultiTypeInterfaceContainerHelper();

        css::uno::Sequence<css::uno::Type> getContainedTypes() const;

        OInterfaceContainerHelper *getContainer(const css::uno::Type &rKey) const;

        sal_Int32 addInterface(
            const css::uno::Type &rKey,
            const css::uno::Reference<css::uno::XInterface> &r);

        sal_Int32 removeInterface(
            const css::uno::Type &rKey,
            const css::uno::Reference<css::uno::XInterface> &rxIFace);

        void disposeAndClear(const css::lang::EventObject &rEvt);
        void clear();

        typedef css::uno::Type keyType;

    private:
        void *m_pMap;
        ::osl::Mutex &rMutex;

        OMultiTypeInterfaceContainerHelper(const OMultiTypeInterfaceContainerHelper &);
        OMultiTypeInterfaceContainerHelper &operator=(const OMultiTypeInterfaceContainerHelper &);
    };

    typedef OBroadcastHelperVar<OMultiTypeInterfaceContainerHelper, OMultiTypeInterfaceContainerHelper::keyType> OBroadcastHelper;

} // namespace cppu

#endif

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */